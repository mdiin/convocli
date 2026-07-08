(ns convocli.core
  (:require
   [clojure.string :as str]
   [clojure.edn :as edn]
   [cheshire.core :as json]
   [sci.core :as sci]
   [charm.message :as msg]
   [charm.program :as program]
   [charm.style.border :as border]
   [charm.style.core :as style]
   [charm.ansi.width :as ansi]
   [charm.components.text-input :as text-input]
   [charm.components.list :as item-list]
   [charm.components.spinner :as spinner]
   [charm.components.viewport :as viewport]
   [babashka.http-client :as http]))

;; ToolConfig loading (see convocli.allium): each entry supplies an
;; OpenAI-style function tool definition plus a `mapper` - user-authored
;; SCI/Clojure source, evaluated once, that turns the raw JSON arguments
;; string the LLM supplied into a shell command string.

(def tool-configs (edn/read-string (slurp "tools.edn")))

(def tool-configs-by-name (into {} (map (juxt :name identity) tool-configs)))

(defn tool-config->openai-tool
  [{:keys [name description parameters]}]
  {:type "function"
   :function {:name name
              :description description
              :parameters parameters}})

(def tools (mapv tool-config->openai-tool tool-configs))

;; Mapper source only ever sees json-parse/json-encode - no filesystem,
;; network or process access - so a broken or malicious mapper can produce
;; a bad command string but can't do anything else.
(def mapper-sci-ctx
  (sci/init {:bindings {'json-parse #(json/parse-string % true)
                         'json-encode json/encode}}))

(defn apply-mapper
  "Evaluates tool-config's mapper source and applies it to the raw
  arguments JSON string, returning the shell command it produces."
  [tool-config arguments-json]
  (let [mapper-fn (sci/eval-string* mapper-sci-ctx (:mapper tool-config))]
    (mapper-fn arguments-json)))

(def my-input (text-input/text-input :prompt ":> "
                                     :placeholder ""))

(defn conversation-log->messages
  [{:keys [type] :as clog}]
  (condp = type
    :llm-response (mapv :message (:value clog))
    :user-message [(:value clog)]
    :tool-call [(:value clog)]))

(defn call-llm-cmd
  [q conversation-log]
  (let [msg {:role "user"
             :content q}
        user-cmd (program/cmd (fn []
                                {:type :user-message
                                 :unixtime (System/currentTimeMillis)
                                 :value msg}))
        llm-cmd (program/cmd (fn []
                       (let [response (http/post "http://localhost:9090/v1/chat/completions"
                                                 {:headers {:content-type "application/json"}
                                                  :body (json/encode {:model "ministral-3-3b"
                                                                      :tools tools
                                                                      :messages (into [{:role "system"
                                                                                        :content "You know Event Modeling (Adam Dymitruk). Your goal is to command the Event Modeling CLI. Whenever you have enough knowledge to invoke a tool, do so."}
                                                                                       ]
                                                                                      (conj (vec (flatten (mapv conversation-log->messages conversation-log))) msg))
                                                                      :prompt_logprobs 0
                                                                      :cache_prompt true})})
                             foo (json/parse-string (:body response) true)]
                         {:type :llm-response
                          :unixtime (or (:created foo) (System/currentTimeMillis))
                          :value (:choices foo)})))]
    (program/sequence-cmds user-cmd llm-cmd)))

(defn clear-input-cmd
  []
  (program/cmd (fn []
                 {:type :clear-query})))

(defn set-waiting-cmd
  []
  (program/cmd (fn []
                 {:type :awaiting-llm})))

(defn init []
  (let [[s s-cmd] (spinner/spinner-init (spinner/spinner :dots))
        [vp vp-cmd] (viewport/viewport-init (viewport/viewport ""))]
    [{:llm-query my-input
      :spinner s
      :viewport vp
      :current :llm-query
      :conversation-log []
      :processing? false
      :terminal-width 120
      :terminal-height 80}
     (program/batch s-cmd vp-cmd)]))

(defn parse-function
  [{:keys [type] :as tool-call}]
  (when-let [function (:function tool-call)]
    (let [{:keys [name arguments]} function
          tool-config (get tool-configs-by-name name)
          content (if (nil? tool-config)
                    (str "unknown tool: " name)
                    (try
                      (apply-mapper tool-config arguments)
                      (catch Exception e
                        (str "mapper failed for " name ": " (ex-message e)))))]
      {:content content
       :role "tool"
       :tool_call_id (:id tool-call)})))

(defn tool-calls-cmd
  "Converts a seq of tool_call objects to a vector of charm commands."
  [tool-calls]
  (let [commands (mapv (fn [tool-call]
                         (program/cmd (fn []
                                        {:type :tool-call
                                         :value (parse-function tool-call)})))
                       tool-calls)]
    commands))

(defn update-fn [state msg]
  (cond
    (msg/window-size? msg)
    [(assoc state
            :terminal-width (:width msg)
            :terminal-height (:height msg)
            :viewport (viewport/viewport-set-dimensions (:viewport state) (- (:width msg) 4) (- (:height msg) 12)))
     nil]

    (msg/key-match? msg "ctrl+c")
    [state program/quit-cmd]

    (msg/key-match? msg "enter")
    (let [clear-cmd (clear-input-cmd)
          llm-cmd (call-llm-cmd (text-input/value (get state :llm-query)) (:conversation-log state))
          waiting-cmd (set-waiting-cmd)]
      [state (program/batch waiting-cmd clear-cmd llm-cmd)])

    (spinner/spinning? (:spinner state) msg)
    (let [[s cmd] (spinner/spinner-update (:spinner state) msg)]
      [(assoc state :spinner s) cmd])

    (= :awaiting-llm (:type msg))
    [(assoc state :processing? true) nil]

    (= :clear-query (:type msg))
    (let [input (text-input/reset (get state :llm-query))]
      [(assoc state :llm-query input) nil])

    (= :user-message (:type msg))
    [(update state :conversation-log conj msg) nil]

    (= :llm-response (:type msg))
    (let [tool-calls (mapcat (comp tool-calls-cmd :tool_calls :message) (:value msg))]
      [(-> state
           (update :conversation-log conj msg)
           (assoc :processing? false))
       (apply program/sequence-cmds tool-calls)])

    (= :tool-call (:type msg))
    [(update state :conversation-log conj msg)
     nil]

    :else
    (let [field (:current state)
          [input cmd] (text-input/text-input-update (get state field) msg)
          [vp vp-cmd] (viewport/viewport-update (get state :viewport) msg)]
      [(-> state
           (assoc field input)
           (assoc :viewport vp))
       (program/batch cmd vp-cmd)])))

(defn render-header
  []
  "ConvoCLI")

(defn wrap-text [text max-width]
  (let [{:keys [lines current]} (reduce (fn [{:keys [current lines] :as acc} word]
                                          (let [new-current (if (empty? current)
                                                              word
                                                              (str current " " word))]
                                            (if (> (ansi/string-width new-current) max-width)
                                              (assoc acc
                                                     :current word
                                                     :lines (conj lines current))
                                              (assoc acc
                                                     :current new-current
                                                     :lines lines))))

                                        {:current ""
                                         :lines []
                                         }
                                        (str/split text #" "))]
    (conj lines current)))

(defn wrap-with-prefix
  [conversation-log {:keys [user-prefix llm-prefix tool-prefix max-width]}]
  (into []
        (comp
         (map (fn [r]
                (let [messages (conversation-log->messages r)
                      prefix (condp = (:type r)
                               :user-message user-prefix
                               :llm-response llm-prefix
                               tool-prefix)]
                  (map (fn [msg]
                            [r (wrap-text (str (:content msg)) (- max-width (ansi/string-width (str prefix " => ")) 10))])
                          messages))))
         (map (fn [[[r [text & texts]]]]
                (let [prefix (condp = (:type r)
                               :user-message user-prefix
                               :llm-response llm-prefix
                               tool-prefix)]
                  (into [(str prefix " => " text)] (mapv #(str (str/join (repeat (ansi/string-width (str prefix " => ")) " ")) %) texts))))))
        conversation-log))

(defn render-content
  [{:keys [processing? conversation-log spinner viewport terminal-width]}]
  (style/render
   (style/style)

   (viewport/viewport-view
    (viewport/viewport-set-content viewport (str
                                             (str/join "\n\n"
                                                       (map (fn [ws] (str/join "\n" ws))
                                                            (wrap-with-prefix conversation-log {:llm-prefix "[*_*]"
                                                                                                :user-prefix "<O_O>"
                                                                                                :tool-prefix ">=>"
                                                                                         :max-width terminal-width})))
                                             (when processing?
                                               (str
                                                (when (seq conversation-log) "\n\n")
                                                "[-_o] => "
                                                (spinner/spinner-view spinner))))))


   ))

(defn render-footer
  [query-input]
  (text-input/text-input-view query-input))

(defn render-page
  [header content footer & {:keys [width]}]
  (let [w (max 60 (- width 4))]
    (style/join-vertical
     :left
     (style/render
      (style/style :width w :align :center :border border/normal)
      header)
     (style/render
      (style/style :width w :align :left :border border/rounded)
      content)
     (style/render
      (style/style :width w :align :left :border border/normal)
      footer))))

(defn view-2 [state]
  (let [{:keys [terminal-width]} state]
    (render-page
     (render-header)
     (render-content state)
     (render-footer (:llm-query state))
     :width terminal-width)))

(defn -main [& _args]
  (program/run {:init init
                      :update #'update-fn
                      :view #'view-2
                      :alt-screen true}))
