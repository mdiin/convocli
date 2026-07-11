(ns convocli.core
  (:require
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [sci.core :as sci]
   [babashka.process :as process]
   [charm.message :as msg]
   [charm.program :as program]
   [charm.style.border :as border]
   [charm.style.core :as style]
   [charm.ansi.width :as ansi]
   [charm.components.text-input :as text-input]
   [charm.components.spinner :as spinner]
   [charm.components.viewport :as viewport]
   [babashka.http-client :as http]))

(declare sync-viewport-content)

;; ---------------------------------------------------------------------------
;; Config file lookup: .convocli/<filename> under the current directory
;; takes precedence, falling back to <XDG config dir>/convocli/<filename>
;; so config.edn/tools.edn can live either alongside a project or once
;; per-user.
;; ---------------------------------------------------------------------------

(defn- xdg-config-home []
  (or (System/getenv "XDG_CONFIG_HOME")
      (str (System/getProperty "user.home") "/.config")))

(defn find-config-file [filename]
  (let [local (io/file ".convocli" filename)
        xdg (io/file (xdg-config-home) "convocli" filename)]
    (cond
      (.exists local) local
      (.exists xdg) xdg
      :else nil)))

;; If neither location has the file, seed one under ./.convocli/ (the
;; directory convocli was invoked from) from the copy bundled on the
;; classpath, so a first run has something to edit instead of a crash.
(defn- install-default-file! [filename resource-name]
  (let [dest (io/file ".convocli" filename)]
    (io/make-parents dest)
    (io/copy (io/reader (io/resource resource-name)) dest)
    dest))

(defn ensure-config-file! [filename resource-name]
  (or (find-config-file filename)
      (install-default-file! filename resource-name)))

;; ---------------------------------------------------------------------------
;; Config (see convocli.allium's `config` block)
;; ---------------------------------------------------------------------------

(def config
  (edn/read-string (slurp (ensure-config-file! "config.edn" "config.edn.example"))))

;; llm-endpoint/llm-model are mandatory per convocli.allium's config block
;; (no default there) - fail loudly at startup rather than silently
;; running against an endpoint/model nobody chose. The auto-installed
;; default config.edn (see ensure-config-file! above) already sets both,
;; so this only fires if a user-edited config.edn drops one of them.
(defn validate-config! []
  (doseq [k [:llm-endpoint :llm-model]]
    (when (nil? (get config k))
      (throw (ex-info (str "config.edn must set " k) {:missing-key k})))))

;; advertised_context_window is a black box in the spec. llama.cpp/vLLM-
;; style servers expose the model's real n_ctx via GET /v1/models, so this
;; queries that instead of guessing. Falls back to 8192 if the endpoint
;; doesn't support it. Fetched once at startup since :llm-model is fixed
;; for the life of the process; set :context-window-override in
;; config.edn to bypass this entirely.
(defn fetch-advertised-context-window [endpoint model]
  (try
    (let [models-url (str/replace endpoint #"/chat/completions$" "/models")
          body (json/parse-string (:body (http/get models-url)) true)
          entry (first (filter #(= model (:id %)) (:data body)))]
      (or (get-in entry [:meta :n_ctx]) 8192))
    (catch Exception _ 8192)))

(def advertised-context-window-tokens
  (delay (fetch-advertised-context-window (:llm-endpoint config) (:llm-model config))))

(defn context-window-limit []
  (or (:context-window-override config) @advertised-context-window-tokens))

;; ---------------------------------------------------------------------------
;; ToolConfig loading (see convocli.allium): each entry supplies an
;; OpenAI-style function tool definition plus a mapper - user-authored
;; Clojure code, stored as a real form (not an escaped string) so it's
;; directly editable in tools.edn, evaluated via sci once per call to
;; turn the raw JSON arguments string the LLM supplied into a shell
;; command string.
;; ---------------------------------------------------------------------------

(def tool-configs
  (edn/read-string (slurp (ensure-config-file! "tools.edn" "tools.edn"))))

(def tool-configs-by-name (into {} (map (juxt :name identity) tool-configs)))

(defn tool-config->openai-tool
  [{:keys [name description parameters-schema]}]
  {:type "function"
   :function {:name name
              :description description
              :parameters parameters-schema}})

(def tools (mapv tool-config->openai-tool tool-configs))

(defn shell-quote
  "Wraps a value in single quotes, escaping any embedded single quote as
  '\\'' - the standard POSIX-safe way to make a value immune to shell
  interpretation regardless of its content (spaces, quotes, $, ;, `, etc).
  The mapper's job is to produce a shell command string that later gets
  run via `bash -c`; every value embedded in it that could contain
  arbitrary text (any LLM-supplied argument) must go through this."
  [v]
  (str "'" (str/replace (str v) "'" "'\\''") "'"))

;; Mapper source can call json-parse/json-encode/shell-quote - no direct
;; filesystem/network/process access from within the sandbox itself, but
;; its OUTPUT is a shell command string that DOES get executed with full
;; shell access (see execute-tool-call-cmd), so an unescaped value in
;; that string is a real command-injection risk, not just a "bad string"
;; - shell-quote exists specifically so mapper authors don't have to
;; hand-roll escaping to stay safe.
;; :bindings is deprecated (sci.core/eval-string's docstring: ":bindings x
;; is the same as :namespaces {'user x}"); using the explicit replacement
;; directly rather than relying on deprecated behaviour that may vary
;; across sci/babashka versions.
(def mapper-sci-ctx
  (sci/init {:namespaces {'user {'json-parse #(json/parse-string % true)
                                 'json-encode json/encode
                                 'shell-quote shell-quote}}}))

;; sci/eval-form (like eval-string*) evaluates in whatever namespace *ns*
;; (the host Clojure/babashka dynamic var, not something sci-internal)
;; currently names - it does NOT default to 'user regardless of context.
;; Under `bb -e`/`bb -m` that happens to be 'user, so this worked in
;; every isolated test; under `bb run`/bb tasks, babashka rebinds *ns*
;; per invocation to a freshly generated `user-<uuid>` namespace, so the
;; bindings above (placed under the namespace literally named 'user)
;; were never actually visible - producing "Unable to resolve symbol:
;; json-parse" on every real run despite passing every isolated test.
;; Force the namespace explicitly rather than depend on ambient *ns*.
(def mapper-eval-ns (create-ns 'user))

(defn apply-mapper
  "Evaluates tool-config's mapper form and applies it to the raw
  arguments JSON string, returning the shell command it produces.
  mapper-source is a real EDN form (e.g. `(fn [args-json] ...)`), not a
  string - tools.edn holds it as actual, directly-editable Clojure code
  rather than one long escaped string."
  [tool-config arguments-json]
  (let [mapper-fn (binding [*ns* mapper-eval-ns]
                    (sci/eval-form mapper-sci-ctx (:mapper-source tool-config)))]
    (mapper-fn arguments-json)))

;; ---------------------------------------------------------------------------
;; Conversation persistence (see convocli.allium: Conversation always
;; resumes on launch). completion_pending is intentionally not persisted:
;; a stale "awaiting" flag with no request actually in flight would lock
;; the input forever after a crash/restart.
;; ---------------------------------------------------------------------------

(def conversation-file (io/file ".convocli" "conversation.edn"))

(defn recover-stuck-approvals
  "approved is transient (execution follows immediately); finding one on
  load means convocli crashed between approval and the result arriving,
  so the command's outcome is unknown. Reset to pending_approval rather
  than leave it stuck forever with no recovery path - see convocli.allium's
  ToolCall.status comment."
  [log]
  (mapv (fn [m]
          (if (= :assistant-response (:type m))
            (update m :tool-calls
                    (fn [calls] (mapv (fn [c] (if (= :approved (:status c)) (assoc c :status :pending-approval) c)) calls)))
            m))
        log))

(defn load-conversation []
  (if (.exists (io/file conversation-file))
    (update (edn/read-string (slurp conversation-file)) :conversation-log recover-stuck-approvals)
    {:conversation-log [] :approval-mode :manual}))

(defn save-conversation! [state]
  (io/make-parents conversation-file)
  (spit conversation-file
        (pr-str (select-keys state [:conversation-log :approval-mode])))
  state)

;; ---------------------------------------------------------------------------
;; Domain helpers (mirrors convocli.allium's derived values/rules)
;; ---------------------------------------------------------------------------

(def terminal-statuses #{:executed :rejected :interrupted})

(defn visible-messages
  "Mirrors Conversation.visible_messages: everything from the latest
  compaction-summary onward (inclusive), or the whole log if none exists."
  [log]
  (if-let [idx (last (keep-indexed (fn [i m] (when (= :compaction-summary (:type m)) i)) log))]
    (subvec log idx)
    log))

(defn batch? [response]
  (seq (:tool-calls response)))

(defn all-calls-resolved? [response]
  (and (batch? response)
       (every? #(contains? terminal-statuses (:status %)) (:tool-calls response))))

(defn response-settled?
  "True when a response has no calls awaiting approval or execution -
  either it proposed none, or every proposed call has reached a terminal
  status. Compaction must only trigger on a settled response (see
  handle-completion-received): visible-messages cuts the log by index at
  whatever moment the summary happens to land, so summarizing while a
  batch is still open risks the cut landing between the batch and its
  eventual resolution, silently dropping it from context/display."
  [response]
  (or (not (batch? response)) (all-calls-resolved? response)))

(defn latest-assistant-response-idx [log]
  (last (keep-indexed (fn [i m] (when (= :assistant-response (:type m)) i)) log)))

(defn latest-assistant-response [log]
  (when-let [idx (latest-assistant-response-idx log)]
    (get log idx)))

(defn has-unresolved-batch? [log]
  (boolean (when-let [r (latest-assistant-response log)]
             (and (batch? r) (not (all-calls-resolved? r))))))

(defn next-pending-call [response]
  (first (sort-by :sequence (filter #(= :pending-approval (:status %)) (:tool-calls response)))))

(defn tui-mode
  "Mirrors tui.allium's TuiSession.mode: a pure function of state, never
  stored."
  [state]
  (cond
    (has-unresolved-batch? (:conversation-log state)) :reviewing-batch
    (:summarization-pending? state) :summarizing
    (:completion-pending? state) :awaiting-completion
    :else :composing))

(defn update-latest-response [log f]
  (if-let [idx (latest-assistant-response-idx log)]
    (update log idx f)
    log))

(defn update-tool-call [response call-id f]
  (update response :tool-calls
          (fn [calls] (mapv (fn [c] (if (= (:id c) call-id) (f c) c)) calls))))

;; ---------------------------------------------------------------------------
;; Building the OpenAI-compatible request from conversation-log
;; ---------------------------------------------------------------------------

(defn tool-result-content
  [tool-call]
  (case (:status tool-call)
    :executed (str "exit " (:exit-code tool-call)
                   (when (seq (:stdout tool-call)) (str "\n" (:stdout tool-call)))
                   (when (seq (:stderr tool-call)) (str "\nSTDERR:\n" (:stderr tool-call))))
    :rejected "Rejected by the operator; not executed."
    :interrupted "Skipped: the operator interrupted auto-run before this call ran."
    nil))

(defn message->openai
  [m]
  (case (:type m)
    :user-prompt [{:role "user" :content (:text m)}]

    :assistant-response
    (into [(cond-> {:role "assistant"}
             (:text m) (assoc :content (:text m))
             (batch? m) (assoc :tool_calls
                                (mapv (fn [tc] {:id (:id tc)
                                                :type "function"
                                                :function {:name (:tool-name tc) :arguments (:arguments tc)}})
                                      (:tool-calls m))))]
          (keep (fn [tc]
                  (when-let [content (tool-result-content tc)]
                    {:role "tool" :tool_call_id (:id tc) :content content}))
                (:tool-calls m)))

    :compaction-summary [{:role "user" :content (str "Earlier conversation summary: " (:summary m))}]

    ;; llm-error / auto-cap-reached / auto-run-interrupted are client-side
    ;; bookkeeping only; the LLM never sees them directly.
    []))

(defn conversation->openai-messages [log]
  (vec (mapcat message->openai log)))

;; ---------------------------------------------------------------------------
;; Commands (async work; see charm.program/cmd)
;; ---------------------------------------------------------------------------

(defn batch-cmds
  "Combines optional cmds, staying nil (rather than an always-truthy,
  possibly-empty program/batch) when none are present - callers that
  branch on whether a cmd was returned (e.g. tests) need that distinction."
  [& cmds]
  (let [cmds (remove nil? cmds)]
    (case (count cmds)
      0 nil
      1 (first cmds)
      (apply program/batch cmds))))

(defn request-completion-cmd
  [conversation-log iteration]
  (program/cmd
   (fn []
     (try
       (let [response (http/post (:llm-endpoint config)
                                  {:headers {:content-type "application/json"}
                                   :body (json/encode
                                          {:model (:llm-model config)
                                           :tools tools
                                           :messages (into [{:role "system" :content (:system-prompt config)}]
                                                            (conversation->openai-messages conversation-log))})})
             body (json/parse-string (:body response) true)
             choice (:message (first (:choices body)))]
         {:type :completion-received
          :iteration iteration
          :text (:content choice)
          :tool-call-proposals (mapv (fn [tc] {:id (:id tc)
                                                :tool-name (get-in tc [:function :name])
                                                :arguments (get-in tc [:function :arguments])})
                                      (:tool_calls choice))
          :total-tokens (get-in body [:usage :total_tokens] 0)})
       (catch Exception e
         {:type :completion-failed :iteration iteration :error (ex-message e)})))))

(defn execute-tool-call-cmd
  [call]
  (program/cmd
   (fn []
     (let [{:keys [out err exit]} (process/shell {:out :string :err :string :continue true}
                                                  "bash" "-c" (:command call))]
       {:type :tool-call-executed :call-id (:id call) :stdout out :stderr err :exit-code exit}))))

;; ---------------------------------------------------------------------------
;; Proposal -> ToolCall (applies the SCI mapper; see RequestCompletion rule)
;; ---------------------------------------------------------------------------

(defn proposal->tool-call
  [sequence {:keys [id tool-name arguments]}]
  (let [tool-config (get tool-configs-by-name tool-name)]
    (if (nil? tool-config)
      {:id id :sequence sequence :tool-name tool-name :arguments arguments
       :command nil :status :executed :stdout nil :stderr (str "unknown tool: " tool-name)
       :exit-code nil :executed-at (System/currentTimeMillis)}
      (try
        {:id id :sequence sequence :tool-name tool-name :arguments arguments
         :command (apply-mapper tool-config arguments) :status :pending-approval}
        (catch Exception e
          {:id id :sequence sequence :tool-name tool-name :arguments arguments
           :command nil :status :executed :stdout nil
           :stderr (str "mapper failed for " tool-name ": " (ex-message e))
           :exit-code nil :executed-at (System/currentTimeMillis)})))))

;; ---------------------------------------------------------------------------
;; Compaction (see TriggerCompaction/ReceiveSummarization rules)
;; ---------------------------------------------------------------------------

;; Sent as a final "user" message (not a second "system" message - see
;; message->openai's :compaction-summary case for why strict chat
;; templates reject a system role anywhere past index 0). The point of
;; this summary is continuation, not narration: the same LLM has to keep
;; driving this Event Modeling session afterward on a fraction of the
;; tokens, so it must preserve model state/decisions/next-steps and
;; nothing else.
(def summarization-instruction
  "Compact the conversation above into a dense summary so you (the same
  assistant) can keep working on this Event Modeling session afterward
  having spent as few tokens as possible re-reading history. Preserve:
  the current state of the event model (events, commands, views,
  aggregates, slices), concrete decisions already made, files/tools
  touched and their outcomes, and any outstanding next steps. Drop
  pleasantries, narration and anything not needed to continue. Output
  only the summary - no preamble, no headings - as tersely as accuracy
  allows.")

(defn needs-compaction? [response]
  (>= (:total-tokens response 0) (context-window-limit)))

(defn summarize-conversation-cmd
  [conversation-log]
  (program/cmd
   (fn []
     (try
       (let [response (http/post (:llm-endpoint config)
                                  {:headers {:content-type "application/json"}
                                   :body (json/encode
                                          {:model (:llm-model config)
                                           :messages (into [{:role "system" :content (:system-prompt config)}]
                                                            (conj (conversation->openai-messages conversation-log)
                                                                  {:role "user" :content summarization-instruction}))})})
             body (json/parse-string (:body response) true)
             choice (:message (first (:choices body)))]
         {:type :summarization-received :summary (:content choice)})
       (catch Exception e
         {:type :summarization-failed :error (ex-message e)})))))

;; ---------------------------------------------------------------------------
;; Batch resolution (see BatchResolved rule)
;; ---------------------------------------------------------------------------

(defn maybe-resolve-batch
  "If the latest AssistantResponse's batch just became fully resolved,
  either records why auto-run stopped, or requests the next completion.
  Returns [state cmd]."
  [state]
  (let [log (:conversation-log state)
        idx (latest-assistant-response-idx log)
        response (when idx (get log idx))]
    (if (and response (all-calls-resolved? response))
      (cond
        (:interrupted? response)
        [(update state :conversation-log conj
                 {:type :auto-run-interrupted :created-at (System/currentTimeMillis)
                  :calls-skipped (count (filter #(= :interrupted (:status %)) (:tool-calls response)))})
         nil]

        (and (= :auto-run (:batch-approval-mode response))
             (>= (:batch-iteration response) (:max-auto-iterations config)))
        [(update state :conversation-log conj
                 {:type :auto-cap-reached :created-at (System/currentTimeMillis)
                  :iterations-run (:batch-iteration response)})
         nil]

        ;; A compaction (triggered by this same response settling, or by
        ;; an earlier one still in flight) must finish before any more
        ;; log-mutating cmd fires - otherwise the next completion could
        ;; land, get appended, and then get excised the moment the
        ;; summary arrives after it (see response-settled?). Park the
        ;; request; handle-summarization-received/-failed re-runs this
        ;; fn once summarization-pending? clears.
        (:summarization-pending? state)
        [(assoc state :resume-pending? true) nil]

        :else
        (let [next-iteration (if (= :auto-run (:batch-approval-mode response))
                                (inc (:batch-iteration response))
                                0)
              state' (assoc state :completion-pending? true :resume-pending? false)]
          [state' (request-completion-cmd (visible-messages (:conversation-log state')) next-iteration)]))
      [state nil])))

(defn continue-auto-run-cmd
  "If the latest response is an unresolved auto_run batch (and not
  interrupted), returns a cmd to execute its next pending call."
  [log]
  (when-let [response (latest-assistant-response log)]
    (when (and (= :auto-run (:batch-approval-mode response))
               (not (:interrupted? response)))
      (when-let [call (next-pending-call response)]
        (execute-tool-call-cmd call)))))

;; ---------------------------------------------------------------------------
;; charm.clj wiring
;; ---------------------------------------------------------------------------

(def my-input (text-input/text-input :prompt ":> " :placeholder ""))

;; The message-history viewport and the always-focused text-input receive
;; every key unconditionally (see update-fn*'s :else branch), so their
;; bindings must not collide. viewport's defaults (j/k/g/G, home/end,
;; ctrl+u/ctrl+d) collide with plain typing and with text-input's own
;; word/line-editing bindings; keep only the arrow/page keys, which
;; text-input never uses.
(def viewport-scroll-keys
  {:line-up ["up"] :line-down ["down"]
   :page-up ["pgup"] :page-down ["pgdown"]
   :half-page-up [] :half-page-down []
   :top [] :bottom []})

(defn init []
  (let [[s s-cmd] (spinner/spinner-init (spinner/spinner :dots))
        [vp vp-cmd] (viewport/viewport-init (viewport/viewport "" :keys viewport-scroll-keys))
        {:keys [conversation-log approval-mode]} (load-conversation)
        state (sync-viewport-content
               {:llm-query my-input
                :spinner s
                :viewport vp
                :current :llm-query
                :conversation-log conversation-log
                :approval-mode (or approval-mode :manual)
                :completion-pending? false
                :summarization-pending? false
                :terminal-width 120
                :terminal-height 80})]
    [state (program/batch s-cmd vp-cmd)]))

(defn submit-prompt [state]
  (let [text (str/trim (text-input/value (:llm-query state)))]
    (cond
      (or (str/blank? text) (not= :composing (tui-mode state)))
      [state nil]

      (= text "/compact")
      [(-> state
           (assoc :summarization-pending? true)
           (assoc :llm-query (text-input/reset (:llm-query state))))
       (summarize-conversation-cmd (visible-messages (:conversation-log state)))]

      :else
      (let [new-log (conj (:conversation-log state)
                           {:type :user-prompt :created-at (System/currentTimeMillis) :text text})
            state' (-> state
                       (assoc :conversation-log new-log)
                       (assoc :completion-pending? true)
                       (assoc :llm-query (text-input/reset (:llm-query state))))]
        [state' (request-completion-cmd (visible-messages new-log) 0)]))))

(defn handle-completion-received [state msg]
  (let [{:keys [iteration text tool-call-proposals total-tokens]} msg
        tool-calls (vec (map-indexed proposal->tool-call tool-call-proposals))
        response {:type :assistant-response :created-at (System/currentTimeMillis)
                   :text text :tool-calls tool-calls
                   :batch-approval-mode (:approval-mode state)
                   :batch-iteration iteration
                   :interrupted? false
                   :total-tokens total-tokens}
        log' (conj (:conversation-log state) response)
        ;; Don't kick off a second summarization while one's already in
        ;; flight (either automatic or from a prior "/compact"), and
        ;; don't kick one off at all while this response still has an
        ;; open batch (see response-settled?) - wait for it to resolve
        ;; first so the eventual cut can't land mid-batch.
        trigger-compaction? (and (needs-compaction? response)
                                  (not (:summarization-pending? state))
                                  (response-settled? response))
        state' (cond-> (assoc state :conversation-log log' :completion-pending? false)
                 trigger-compaction? (assoc :summarization-pending? true))
        compact-cmd (when trigger-compaction? (summarize-conversation-cmd (visible-messages log')))
        [state'' next-cmd] (if (all-calls-resolved? response)
                              (maybe-resolve-batch state')
                              [state' (continue-auto-run-cmd log')])]
    [state'' (batch-cmds compact-cmd next-cmd)]))

(defn handle-completion-failed [state msg]
  [(-> state
       (update :conversation-log conj
               {:type :llm-error :created-at (System/currentTimeMillis) :error (:error msg)})
       (assoc :completion-pending? false))
   nil])

(defn resume-after-summarization
  "Re-runs the completion/tool-call request that maybe-resolve-batch
  parked while this summarization was in flight (see its
  :summarization-pending? branch). No-op if nothing was parked."
  [state]
  (if (:resume-pending? state)
    (maybe-resolve-batch (dissoc state :resume-pending?))
    [state nil]))

(defn handle-summarization-received [state msg]
  (resume-after-summarization
   (-> state
       (update :conversation-log conj
               {:type :compaction-summary :created-at (System/currentTimeMillis) :summary (:summary msg)})
       (assoc :summarization-pending? false))))

(defn handle-summarization-failed [state msg]
  (resume-after-summarization
   (-> state
       (update :conversation-log conj
               {:type :llm-error :created-at (System/currentTimeMillis) :error (:error msg)})
       (assoc :summarization-pending? false))))

(defn handle-tool-call-executed [state msg]
  (let [log' (update-latest-response (:conversation-log state)
                                      (fn [r] (update-tool-call r (:call-id msg)
                                                                 (fn [c] (assoc c :status :executed
                                                                                :stdout (:stdout msg)
                                                                                :stderr (:stderr msg)
                                                                                :exit-code (:exit-code msg)
                                                                                :executed-at (System/currentTimeMillis))))))
        state' (assoc state :conversation-log log')
        [state'' cmd] (maybe-resolve-batch state')]
    (if cmd
      [state'' cmd]
      [state'' (continue-auto-run-cmd (:conversation-log state''))])))

(defn approve-current-call [state]
  (let [response (latest-assistant-response (:conversation-log state))
        call (and response (= :manual (:batch-approval-mode response)) (next-pending-call response))]
    (if-not call
      [state nil]
      (let [log' (update-latest-response (:conversation-log state)
                                          #(update-tool-call % (:id call) (fn [c] (assoc c :status :approved))))]
        [(assoc state :conversation-log log') (execute-tool-call-cmd call)]))))

(defn reject-current-call [state]
  (let [response (latest-assistant-response (:conversation-log state))
        call (and response (= :manual (:batch-approval-mode response)) (next-pending-call response))]
    (if-not call
      [state nil]
      (let [log' (update-latest-response (:conversation-log state)
                                          #(update-tool-call % (:id call) (fn [c] (assoc c :status :rejected))))]
        (maybe-resolve-batch (assoc state :conversation-log log'))))))

(defn interrupt-auto-run [state]
  (let [response (latest-assistant-response (:conversation-log state))]
    (if-not (and response (= :auto-run (:batch-approval-mode response)) (next-pending-call response))
      [state nil]
      (let [log' (update-latest-response (:conversation-log state)
                                          (fn [r]
                                            (-> r
                                                (assoc :interrupted? true)
                                                (update :tool-calls
                                                        (fn [calls]
                                                          (mapv (fn [c] (if (= :pending-approval (:status c))
                                                                          (assoc c :status :interrupted)
                                                                          c))
                                                                calls))))))]
        (maybe-resolve-batch (assoc state :conversation-log log'))))))

(defn toggle-approval-mode [state]
  [(update state :approval-mode #(if (= % :manual) :auto-run :manual)) nil])

(defn remap-shadowed-word-edit-key
  "charm.components.text-input's key-match? ignores modifiers for plain
  (no '+') binding strings, and its cond checks single-character actions
  (bound to plain \"left\"/\"right\"/\"backspace\"/\"delete\") before the
  word-level ones that share those base keys - so alt+left, ctrl+left,
  alt+right, ctrl+right, alt+backspace and alt+delete never fire; they
  silently act as single-character moves/deletes instead. The library's
  own emacs-style bindings (alt+b/f/d, ctrl+w) don't share a base key
  with anything earlier in that cond, so they work correctly. Translate
  the dead combos to the working ones rather than patch the dependency."
  [msg]
  (cond
    (or (msg/key-match? msg "ctrl+left") (msg/key-match? msg "alt+left"))
    (msg/key-press "b" :alt true)

    (or (msg/key-match? msg "ctrl+right") (msg/key-match? msg "alt+right"))
    (msg/key-press "f" :alt true)

    (msg/key-match? msg "alt+backspace")
    (msg/key-press "w" :ctrl true)

    (msg/key-match? msg "alt+delete")
    (msg/key-press "d" :alt true)

    :else msg))

(defn update-fn* [state msg]
  (cond
    (msg/window-size? msg)
    ;; Actual dimension-setting happens uniformly in sync-viewport-content
    ;; (called from the update-fn wrapper after every message, not just
    ;; resize), since input growth must also resize the viewport.
    [(assoc state :terminal-width (:width msg) :terminal-height (:height msg))
     nil]

    (msg/key-match? msg "ctrl+c")
    [state program/quit-cmd]

    (msg/key-match? msg "ctrl+t")
    (toggle-approval-mode state)

    (and (= :reviewing-batch (tui-mode state)) (msg/key-match? msg "y"))
    (approve-current-call state)

    (and (= :reviewing-batch (tui-mode state)) (msg/key-match? msg "n"))
    (reject-current-call state)

    (and (= :reviewing-batch (tui-mode state)) (msg/key-match? msg "i"))
    (interrupt-auto-run state)

    (msg/key-match? msg "enter")
    (submit-prompt state)

    (spinner/spinning? (:spinner state) msg)
    (let [[s cmd] (spinner/spinner-update (:spinner state) msg)]
      [(assoc state :spinner s) cmd])

    (= :completion-received (:type msg))
    (handle-completion-received state msg)

    (= :completion-failed (:type msg))
    (handle-completion-failed state msg)

    (= :tool-call-executed (:type msg))
    (handle-tool-call-executed state msg)

    (= :summarization-received (:type msg))
    (handle-summarization-received state msg)

    (= :summarization-failed (:type msg))
    (handle-summarization-failed state msg)

    :else
    (let [field (:current state)
          msg' (remap-shadowed-word-edit-key msg)
          [input cmd] (text-input/text-input-update (get state field) msg')
          [vp vp-cmd] (viewport/viewport-update (get state :viewport) msg')]
      [(-> state
           (assoc field input)
           (assoc :viewport vp))
       (program/batch cmd vp-cmd)])))

(defn- wrap-single-line
  "Word-wraps text with no embedded newlines. Blank input stays blank
  (preserves paragraph breaks - see wrap-text)."
  [text max-width]
  (if (str/blank? text)
    [text]
    (let [{:keys [lines current]}
          (reduce (fn [{:keys [current lines] :as acc} word]
                    (let [new-current (if (empty? current) word (str current " " word))]
                      ;; Only break if there was already something on the
                      ;; line - a lone word wider than max-width must still
                      ;; go somewhere; breaking here would just insert an
                      ;; empty line before it instead of after.
                      (if (and (seq current) (> (ansi/string-width new-current) max-width))
                        (assoc acc :current word :lines (conj lines current))
                        (assoc acc :current new-current :lines lines))))
                  {:current "" :lines []}
                  (str/split text #" "))]
      (conj lines current))))

(defn wrap-text
  "Word-wraps text to max-width, treating embedded newlines as hard
  breaks. LLM output routinely contains markdown like \"**Heading**\\nBody
  text\" with no space around the newline; wrapping the whole blob as one
  space-delimited token stream (the previous behaviour) glued unrelated
  lines into the same wrap unit and produced garbled, off-width breaks
  with no relation to the actual paragraph structure."
  [text max-width]
  (mapcat #(wrap-single-line % max-width) (str/split text #"\n")))

(defn tool-call-line
  "One display entry per ToolCall: always shows tool_name and the actual
  command (see convocli.allium's ToolCall - both are present regardless
  of outcome, except command when unknown-tool/mapper-failure never
  produced one). Executed calls also show exit code and stdout/stderr,
  since exit code alone gives no indication of what the command did."
  [c]
  (let [header (str ">=> " (:tool-name c) " [" (name (:status c)) "]")]
    (cond
      (and (= :executed (:status c)) (nil? (:command c)))
      (str header " " (:stderr c))

      (= :executed (:status c))
      (str header " " (:command c) " (exit " (:exit-code c) ")"
           (when (seq (:stdout c)) (str "\n" (:stdout c)))
           (when (seq (:stderr c)) (str "\nSTDERR: " (:stderr c))))

      :else
      (str header " " (:command c)))))

(defn message-lines
  [{:keys [type] :as m}]
  (case type
    :user-prompt [(str "<O_O> => " (:text m))]
    :assistant-response
    (into (if (seq (:text m)) [(str "[*_*] => " (:text m))] [])
          (map tool-call-line (:tool-calls m)))
    :llm-error [(str "[!!!] LLM error: " (:error m))]
    :auto-cap-reached [(str "[...] auto-run stopped: hit the " (:iterations-run m) "-iteration cap")]
    :auto-run-interrupted [(str "[...] auto-run interrupted (" (:calls-skipped m) " call(s) skipped)")]
    :compaction-summary [(str "[compacted] " (:summary m))]
    []))

(defn conversation-display-text
  "Wrapped history text for the scrollable viewport. Pure; does not
  include the status line, which changes every tick (spinner) rather
  than when conversation-log changes, and must not reset viewport scroll
  as a side effect of that.

  width should be the viewport's own configured :width, not a guessed
  offset from the raw terminal width - viewport-view pads/truncates each
  line to that same width, so wrapping any narrower than it just wastes
  space (every line gets padded back out) without fitting more text on
  screen."
  [conversation-log width]
  (let [lines (mapcat message-lines (visible-messages conversation-log))]
    (str/join "\n" (mapcat #(wrap-text % width) lines))))

(defn page-width
  "The inner content width shared by every pane (header/content/footer) -
  see render-page. Duplicated here (rather than threaded as a parameter)
  because both rendering and viewport/footer sizing need the identical
  value to avoid overflowing or under-using the bordered box."
  [terminal-width]
  (max 60 (- (or terminal-width 120) 4)))

(defn footer-lines
  "The input line, wrapped to the page width so it grows in height to
  fit longer input instead of running off-screen. Needed both for
  rendering (render-footer) and for budgeting how much height remains
  for the history viewport (see sync-viewport-content) - both must
  agree on the same line count, or the footer box and the space
  reserved for it drift apart."
  [{:keys [approval-mode llm-query terminal-width]}]
  (wrap-text (str "[" (name approval-mode) ", ctrl+t to toggle] " (text-input/text-input-view llm-query))
             (page-width terminal-width)))

(defn sync-viewport-content
  "Keeps the viewport's dimensions and persisted content in sync with
  conversation-log/terminal size/input length, scrolling to the newest
  message when content changes. Must be called whenever state changes,
  never from a pure render fn - viewport-set-content resets scroll, so
  calling it on every render (rather than only when content actually
  changed) would make manual scrolling impossible. Dimensions are
  updated unconditionally on every call (not just on resize), since a
  growing multi-line input must shrink the viewport in real time to
  keep the total layout within the terminal's real height;
  viewport-set-dimensions only clamps the scroll offset, never resets
  it, so this part is always safe to repeat.

  force? bypasses the content no-op-if-unchanged check: needed right
  after the viewport's height becomes known (the initial window-size
  message), since short lines often wrap identically at any width, so
  the unchanged-text check alone would never notice that scroll-to-bottom
  now has a real height to compute against instead of the 0 it started
  with."
  ([state] (sync-viewport-content state false))
  ([state force?]
   (let [width (page-width (:terminal-width state))
         ;; See render-page: header (3 rows) + content borders (2) +
         ;; footer borders (2) + 4 rows of slack = 11, plus however many
         ;; rows the footer itself now takes (1 when the input is short,
         ;; matching the original fixed budget exactly).
         height (max 1 (- (or (:terminal-height state) 80) 11 (count (footer-lines state))))
         state (update state :viewport #(viewport/viewport-set-dimensions % width height))
         text (conversation-display-text (:conversation-log state) width)]
     (if (and (not force?) (= text (viewport/viewport-content (:viewport state))))
       state
       (update state :viewport #(-> % (viewport/viewport-set-content text) viewport/scroll-to-bottom))))))

(defn update-fn [state msg]
  (let [[new-state cmd] (update-fn* state msg)
        resized? (not= (:terminal-height state) (:terminal-height new-state))
        new-state (sync-viewport-content new-state resized?)]
    (when (or (not= (:conversation-log state) (:conversation-log new-state))
              (not= (:approval-mode state) (:approval-mode new-state)))
      (save-conversation! new-state))
    [new-state cmd]))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(defn render-header []
  "ConvoCLI")

(defn render-content
  [{:keys [spinner viewport] :as state}]
  (let [mode (tui-mode state)
        status (case mode
                 :awaiting-completion (str "[-_o] => " (spinner/spinner-view spinner))
                 :summarizing (str "[-_o] => " (spinner/spinner-view spinner) " compacting...")
                 :reviewing-batch
                 (let [response (latest-assistant-response (:conversation-log state))]
                   (if (= :manual (:batch-approval-mode response))
                     "[y] approve  [n] reject"
                     "auto-running... [i] interrupt"))
                 :composing nil)]
    (style/render
     (style/style)
     (str (viewport/viewport-view viewport)
          (when status (str "\n\n" status))))))

(defn render-footer [state]
  (str/join "\n" (footer-lines state)))

(defn render-page
  [header content footer & {:keys [width]}]
  (let [w (page-width width)]
    (style/join-vertical
     :left
     (style/render (style/style :width w :align :center :border border/normal) header)
     (style/render (style/style :width w :align :left :border border/rounded) content)
     (style/render (style/style :width w :align :left :border border/normal) footer))))

(defn view-2 [state]
  (render-page
   (render-header)
   (render-content state)
   (render-footer state)
   :width (:terminal-width state)))

(defn -main [& _args]
  (validate-config!)
  (program/run {:init init
                :update #'update-fn
                :view #'view-2
                :alt-screen true}))
