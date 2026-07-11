(ns convocli.core-test
  "Unit tests for convocli.core's pure/deterministic logic - the state
  machine, rendering and serialization described in convocli.allium and
  tui.allium. Deliberately excludes anything that needs a live LLM
  endpoint (request-completion-cmd) - that's exercised manually against
  a real server, documented in commit history. execute-tool-call-cmd IS
  tested, but only by shelling out to `echo`/`false`, never a real
  destructive command."
  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.string :as str]
   [cheshire.core :as json]
   [charm.message :as msg]
   [charm.ansi.width :as ansi]
   [charm.components.viewport :as viewport]
   [charm.components.text-input :as text-input]
   [babashka.process :as process]
   [convocli.core :as c]))

;; ---------------------------------------------------------------------------
;; tui.allium: mode derivation
;; ---------------------------------------------------------------------------

(deftest tui-mode-test
  (testing "composing: nothing pending, no unresolved batch"
    (is (= :composing (c/tui-mode {:conversation-log [] :completion-pending? false}))))
  (testing "awaiting-completion: a request is in flight"
    (is (= :awaiting-completion (c/tui-mode {:conversation-log [] :completion-pending? true}))))
  (testing "reviewing-batch: an unresolved batch takes priority over completion-pending"
    (let [response {:type :assistant-response
                     :tool-calls [{:id "1" :sequence 0 :status :pending-approval}]}]
      (is (= :reviewing-batch (c/tui-mode {:conversation-log [response] :completion-pending? true})))))
  (testing "a response with zero tool-calls is not a batch"
    (let [response {:type :assistant-response :tool-calls []}]
      (is (= :composing (c/tui-mode {:conversation-log [response] :completion-pending? false}))))))

;; ---------------------------------------------------------------------------
;; Sequential approval (next_pending_call / all_calls_resolved)
;; ---------------------------------------------------------------------------

(deftest sequential-approval-test
  (let [response {:type :assistant-response
                   :tool-calls [{:id "1" :sequence 0 :status :pending-approval}
                                {:id "2" :sequence 1 :status :pending-approval}]}]
    (testing "next-pending-call is always the lowest sequence"
      (is (= "1" (:id (c/next-pending-call response)))))
    (testing "resolving the first advances to the second"
      (let [response' (c/update-tool-call response "1" #(assoc % :status :executed))]
        (is (= "2" (:id (c/next-pending-call response'))))
        (is (false? (c/all-calls-resolved? response')))))
    (testing "resolving both marks the batch resolved"
      (let [response' (-> response
                           (c/update-tool-call "1" #(assoc % :status :executed))
                           (c/update-tool-call "2" #(assoc % :status :rejected)))]
        (is (nil? (c/next-pending-call response')))
        (is (true? (c/all-calls-resolved? response')))))))

;; ---------------------------------------------------------------------------
;; Batch resolution: continue / cap / interrupted (BatchResolved rule)
;; ---------------------------------------------------------------------------

(deftest maybe-resolve-batch-test
  (testing "manual batch, fully resolved -> always continues (no cap in manual mode)"
    (let [response {:type :assistant-response :batch-approval-mode :manual :batch-iteration 0
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :executed}]}
          [state' cmd] (c/maybe-resolve-batch {:conversation-log [response] :completion-pending? false})]
      (is (true? (:completion-pending? state')))
      (is (some? cmd))))

  (testing "auto-run batch under the cap -> continues, iteration increments"
    (let [response {:type :assistant-response :batch-approval-mode :auto-run :batch-iteration 2
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :executed}]}
          [state' cmd] (c/maybe-resolve-batch {:conversation-log [response] :completion-pending? false})]
      (is (true? (:completion-pending? state')))
      (is (some? cmd))))

  (testing "auto-run batch at the cap -> records AutoCapReached, does not continue"
    (let [response {:type :assistant-response :batch-approval-mode :auto-run
                     :batch-iteration (:max-auto-iterations c/config)
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :executed}]}
          [state' cmd] (c/maybe-resolve-batch {:conversation-log [response] :completion-pending? false})]
      (is (nil? cmd))
      (is (= :auto-cap-reached (:type (last (:conversation-log state')))))))

  (testing "interrupted batch -> records AutoRunInterrupted, does not continue"
    (let [response {:type :assistant-response :batch-approval-mode :auto-run :batch-iteration 0
                     :interrupted? true
                     :tool-calls [{:id "1" :sequence 0 :status :executed}
                                  {:id "2" :sequence 1 :status :interrupted}]}
          [state' cmd] (c/maybe-resolve-batch {:conversation-log [response] :completion-pending? false})]
      (is (nil? cmd))
      (let [notice (last (:conversation-log state'))]
        (is (= :auto-run-interrupted (:type notice)))
        (is (= 1 (:calls-skipped notice))))))

  (testing "unresolved batch -> no-op"
    (let [response {:type :assistant-response :batch-approval-mode :manual :batch-iteration 0
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :pending-approval}]}
          state {:conversation-log [response] :completion-pending? false}
          [state' cmd] (c/maybe-resolve-batch state)]
      (is (= state state'))
      (is (nil? cmd)))))

;; ---------------------------------------------------------------------------
;; Manual approve/reject and auto-run interrupt (surface actions)
;; ---------------------------------------------------------------------------

(deftest approve-reject-interrupt-test
  (testing "approve-current-call only acts in manual mode, dispatches execution"
    (let [response {:type :assistant-response :batch-approval-mode :manual :batch-iteration 0
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :pending-approval :command "echo hi"}]}
          state {:conversation-log [response]}
          [state' cmd] (c/approve-current-call state)]
      (is (= :approved (:status (first (:tool-calls (c/latest-assistant-response (:conversation-log state')))))))
      (is (some? cmd))))

  (testing "approve-current-call is a no-op in auto-run mode"
    (let [response {:type :assistant-response :batch-approval-mode :auto-run :batch-iteration 0
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :pending-approval :command "echo hi"}]}
          state {:conversation-log [response]}
          [state' cmd] (c/approve-current-call state)]
      (is (= state state'))
      (is (nil? cmd))))

  (testing "reject-current-call marks rejected and checks for resolution"
    (let [response {:type :assistant-response :batch-approval-mode :manual :batch-iteration 0
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :pending-approval :command "echo hi"}]}
          state {:conversation-log [response] :completion-pending? false}
          [state' cmd] (c/reject-current-call state)]
      (is (= :rejected (:status (first (:tool-calls (c/latest-assistant-response (:conversation-log state')))))))
      (is (some? cmd) "batch became fully resolved, so a follow-up completion is requested")))

  (testing "interrupt-auto-run only acts in auto-run mode with a pending call, skips the rest"
    (let [response {:type :assistant-response :batch-approval-mode :auto-run :batch-iteration 0
                     :interrupted? false
                     :tool-calls [{:id "1" :sequence 0 :status :executed}
                                  {:id "2" :sequence 1 :status :pending-approval}]}
          state {:conversation-log [response] :completion-pending? false}
          [state' _cmd] (c/interrupt-auto-run state)
          response' (c/latest-assistant-response (:conversation-log state'))]
      (is (true? (:interrupted? response')))
      (is (= :interrupted (:status (second (:tool-calls response')))))))

  (testing "interrupt-auto-run is a no-op in manual mode"
    (let [response {:type :assistant-response :batch-approval-mode :manual :batch-iteration 0
                     :interrupted? false :tool-calls [{:id "1" :sequence 0 :status :pending-approval}]}
          state {:conversation-log [response]}
          [state' cmd] (c/interrupt-auto-run state)]
      (is (= state state'))
      (is (nil? cmd)))))

;; ---------------------------------------------------------------------------
;; Tool call resolution: unknown tool / mapper failure / success
;; ---------------------------------------------------------------------------

(def echo-tool-config
  {:name "echo_tool"
   :description "test tool"
   :parameters-schema {:type "object" :properties {"msg" {:type "string"}}}
   ;; mapper-source is a real form, not a string - see apply-mapper.
   :mapper-source '(fn [args-json]
                     (let [args (json-parse args-json)]
                       (str "echo " (shell-quote (:msg args)))))})

(def broken-tool-config
  {:name "broken_tool"
   :description "test tool whose mapper fails at evaluation, not at read time"
   :parameters-schema {:type "object"}
   :mapper-source '(fn [args-json] (this-function-does-not-exist args-json))})

(deftest apply-mapper-test
  (testing "resolves json-parse/json-encode/shell-quote regardless of ambient *ns*"
    (is (= "echo 'hi'" (c/apply-mapper echo-tool-config (json/encode {:msg "hi"})))))

  (testing "shell-quote neutralizes injection attempts - executing the command treats the whole
            malicious value as one inert argument rather than running the injected command"
    (let [cmd (c/apply-mapper echo-tool-config (json/encode {:msg "hi\"; echo INJECTED; echo \"bye"}))
          result (process/shell {:out :string :err :string :continue true} "bash" "-c" cmd)]
      (is (= 1 (count (str/split-lines (str/trim (:out result))))) "must produce exactly one line of output, not three")
      (is (str/includes? (:out result) "INJECTED") "the literal text still appears, just as inert data")))

  (testing "a broken mapper is caught, not thrown"
    (is (thrown? Exception (c/apply-mapper broken-tool-config "{}")))))

(deftest proposal->tool-call-test
  (with-redefs [c/tool-configs-by-name {"echo_tool" echo-tool-config}]
    (testing "known tool, valid mapper -> pending_approval with a command"
      (let [tc (c/proposal->tool-call 0 {:id "1" :tool-name "echo_tool" :arguments (json/encode {:msg "hi"})})]
        (is (= :pending-approval (:status tc)))
        (is (= "echo 'hi'" (:command tc)))
        (is (= "1" (:id tc)))
        (is (= "echo_tool" (:tool-name tc)))))

    (testing "unknown tool name -> executed with an explanatory stderr, no command"
      (let [tc (c/proposal->tool-call 0 {:id "1" :tool-name "does_not_exist" :arguments "{}"})]
        (is (= :executed (:status tc)))
        (is (nil? (:command tc)))
        (is (str/includes? (:stderr tc) "unknown tool"))))

    (testing "malformed arguments -> mapper failure caught, executed with stderr"
      (let [tc (c/proposal->tool-call 0 {:id "1" :tool-name "echo_tool" :arguments "not-json"})]
        (is (= :executed (:status tc)))
        (is (nil? (:command tc)))
        (is (str/includes? (:stderr tc) "mapper failed"))))))

;; ---------------------------------------------------------------------------
;; Completion handling: manual/auto-run batches, plain replies
;; ---------------------------------------------------------------------------

(deftest handle-completion-received-test
  (with-redefs [c/tool-configs-by-name {"echo_tool" echo-tool-config}]
    (testing "manual mode, one proposal -> pending batch, no auto-execution"
      (let [msg {:type :completion-received :iteration 0 :text nil
                 :tool-call-proposals [{:id "1" :tool-name "echo_tool" :arguments (json/encode {:msg "hi"})}]
                 :total-tokens 10}
            state {:conversation-log [] :approval-mode :manual :completion-pending? true}
            [state' cmd] (c/handle-completion-received state msg)
            response (c/latest-assistant-response (:conversation-log state'))]
        (is (false? (:completion-pending? state')))
        (is (= :manual (:batch-approval-mode response)))
        (is (= :pending-approval (:status (first (:tool-calls response)))))
        (is (nil? cmd))))

    (testing "auto-run mode, one proposal -> kicks off execution immediately"
      (let [msg {:type :completion-received :iteration 0 :text nil
                 :tool-call-proposals [{:id "1" :tool-name "echo_tool" :arguments (json/encode {:msg "hi"})}]
                 :total-tokens 10}
            state {:conversation-log [] :approval-mode :auto-run :completion-pending? true}
            [_state' cmd] (c/handle-completion-received state msg)]
        (is (some? cmd))))

    (testing "no tool calls -> plain reply, turn ends, mode returns to composing"
      (let [msg {:type :completion-received :iteration 0 :text "just a plain answer"
                 :tool-call-proposals [] :total-tokens 10}
            state {:conversation-log [] :approval-mode :manual :completion-pending? true}
            [state' cmd] (c/handle-completion-received state msg)]
        (is (nil? cmd))
        (is (= :composing (c/tui-mode state')))))))

(deftest handle-completion-failed-test
  (testing "records an LlmError and clears completion-pending"
    (let [[state' cmd] (c/handle-completion-failed {:conversation-log [] :completion-pending? true}
                                                    {:error "connection refused"})]
      (is (false? (:completion-pending? state')))
      (is (= :llm-error (:type (last (:conversation-log state')))))
      (is (nil? cmd)))))

;; ---------------------------------------------------------------------------
;; Executing an approved/auto-run call (real shell, safe commands only)
;; ---------------------------------------------------------------------------

(deftest execute-and-handle-tool-call-test
  (testing "a real (safe) shell command executes and the result is applied"
    (let [response {:type :assistant-response :batch-approval-mode :manual :batch-iteration 0
                     :interrupted? false
                     :tool-calls [{:id "1" :sequence 0 :status :approved :command "echo hello"}]}
          cmd (c/execute-tool-call-cmd (first (:tool-calls response)))
          exec-msg ((:fn cmd))]
      (is (= :tool-call-executed (:type exec-msg)))
      (is (= "hello\n" (:stdout exec-msg)))
      (is (zero? (:exit-code exec-msg)))

      (let [[state' _cmd] (c/handle-tool-call-executed {:conversation-log [response]} exec-msg)
            tc (first (:tool-calls (c/latest-assistant-response (:conversation-log state'))))]
        (is (= :executed (:status tc)))
        (is (= "hello\n" (:stdout tc))))))

  (testing "a nonzero exit is recorded, not thrown"
    (let [cmd (c/execute-tool-call-cmd {:id "1" :command "false"})
          exec-msg ((:fn cmd))]
      (is (= 1 (:exit-code exec-msg))))))

;; ---------------------------------------------------------------------------
;; Crash recovery
;; ---------------------------------------------------------------------------

(deftest recover-stuck-approvals-test
  (testing "an approved call (outcome unknown - crash mid-execution) resets to pending_approval"
    (let [log [{:type :assistant-response :batch-approval-mode :manual
                :tool-calls [{:id "1" :sequence 0 :status :approved}]}]
          recovered (c/recover-stuck-approvals log)]
      (is (= :pending-approval (:status (first (:tool-calls (first recovered))))))))

  (testing "leaves other statuses alone"
    (let [log [{:type :assistant-response :batch-approval-mode :manual
                :tool-calls [{:id "1" :sequence 0 :status :executed}
                             {:id "2" :sequence 1 :status :rejected}]}]
          recovered (c/recover-stuck-approvals log)]
      (is (= [:executed :rejected] (mapv :status (:tool-calls (first recovered))))))))

;; ---------------------------------------------------------------------------
;; Compaction (visible_messages)
;; ---------------------------------------------------------------------------

(deftest visible-messages-test
  (testing "no compaction yet -> everything is visible"
    (let [log [{:type :user-prompt :created-at 1 :text "a"}
               {:type :user-prompt :created-at 2 :text "b"}]]
      (is (= log (c/visible-messages log)))))

  (testing "visible_messages includes the compaction summary itself, not just what follows it"
    (let [log [{:type :user-prompt :created-at 1 :text "old"}
               {:type :compaction-summary :created-at 2 :summary "..."}
               {:type :user-prompt :created-at 3 :text "new"}]
          visible (c/visible-messages log)]
      (is (= 2 (count visible)))
      (is (= :compaction-summary (:type (first visible))))))

  (testing "uses the latest compaction if there are multiple"
    (let [log [{:type :compaction-summary :created-at 1 :summary "first"}
               {:type :user-prompt :created-at 2 :text "x"}
               {:type :compaction-summary :created-at 3 :summary "second"}
               {:type :user-prompt :created-at 4 :text "y"}]
          visible (c/visible-messages log)]
      (is (= "second" (:summary (first visible)))))))

;; ---------------------------------------------------------------------------
;; OpenAI wire-format serialization (id round-trip, tool result correlation)
;; ---------------------------------------------------------------------------

(deftest message->openai-test
  (testing "user prompt"
    (is (= [{:role "user" :content "hi"}]
           (c/message->openai {:type :user-prompt :text "hi"}))))

  (testing "assistant response with a resolved tool call round-trips the LLM's own id"
    (let [m {:type :assistant-response :text nil
             :tool-calls [{:id "call_abc" :tool-name "echo_tool" :arguments "{}"
                           :status :executed :stdout "ok" :stderr "" :exit-code 0}]}
          msgs (c/message->openai m)]
      (is (= 2 (count msgs)))
      (is (= "call_abc" (get-in (first msgs) [:tool_calls 0 :id])))
      (is (= "call_abc" (:tool_call_id (second msgs))))
      (is (= "tool" (:role (second msgs))))))

  (testing "an unresolved call (should never happen in practice) contributes no tool-result message"
    (let [m {:type :assistant-response :text nil
             :tool-calls [{:id "call_abc" :tool-name "echo_tool" :arguments "{}" :status :pending-approval}]}
          msgs (c/message->openai m)]
      (is (= 1 (count msgs)))))

  (testing "compaction summary becomes a user message (system is reserved for the leading prompt only)"
    (is (= "user" (:role (first (c/message->openai {:type :compaction-summary :summary "..."}))))))

  (testing "bookkeeping-only messages contribute nothing to the LLM's view"
    (is (empty? (c/message->openai {:type :llm-error :error "boom"})))
    (is (empty? (c/message->openai {:type :auto-cap-reached :iterations-run 10})))
    (is (empty? (c/message->openai {:type :auto-run-interrupted :calls-skipped 1})))))

(deftest compaction-end-to-end-wire-format-test
  (testing "post-compaction wire messages never carry a second system role
            (Ministral/Mistral-style templates reject 'system' anywhere but
            index 0 - this is what broke after resume from compaction)"
    (let [log [{:type :user-prompt :created-at 1 :text "old, should be dropped"}
               {:type :compaction-summary :created-at 2 :summary "user asked about X, agent did Y"}
               {:type :user-prompt :created-at 3 :text "continue where we left off"}
               {:type :assistant-response :created-at 4 :text "sure, doing that"}]
          wire (into [{:role "system" :content "you are an agent"}]
                     (c/conversation->openai-messages (c/visible-messages log)))]
      (is (= ["system" "user" "user" "assistant"] (mapv :role wire)))
      (is (= 1 (count (filter #(= "system" (:role %)) wire))))
      (is (= 0 (.indexOf (mapv :role wire) "system")))
      (is (str/includes? (:content (second wire)) "user asked about X, agent did Y")))))

;; ---------------------------------------------------------------------------
;; Line wrapping (embedded newlines, oversized tokens)
;; ---------------------------------------------------------------------------

(deftest wrap-text-test
  (testing "embedded newlines are hard breaks, not glued into the word stream"
    (is (= ["line one" "line two"] (c/wrap-text "line one\nline two" 30))))

  (testing "blank lines (paragraph breaks) are preserved"
    (is (= ["para one" "" "para two"] (c/wrap-text "para one\n\npara two" 30))))

  (testing "a single word wider than max-width overflows without an extra blank line first"
    (let [long-word (apply str (repeat 50 "x"))]
      (is (= [long-word] (c/wrap-text long-word 30)))))

  (testing "normal wrapping never exceeds max-width"
    (let [text (str/join " " (repeat 20 "word"))
          lines (c/wrap-text text 20)]
      (is (every? #(<= (ansi/string-width %) 20) lines)))))

;; ---------------------------------------------------------------------------
;; Tool call display (stdout/stderr surfaced, not just exit code)
;; ---------------------------------------------------------------------------

(deftest tool-call-line-test
  (testing "pending/approved/rejected/interrupted show tool_name + command, no output"
    (doseq [status [:pending-approval :approved :rejected :interrupted]]
      (let [line (c/tool-call-line {:tool-name "echo_tool" :status status :command "echo hi"})]
        (is (str/includes? line "echo_tool"))
        (is (str/includes? line "echo hi")))))

  (testing "executed shows exit code and stdout"
    (let [line (c/tool-call-line {:tool-name "echo_tool" :status :executed :command "echo hi"
                                   :stdout "hi\n" :stderr "" :exit-code 0})]
      (is (str/includes? line "(exit 0)"))
      (is (str/includes? line "hi"))))

  (testing "executed with a nonzero exit shows stderr"
    (let [line (c/tool-call-line {:tool-name "echo_tool" :status :executed :command "false"
                                   :stdout "" :stderr "boom" :exit-code 1})]
      (is (str/includes? line "(exit 1)"))
      (is (str/includes? line "boom"))))

  (testing "executed with no command (unknown tool/mapper failure) shows the error, not a blank exit code"
    (let [line (c/tool-call-line {:tool-name "echo_tool" :status :executed :command nil
                                   :stdout nil :stderr "unknown tool: echo_tool" :exit-code nil})]
      (is (str/includes? line "unknown tool"))
      (is (not (str/includes? line "(exit"))))))

;; ---------------------------------------------------------------------------
;; Viewport content sync (see the "scrolling never worked" fix)
;; ---------------------------------------------------------------------------

(def footer-test-fixture
  {:approval-mode :manual
   :llm-query (text-input/text-input :prompt ":> ")})

(defn fresh-viewport []
  (first (viewport/viewport-init (viewport/viewport "" :keys c/viewport-scroll-keys))))

(deftest sync-viewport-content-test
  (testing "populates the viewport's persisted content and real dimensions, not just a locally-discarded copy"
    (let [log [{:type :user-prompt :created-at 1 :text "hello"}]
          state (c/sync-viewport-content (merge footer-test-fixture
                                                 {:conversation-log log :terminal-width 100
                                                  :terminal-height 20 :viewport (fresh-viewport)}))]
      (is (pos? (viewport/viewport-line-count (:viewport state))))
      (is (pos? (:height (:viewport state))) "dimensions must come from terminal size, not stay at 0")))

  (testing "no-op (content unchanged) unless force?"
    (let [log (mapv (fn [i] {:type :user-prompt :created-at i :text (str "line " i)}) (range 50))
          state0 (merge footer-test-fixture {:conversation-log log :terminal-width 100
                                              :terminal-height 20 :viewport (fresh-viewport)})
          state1 (c/sync-viewport-content state0)
          scrolled (update state1 :viewport viewport/scroll-up)
          state2 (c/sync-viewport-content scrolled)]
      (is (= (:y-offset (:viewport scrolled)) (:y-offset (:viewport state2)))
          "unchanged content must not reset a manual scroll position")))

  (testing "force? bypasses the no-op check, re-scrolling to the bottom despite unchanged content"
    (let [log (mapv (fn [i] {:type :user-prompt :created-at i :text (str "line " i)}) (range 50))
          state0 (merge footer-test-fixture {:conversation-log log :terminal-width 100
                                              :terminal-height 20 :viewport (fresh-viewport)})
          state1 (c/sync-viewport-content state0)
          scrolled (update state1 :viewport viewport/scroll-up)
          state2 (c/sync-viewport-content scrolled true)]
      (is (> (:y-offset (:viewport state2)) (:y-offset (:viewport scrolled)))
          "force? must re-scroll to the bottom even though the text itself didn't change"))))

;; ---------------------------------------------------------------------------
;; Growing input (footer-lines / page-width) - see sync-viewport-content
;; ---------------------------------------------------------------------------

(deftest growing-input-test
  (testing "short input is one footer line; the viewport gets the full budgeted height"
    (let [state (c/sync-viewport-content (merge footer-test-fixture
                                                 {:conversation-log [] :terminal-width 60
                                                  :terminal-height 20 :viewport (fresh-viewport)}))]
      (is (= 1 (count (c/footer-lines state))))
      (is (= 8 (:height (:viewport state))))))

  (testing "long input grows the footer and shrinks the viewport by exactly as much,
            keeping the total layout within the same terminal height"
    (let [long-input (text-input/set-value (text-input/text-input :prompt ":> ")
                                            (str/join " " (repeat 30 "word")))
          state (c/sync-viewport-content (merge footer-test-fixture
                                                 {:llm-query long-input
                                                  :conversation-log [] :terminal-width 60
                                                  :terminal-height 20 :viewport (fresh-viewport)}))
          footer-count (count (c/footer-lines state))]
      (is (> footer-count 1))
      (is (= (- 20 11 footer-count) (:height (:viewport state)))))))

;; ---------------------------------------------------------------------------
;; Word-edit key shadowing workaround
;; ---------------------------------------------------------------------------

(deftest remap-shadowed-word-edit-key-test
  (testing "translates dead combos to the library's working equivalents"
    (is (= "b" (:key (c/remap-shadowed-word-edit-key (msg/key-press "left" :ctrl true)))))
    (is (= "f" (:key (c/remap-shadowed-word-edit-key (msg/key-press "right" :alt true)))))
    (is (= "w" (:key (c/remap-shadowed-word-edit-key (msg/key-press "backspace" :alt true)))))
    (is (= "d" (:key (c/remap-shadowed-word-edit-key (msg/key-press "delete" :alt true))))))

  (testing "leaves everything else untouched"
    (let [msg (msg/key-press "x")]
      (is (= msg (c/remap-shadowed-word-edit-key msg))))))
