# convocli

A light-weight conversational TUI for driving the Event Modeling CLI (and perhaps other CLI's).

## Install

```
bbin install io.github.mdiin/convocli
```

Requires [bbin](https://github.com/babashka/bbin). Or grab a prebuilt binary from [Releases](https://github.com/mdiin/convocli/releases).

## Configure

convocli reads `config.edn` and `tools.edn` from `.convocli/` in the
directory it's invoked from. If either file isn't there, it falls back
to `$XDG_CONFIG_HOME/convocli/` (or `~/.config/convocli/` if
`XDG_CONFIG_HOME` isn't set).

If neither location has them, convocli seeds
`./.convocli/config.edn` and `./.convocli/tools.edn` with defaults on
first run, so there's something to edit right away instead of a crash.

### config.edn

```clojure
{:llm-endpoint "http://localhost:9090/v1/chat/completions"
 :llm-model "your-model-id-or-alias"
 :context-window-override nil
 :max-auto-iterations 10
 :system-prompt "You know Event Modeling (Adam Dymitruk). Your goal is to command the Event Modeling CLI. Whenever you have enough knowledge to invoke a tool, do so."}
```

- `:llm-endpoint` / `:llm-model` - required. Point these at an
  OpenAI-compatible chat completions endpoint (e.g. llama.cpp, vLLM).
- `:context-window-override` - set to a token count to skip querying
  the endpoint's `/v1/models` for the model's context window.
- `:max-auto-iterations` - how many tool-call iterations convocli will
  run automatically before falling back to manual approval.
- `:system-prompt` - the system prompt sent with every request.

### tools.edn

Defines the tools convocli exposes to the LLM: an OpenAI-style function
tool definition per entry, plus a mapper (real Clojure code, evaluated
via [sci](https://github.com/babashka/sci)) that turns the LLM's JSON
arguments into a shell command to run. Edit entries by hand, or
regenerate a starting point from an emcli manifest with:

```
bb scripts/generate_tools.clj <manifest-path> [out-path]
```

### Conversation history

Conversation history is persisted to `./.convocli/conversation.edn` and
resumed automatically on the next launch from the same directory.
