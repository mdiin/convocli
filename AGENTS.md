# convocli

A light-weight conversational TUI for driving the Event Modeling CLI (and perhaps other CLI's).

## Tech stack

- Babashka
- charm for TUI development
- Local LLM for the conversational interface

## Guidelines
- Use an idiomatic functional style
- Use transducers if possible

## Config, tools, and conversation history
- `config.edn` and `tools.edn` are looked up under `./.convocli/` in the
  directory convocli was invoked from, falling back to
  `$XDG_CONFIG_HOME/convocli/` (or `~/.config/convocli/`) if not found
  there.
- If neither location has them, convocli seeds `./.convocli/config.edn`
  and `./.convocli/tools.edn` from the copies bundled on the classpath
  (see `.convocli/config.edn.example` and `.convocli/tools.edn` in this
  repo) so a first run has something to edit instead of crashing.
- Conversation history is persisted to `./.convocli/conversation.edn`.

## End goal
- Package as single binary
