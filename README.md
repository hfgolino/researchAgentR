# researchAgentR

LLM agents for research, built from first principles in R.

`researchAgentR` is the companion package of the seminar *LLM Agents for
Research Using R* (Hudson Golino, University of Virginia). It implements
agents in plain R, on top of `httr2` and `jsonlite` only, so that every part
can be read and changed: the agent loop (`R/agent.R`) is short enough to read
in one sitting, and the package source is part of the course material.

Every agent run is traced, costed and saved, because the course teaches how to
evaluate agents, not how to trust them.

## Installation

```r
install.packages("remotes")
remotes::install_github("hfgolino/researchAgentR")
```

R 4.1 or later works; R 4.3 or later is recommended. No Python is needed.
Optional features use packages from `Suggests` (`callr`, `xml2`, `pdftools`,
`ggplot2`, `reticulate` and others) and say so when one is missing.

**Course participants:** follow the step-by-step [setup guide](inst/SETUP.md)
(R and RStudio, packages, the three free API keys and where to get them, the
setup check, costs and troubleshooting). After installing the package,
`setup_guide()` opens the same guide in R.

## Keys

Keys live only in `~/.Renviron`, one per line, and are read with
`Sys.getenv()`. A key is never typed in a script.

| Line in `~/.Renviron` | Needed for | Cost |
|---|---|---|
| `GROQ_API_KEY=...` | the default models (`openai/gpt-oss-120b`, `openai/gpt-oss-20b`) | free tier |
| `OPENALEX_API_KEY=...` | literature search at a useful daily budget | free |
| `RESEARCHAGENTR_EMAIL=you@university.edu` | polite access to Crossref and Unpaywall | free |
| `TAVILY_API_KEY=...` | web search | free plan |
| `OPENAI_API_KEY=...`, `ANTHROPIC_API_KEY=...` | optional paid models | paid, optional |

```r
library(researchAgentR)
verify_setup(test_apis = TRUE)   # checks every key and service
```

## A first agent

```r
library(researchAgentR)

# 1. A tool is an R function plus a description written for the model.
word_count <- function(text) length(strsplit(trimws(text), "\\s+")[[1]])
count_tool <- define_tool(word_count, "Count the words in a text.",
                          args = list(text = arg_string("The text to count")))

# 2. An agent is a model, instructions and tools.
counter <- agent("counter",
                 "Count the words of the quoted text with the word_count tool, then report the number it returned.",
                 model = "groq:openai/gpt-oss-20b", tools = list(count_tool))

# 3. One run is one episode, with explicit limits.
ep <- run(counter, "How many words are in 'agents use tools in a loop'?",
          budget(max_steps = 4, max_cost = 0.01))
ep                 # the trajectory: every model call and tool call, with cost
ep$answer
view_episode(ep)   # the same episode as a self-contained HTML page
```

## Any model, any provider

A model is written as `"provider:model"`. The same code runs on any of them:

```r
chat("In one sentence, what is exploratory graph analysis?", model = "groq:openai/gpt-oss-120b")
chat("Same question.", model = "openai:gpt-6-luna")            # paid
chat("Same question.", model = "anthropic:claude-haiku-5-5")   # paid
chat("Same question.", model = "ollama:qwen3:4b")              # local, optional
providers()          # Groq, OpenAI, Anthropic, Gemini, Mistral, Cerebras, OpenRouter, Ollama, LM Studio
model_registry()     # context windows, capabilities and prices (with the date checked)
register_provider("myserver", "http://localhost:8080/v1", local = TRUE)   # any compatible endpoint
```

## What is inside

| Area | Main functions |
|---|---|
| Setup | `setup_guide()`, `verify_setup()`, `set_groq_key()` and relatives |
| Models | `chat()`, `ask()`, `providers()`, `model_registry()`, `set_default_model()` |
| Tools | `define_tool()`, `arg_string()` and relatives, `call_tool()`, `builtin_tool()`, `remote_mcp_tool()` |
| Agent loop | `agent()`, `run()`, `budget()` |
| Tracing ("Lens") | `view_episode()`, `episodes_df()`, `episode_steps()`, `save_episode()`, `cost_ledger()` |
| Context and memory | `ctx_window()`, `ctx_compact()`, `ctx_notes()`, `subagent()`, `compress_text()`, `vector_store()`, `embed_texts()`, `ingest_library()` |
| Research tools | `search_openalex()`, `search_crossref()`, `search_europepmc()`, `search_arxiv()`, `search_nih_projects()`, `search_nsf_awards()`, `search_grants_gov()`, `web_search()`, `read_page()`, `run_r_code()` |
| Verification | `verify_citations()`, `check_claims()`, `plain_punctuation()`, `ai_use_statement()` |
| Multi-agent | `team()`, `as_tool()`, `fan_out()`, `route()`, `critique_loop()`, `checkpoint()`, `compare_single_vs_team()` |
| MCP | `mcp_connect()`, `mcp_list_tools()`, `mcp_call()`, `mcp_as_tools()` |
| Evaluation | `task_set()`, `run_eval()`, `grader_code()`, `grader_judge()`, `pass_at_k()`, `pass_hat_k()`, `paired_bootstrap()`, `plot_frontier()` |
| Cost | `cascade()`, `baselines()`, `filter_output()`, `use_cache()`, `keep()` |
| Writing | `create_skill()`, `learn_style()`, `write_in_style()`, `review_manuscript()` |

## Activities

Twenty guided scripts, in the format of the `llmClassificR` activities (goal,
setup, steps with expected output, exercises, solutions, what can go wrong):

```r
file.copy(system.file("activities", package = "researchAgentR"), ".", recursive = TRUE)
```

Start with `00_Setup_researchAgentR.R`. The list, with the functions each
script demonstrates, is in `activities/README.md`.

## Record once, replay offline

```r
use_cache("auto")                       # save every API response; replay it next time
ep <- keep("my_run", run(counter, "How many words are in 'record once, replay often'?"))
```

A recorded response replays on another computer and without the key that
made it, so a class demo does not depend on the network. A replayed call costs
nothing and is marked `cached = TRUE` in the ledger: never report costs from a
replay.

## Research integrity and safety

- **Verify, do not trust.** Check every reference with `verify_citations()`
  and every claim with `check_claims()`, which requires a verbatim quote.
- **Document.** `ai_use_statement()` drafts a disclosure from the saved
  episodes; `course_template()` copies the checklist and rubric templates.
- **Confidential and participant data never go to a cloud model.** Local
  embeddings and document search (`ingest_library()`, `store_search()`) keep
  such material on the computer.
- **Peer review.** Many funders and journals forbid generative AI in peer
  review (for example NIH notice NOT-OD-23-149). `review_manuscript()` is for
  one's own work.
- **Tool output is untrusted.** It is marked as data in the prompt, because a
  web page can contain instructions written for the model.
- **Code execution.** `run_r_code()` is a soft sandbox (a separate process,
  a time limit, a temporary folder, no network by default), not a security
  boundary. Use `approval = TRUE` to read code before it runs.

## Development

`R CMD check` is clean and the unit tests need no network. There is no mock
model: everything that calls a model or an API is tested live
(`tools/run_live_tests.R`, `tools/run_activities.R`). Conventions are in
`inst/DEVELOPERS.md`; changes are in `NEWS.md`.

## Credits and license

The course arc (watch an agent, build the loop, make every token count, serve
a small model) is inspired by Harvard CS2680, *Modern AI Systems: Agents and
System Optimizations* (Juncheng Yang, Fall 2026, <https://cs2680.seas.harvard.edu>).
No material from that course is reproduced here.

GPL (>= 3). Hudson Golino, University of Virginia.
