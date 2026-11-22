# researchAgentR 0.1.1 (November 22, 2026)

Changes from the test pass ten days before the December 2026 seminar.

## Recordings replay for everyone

* The cache key of a request no longer depends on optional keys or e-mail
  addresses in its URL. Before, a response recorded with an OpenAlex key or a
  Crossref e-mail address replayed only for people who also had one.
  **Responses recorded with earlier versions through OpenAlex, Crossref or
  Unpaywall need to be recorded again.**
* A key is required only for a live call. A recorded web search, embedding
  or classification replays without the Tavily, Jina, Voyage, OpenAI or
  TypeSafe key; a live call without its key stops before any request, with
  the name of the line to add to `~/.Renviron`.
* A request that gets no answer is reported in plain words ("Ollama is not
  running: open the Ollama app...", "Could not reach https://... Check the
  internet connection...") instead of as a curl error.

## Models and prices (checked against the providers' pages on 2026-11-22)

* New registry rows: `openai:gpt-6.2-astra`, `openai:gpt-6.2-sol`,
  `openai:gpt-6.1-terra`, `anthropic:claude-fable-5-2`.
* OpenAI long-context prices (prompts over 272,000 tokens: twice the input
  and cache rates, 1.5 times the output rate) are now applied; costs of such
  prompts were undercounted before.
* A long-context price applies to prompts *over* the threshold, as the
  providers state it (it was applied from the threshold itself).
* Groq rows point to the model pages, because groq.com/pricing no longer
  lists prices; the cached-input price of `openai/gpt-oss-safeguard-20b` was
  added.

## Agent loop

* The final-answer call of `budget(final_call = TRUE)` no longer forces the
  tool on Anthropic models that reject a forced tool (Claude Opus 5.5, Sonnet
  5.5, Fable): the instruction alone asks for `final_answer`, and any other
  tool call is refused in R. Groq, OpenAI and Claude Haiku are still forced
  through the API's tool choice.
* `tool_run_r()` tells the model to use base R and the packages it was given,
  because code written for a package that is not installed costs a failed
  step (and differs from one laptop to the next).

## Activities and tools

* Activity 16 no longer depends on a folder path saved inside a kept result,
  so it runs after the course folder is moved or copied to another computer.
* `tools/run_activities.R` has a fresh mode (`fresh <- TRUE`) that records
  into a dated folder and leaves earlier recordings untouched;
  `tools/run_full_test.R` runs the smoke test and a fresh run of all
  activities in one go.
* The smoke test checks the new models, the final-answer call on every API,
  Anthropic's web search with a domain filter, and the missing-key message.
  An option set by one of its checks no longer leaks into later checks.
* `README.md` and this file were added.

# researchAgentR 0.1.0 (October 2026)

First complete version, built and tested live between October 10 and 15, 2026.

* One client for many providers (`chat()`): OpenAI-compatible Chat
  Completions, the OpenAI Responses API, the Anthropic Messages API and
  Ollama's own chat API; a model registry with prices; provider built-in
  tools and remote MCP passed through.
* Tools from R functions (`define_tool()`), a ReAct-style agent loop
  (`agent()`, `run()`) with budgets and explicit stop reasons, recovery from
  malformed tool calls, and an optional final-answer call when the budget
  runs out.
* The episode ledger: every step with tokens, cost and latency; JSON files, a
  data frame and a self-contained HTML viewer; AI-use statements drafted from
  the logs.
* Context management (window, compaction, notes, sub-agents, compression),
  memory and retrieval (a base-R vector store; embeddings from Ollama, Jina,
  Voyage, OpenAI or local Hugging Face models).
* Research tools: OpenAlex, Crossref, Semantic Scholar (optional), Europe
  PMC, arXiv, Unpaywall, NIH RePORTER, NSF awards, Grants.gov, web search and
  a page reader, a sandboxed R-code tool, and a bridge to Jev.
* Verification: `verify_citations()` and `check_claims()`.
* Multi-agent patterns, a Model Context Protocol client, an evaluation
  harness (code graders, an LLM judge with a frozen rubric, pass@k, pass^k,
  cost per success, paired bootstrap), and cost helpers (cascade, baselines,
  response cache).
* Skills, writing-style learning and manuscript review.
* Twenty activity scripts with solutions; UTF-8 for all text the package
  reads and writes.
