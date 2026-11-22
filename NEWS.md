# researchAgentR 0.1.2 (November 22, 2026, evening)

Fixes from reading the transcripts of the first full live run of 0.1.1
against what each activity claims.

* **Time limits reach the web requests.** A tool with a 60-second limit once
  waited 13 minutes on a service that had stopped answering, because R's time
  limit cannot interrupt a request that waits on a server. The limit of the
  running tool call, and `max_seconds` of the episode, now also cap the
  timeout and the retries of every request made inside them. A model call cut
  by the episode's limit ends the episode with stop reason `"max_seconds"`.
* **Structured answers written as text are accepted.** A reply that holds the
  final answer as one JSON object instead of a call to `final_answer` is
  checked against the schema and accepted, in the loop and in the final-answer
  call. After a rejected tool call the model is told to use the tool named
  `final_answer` (gpt-oss models have called tools named "answer" or "json").
* **Ollama:** reasoning that a thinking-only model writes into its answer when
  thinking is switched off (ending with `</think>`) is moved to the reasoning.
* **`review_manuscript()`:** a finding from a check in code (a recomputed
  p-value, a DOI that does not resolve) is no longer merged into a model's
  comment on the same sentence, where it was lost.
* `plot_episode()` no longer prints a ggplot2 message for an episode with one
  model call; `print()` of a revision result no longer shows empty parentheses.
* **Activities.** The EXPECTED OUTPUT comments of all 20 scripts were checked
  against the transcripts of November 22 and corrected; many now give the
  values of that recorded run. Activity 05 compares the context strategies
  with one fixed-size search tool, a compaction threshold that is reached,
  and a notes scout that is told to take notes. Activities 03, 06 and 07 use
  `budget(final_call = TRUE)`. Activity 13 says when the World Bank API is
  not answering; activity 18 runs the local review only when the model is
  pulled; activities 02 and 13 explain "(invalid tool call)" steps.

Known problems found in the same reading and NOT fixed yet are listed in the
project notes (benchmark grader for NIH tasks, `fan_out()` in background
processes, `compare_single_vs_team()`, graders and typographic spaces, search
tools and `n`, NSF keyword search, Grants.gov dates, `learn_style()` labels).

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
* The final-answer call is more persistent, after what live runs showed with
  gpt-oss models on Groq: the model is told that a partial or empty answer is
  valid (a model that found little kept searching); a reply rejected because
  the model called another tool, or none, is explained to the model and asked
  for again; a forced call that comes back empty is followed by an unforced
  one; and the last of four attempts accepts the answer as JSON text, checked
  against the same schema.
* `tool_read_page()` has a second argument for the model, `find`: a cut page
  now says how to reach the rest, and `find` returns only the passages around
  a word. Before, a model that needed a number further down a long page
  requested the same page again and again (seven times in one recorded run).
  Both arguments are required (`find = ""` reads from the top), so providers
  that support strict schemas can enforce them.
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
