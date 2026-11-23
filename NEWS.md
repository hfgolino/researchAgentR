# researchAgentR 0.1.4 (November 22, 2026, late night)

The open issues from reading the transcripts of the first full live run.
None of these changes has run live yet; the checks marked (live check) are
in `tools/live_smoke.R`.

## Evaluation

* **The NIH benchmark tasks can pass.** The grader searched the project
  number as free text and found nothing, so success in activities 09, 10 and
  11 was capped at 0.75. It now looks the project up by its number:
  `search_nih_projects()` has a `project_nums` argument, where `*` stands for
  any characters (live check: a project returned by a search must pass, a
  number that does not exist must not).
* **Graders see plain punctuation.** Models write narrow and non-breaking
  spaces, curly quotes and non-breaking hyphens; a grader pattern such as
  `d = 0.71` did not match them (activity 16: every answer failed), and a DOI
  written with non-breaking hyphens was not recognized. Every grader now
  replaces typographic punctuation in the answer first. The new
  `plain_punctuation()` does the same for a comparison of one's own;
  `check_claims()`, the review tools and `learn_style()` use it for their
  quotation checks, so a quotation is no longer "not found" because of a
  hyphen. `research_tasks` was rebuilt with these graders.
* `paired_bootstrap()` names the two solvers from their own rows (the header
  named one solver twice after subsetting a set of results).
* `compare_single_vs_team()`: a task given twice is two independent trials
  (the second was replayed from the cache at cost 0); new columns
  `worker_tokens` and `total_tokens` (the team's `tokens` counted the lead
  only).
* `plot_frontier()`: no ggplot2 message for a front of one solver.

## Agents and tools

* `fan_out()` in background processes: the workspace objects that the tools
  use (a data frame a tool reads, a helper it calls) are copied to the
  background processes, and the attached packages are attached there. In
  activity 08, 7 of 8 workers had failed because the tool could not find
  `docs`. The summary line counts runs with and without an answer (it said
  "0 failed" for runs that stopped without one).
* Literature search tools: the researcher's `n` is the limit of a call. The
  model may ask for fewer records, no longer for up to 50.
* `search_nsf_awards()`: NSF returns awards that contain ANY of several
  words, in no order of relevance (10,000 awards for three words, the first
  ten unrelated). With the new `match = "all"` (the default) the function
  asks NSF for the rarest word and keeps the awards that contain every word
  (live check). `match = "any"` and `"phrase"` send the words as before or in
  quotes.
* `grants_gov_opportunity()`: the closing date was read from a remark field
  that is usually empty. Dates now come from the date fields, as
  `yyyy-mm-dd`; a forecasted opportunity gives the agency's estimated dates
  and `estimated = TRUE`. Checked against the three recorded responses.
* `dedupe_records()`: new column `n_databases` (how many databases found a
  record); `n_found` counts merged records and was read as databases.
* `route(method = "embedding")`: the reason gives the similarity and the
  weight (the number a threshold is compared with).

## Writing tools

* `write_in_style()`: told to use only the content of the brief, and its
  text is checked in code: numbers and figure or table references that are
  not in the brief are listed in `attr(x, "added")` and reported. In the run
  of November 22 the draft had reused two fit values of the source paper and
  pointed to a "Figure 3".
* `learn_style()` no longer removes the names of its own measures
  ("we/our/us") from the guide as quotations not found.
* `validate_skill()`: a file path written as plain text in the body counts
  as a reference (the style skill showed "0 reference(s)"); new rule
  "bundled reference files are named in the body".

## Activities

* 03: `n_databases`. 07: NSF and Grants.gov outputs explained. 08:
  `total_tokens` in the single-versus-team comparison; fan-out comments.
  08b: an unknown tool is a protocol error in the specification and a tool
  error on DeepWiki; the code handles both. 10: the cascade is compared with
  the two fixed models on the same two tools (one more run, "strong, 2
  tools"); its runtime check asks RePORTER whether the project exists in
  2025. 12: the router did not pay with Jev as second screener, and the
  comments say so. 15: weight and similarity. 16: the typography trap, shown
  with `plain_punctuation()`; the grader accepts "95 % CI". 17: the
  added-content check.
* Recorded-run sentences whose values these changes make out of date were
  removed; they are written again after the next full run.

# researchAgentR 0.1.3 (November 22, 2026, night)

* **Fix: time limits with httr2 1.3.0.** The time limit added in 0.1.2 gave
  httr2 the time left as a decimal number of seconds (299.99). httr2 1.3.0
  accepts only a whole number of seconds for retries, so every live request
  made inside an episode or a tool call stopped before it was sent, with
  "`max_seconds` must be a whole number". The time left is now rounded down
  to whole seconds. The unit tests passed because they ran on httr2 1.0.0,
  which does not check this; they now run on httr2 1.0.0 and 1.3.0, and one
  test really sends a request under a time limit.
* A request that is sent once only (`ollama_status()`, closing an MCP
  session, Semantic Scholar without a key) keeps its own number of tries
  under a time limit; 0.1.2 gave it the default of four.
* `keep()` does not save a run that the API stopped before any work was done:
  besides a single episode (as before), a list whose episodes all ended with
  `api_error` (a team run kept with its workers) and an evaluation in which
  every trial did. The next call then tries again instead of loading a failure.
* `run(verbose = FALSE)` no longer prints the "Stopped: ..." line.

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
