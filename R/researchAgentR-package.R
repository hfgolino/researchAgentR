#' researchAgentR: LLM Agents for Research, Built from First Principles in R
#'
#' The package implements research agents in plain R so that every part can
#' be read and changed: one client for many model providers ([chat()]),
#' tools from R functions ([define_tool()]), a short agent loop ([agent()],
#' [run()]) with budgets ([budget()]), an episode ledger for every run
#' ([view_episode()], [episodes_df()]), research tools, verification,
#' evaluation and cost optimization.
#'
#' Keys are read from `~/.Renviron` (see [set_keys]). Check a setup with
#' [verify_setup()]. Options (set with `options()`):
#' * `researchAgentR.verbose`: print progress (default `TRUE`).
#' * `researchAgentR.timeout`: HTTP timeout in seconds (default 120).
#' * `researchAgentR.max_tries`: attempts per request on 429 and 5xx errors (default 4).
#' * `researchAgentR.rpm.<provider>`: requests per minute for a provider, for
#'   example `researchAgentR.rpm.groq = 25` to stay under a free-tier limit.
#' * `researchAgentR.prompt_caching`: Anthropic prompt caching (default `TRUE`).
#' * `researchAgentR.approve`: a function `(tool, arguments)` returning
#'   `TRUE` or `FALSE`, used for tools that need approval in scripts.
#'
#' @keywords internal
"_PACKAGE"
