# Documenting AI use from the logs. ai_use_statement() drafts a disclosure
# from what the episode ledger recorded, and nothing else: models, dates,
# calls, tools, tokens, cost, cache replays and verification calls. What only
# the author can know (the role of AI in the study, how outputs were reviewed,
# who is responsible) is left as [placeholders in square brackets].

utils::globalVariables(".data")

# Tool names that point to a known external service. The statement says the
# names "indicate" these services; it does not claim more than the names show.
.known_services <- c(
  openalex = "OpenAlex", crossref = "Crossref", "semantic_?scholar" = "Semantic Scholar",
  "europe_?pmc" = "Europe PMC", pubmed = "PubMed", arxiv = "arXiv", unpaywall = "Unpaywall",
  reporter = "NIH RePORTER", "(^|_)nsf" = "NSF Award Search", "grants_?gov" = "Grants.gov",
  tavily = "Tavily", brave = "Brave Search", searxng = "SearXNG", "(^|_)jev" = "TypeSafe Jev",
  browser_search = "Groq browser_search", code_interpreter = "Groq code_interpreter",
  mcp = "an MCP server"
)

# Tool names that count as verification steps.
.verification_pattern <- "verif|check_claim|claim_check|retract"

.services_from <- function(names) {
  hits <- vapply(names(.known_services), function(p) any(grepl(p, tolower(names))), logical(1))
  unique(unname(.known_services[hits]))
}

.count_table <- function(x) {
  if (!length(x)) return(stats::setNames(integer(0), character(0)))
  t <- table(x)
  stats::setNames(as.integer(t), names(t))[order(-as.integer(t), names(t))]
}

.count_text <- function(counts) {
  paste(sprintf("%s (%d)", names(counts), counts), collapse = ", ")
}

.date_of <- function(x) {
  x <- x[nzchar(x)]
  if (!length(x)) return(character(0))
  substr(x, 1, 10)
}

# Everything the statement says, computed from the episodes in one place.
.ai_use_facts <- function(eps) {
  eps <- .with_children(eps)     # workers of a team are described too
  nested <- attr(eps, "nested")  # ... but their cost is already in the lead's
  eps <- lapply(eps, .lens_episode)
  steps <- unlist(lapply(eps, `[[`, "steps"), recursive = FALSE)
  model_steps <- Filter(function(s) s$type == "model", steps)
  tool_steps <- Filter(function(s) s$type == "tool", steps)
  totals <- lapply(eps, .totals)
  sum_t <- function(f) sum(vapply(totals, function(t) as.numeric(t[[f]]), numeric(1)))
  costs <- vapply(totals, function(t) t$cost, numeric(1))[!nested]

  step_models <- vapply(model_steps, `[[`, character(1), "model")
  step_models[!nzchar(step_models)] <- "(model not recorded)"
  model_ids <- unique(c(step_models, if (!length(step_models)) vapply(eps, `[[`, character(1), "model")))
  provider_of <- function(id) ifelse(grepl(":", id, fixed = TRUE), sub(":.*$", "", id), NA_character_)
  local_providers <- providers()$provider[providers()$local]

  tools <- vapply(tool_steps, `[[`, character(1), "tool")
  tool_err <- vapply(tool_steps, `[[`, logical(1), "is_error")
  builtins <- unlist(lapply(model_steps, `[[`, "executed_tools"))
  verif <- tools[grepl(.verification_pattern, tools)]
  children <- unique(stats::na.omit(vapply(tool_steps, `[[`, character(1), "child_episode")))

  dates <- .date_of(vapply(eps, `[[`, character(1), "started"))
  if (!length(dates)) dates <- .date_of(vapply(steps, `[[`, character(1), "time"))
  pkg <- unique(.chrv(lapply(eps, `[[`, "package_version")))
  rver <- unique(.chrv(lapply(eps, `[[`, "r_version")))

  list(
    n_episodes = length(eps), models = model_ids,
    providers = unique(stats::na.omit(provider_of(model_ids))),
    calls_by_model = .count_table(step_models),
    local_calls = sum(provider_of(step_models) %in% local_providers),
    model_calls = length(model_steps), tool_calls = length(tool_steps),
    tool_counts = .count_table(tools), tool_errors = sum(tool_err),
    error_counts = .count_table(tools[tool_err]),
    builtins = .count_table(builtins), services = .services_from(c(tools, builtins)),
    verification = .count_table(verif), children = children,
    tokens = sum_t("tokens"), cache_read = sum_t("cache_read"),
    cost = if (anyNA(costs)) NA_real_ else sum(costs),
    replayed = sum(vapply(model_steps, `[[`, logical(1), "cached")),
    stop_reasons = .count_table(vapply(eps, function(e) .chr1(e$stop_reason, "(not recorded)"), character(1))),
    dates = if (length(dates)) range(dates) else c(NA_character_, NA_character_),
    package_version = pkg, r_version = rver
  )
}

.ai_use_sentences <- function(f, purpose, human_review) {
  when <- if (is.na(f$dates[1])) "[date not recorded in the logs]" else
    if (f$dates[1] == f$dates[2]) paste("on", f$dates[1]) else paste("between", f$dates[1], "and", f$dates[2])
  software <- if (length(f$package_version)) {
    sprintf(" with the researchAgentR package (version %s)%s", paste(f$package_version, collapse = ", "),
            if (length(f$r_version)) paste0(" in ", paste(f$r_version, collapse = ", ")) else "")
  } else ""
  models <- if (length(f$calls_by_model)) {
    paste(sprintf("%s (%d call%s)", names(f$calls_by_model), f$calls_by_model,
                  ifelse(f$calls_by_model == 1, "", "s")), collapse = ", ")
  } else paste(f$models, collapse = ", ")
  where <- if (f$model_calls == 0) "" else if (f$local_calls == f$model_calls) {
    " All model calls went to models running locally, so prompts did not leave the computer."
  } else if (f$local_calls == 0) {
    sprintf(" All model calls were sent to cloud providers (%s).", paste(f$providers, collapse = ", "))
  } else {
    sprintf(" Of these, %d model calls went to local models and %d to cloud providers.",
            f$local_calls, f$model_calls - f$local_calls)
  }
  tools <- if (f$tool_calls) {
    sprintf(" The agents made %d tool call%s: %s%s.", f$tool_calls, if (f$tool_calls == 1) "" else "s",
            .count_text(f$tool_counts),
            if (f$tool_errors) sprintf("; %d returned an error to the model", f$tool_errors) else "")
  } else " No tool calls were logged."
  builtins <- if (length(f$builtins)) sprintf(" Provider built-in tools ran on the provider's servers: %s.",
                                              .count_text(f$builtins)) else ""
  services <- if (length(f$services)) sprintf(" Tool names indicate the following external services: %s.",
                                              paste(f$services, collapse = ", ")) else ""
  cost <- if (is.na(f$cost)) {
    " The cost could not be computed for every call, because at least one model has no price in the package registry."
  } else sprintf(paste(" The estimated cost at list price was %s US dollars (list prices from the package",
                       "registry; free tiers and local models may have incurred no actual charge)."),
                 sub("^\\$", "", .fmt_usd(f$cost)))
  replay <- if (f$replayed) {
    sprintf(" %d of the %d model calls %s replayed from a local response cache rather than sent to the provider.",
            f$replayed, f$model_calls, if (f$replayed == 1) "was" else "were")
  } else if (f$model_calls) " No model call was replayed from a local response cache." else ""
  verification <- if (length(f$verification)) {
    sprintf(" The logs record %d call%s to verification tools: %s.", sum(f$verification),
            if (sum(f$verification) == 1) "" else "s", .count_text(f$verification))
  } else paste(" The logs record no calls to verification tools (such as verify_citations or check_claims);",
               "[describe any verification performed outside the logged episodes, or state that none was performed].")
  stops <- sprintf(" Stop reasons: %s.", .count_text(f$stop_reasons))
  children <- if (length(f$children)) sprintf(" Tool calls started %d sub-agent episode%s, logged as separate episodes.",
                                               length(f$children), if (length(f$children) == 1) "" else "s") else ""
  review <- if (is.null(human_review)) {
    "[Describe the human review of agent outputs: who checked what, how many items, and with what result.]"
  } else paste(human_review, collapse = " ")
  list(
    purpose = sprintf("Large language model (LLM) agents were used for the following purpose: %s.", sub("[.]\\s*$", "", purpose)),
    run = sprintf("The agents were run%s %s. The logs contain %d episode%s with %d model call%s to %s.%s",
                  software, when, f$n_episodes, if (f$n_episodes == 1) "" else "s", f$model_calls,
                  if (f$model_calls == 1) "" else "s", models, where),
    tools = trimws(paste0(tools, builtins, services, children)),
    usage = trimws(sprintf(" The logged model calls used %s billed tokens (%s of them read from provider-side prompt caches).%s%s",
                           .fmt_int(f$tokens), .fmt_int(f$cache_read), cost, replay)),
    checks = trimws(paste0(verification, stops)),
    review = review,
    role = "[Describe the role of AI in the study: which outputs were used, how, and which decisions remained with the authors.]",
    responsibility = "[State that the authors reviewed the agent outputs and take full responsibility for the content of the work.]",
    availability = "[State where the episode logs (JSON) are available, for example in the supplementary materials or a repository.]"
  )
}

.md_cell <- function(x) gsub("\\s+", " ", gsub("|", "\\|", as.character(x), fixed = TRUE))

.md_table <- function(df) {
  df[] <- lapply(df, .md_cell)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

.ai_use_appendix <- function(eps, f, s) {
  ep_rows <- do.call(rbind, lapply(eps, function(e) {
    t <- .totals(e)
    data.frame(Episode = e$id, Agent = e$agent, Model = e$model, Date = substr(e$started, 1, 10),
               `Stop reason` = .chr1(e$stop_reason, "NA"), `Model calls` = t$model_calls,
               `Tool calls` = t$tool_calls, Tokens = .fmt_int(t$tokens), Cost = .fmt_usd(t$cost),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  per_episode <- unlist(lapply(eps, function(e) {
    rows <- lapply(e$steps, function(st) {
      m <- st$type == "model"
      data.frame(Step = as.integer(st$step), Type = st$type, `Model or tool` = if (m) st$model else st$tool,
                 `Tokens in` = if (m) .fmt_int(st$input + st$cache_read + st$cache_write) else "",
                 `Tokens out` = if (m) .fmt_int(st$output) else "", Cost = .fmt_usd(st$cost),
                 Seconds = if (is.na(st$latency)) "NA" else sprintf("%.1f", st$latency),
                 Error = if (st$is_error) "yes" else "", check.names = FALSE, stringsAsFactors = FALSE)
    })
    c(sprintf("### Episode %s (%s)", e$id, e$agent), "",
      sprintf("Task: %s", .shorten(e$task, 300)), "",
      if (length(rows)) .md_table(do.call(rbind, rows)) else "No steps were logged.", "")
  }))
  tools_tab <- if (length(f$tool_counts)) {
    errors <- unname(f$error_counts[names(f$tool_counts)])
    .md_table(data.frame(Tool = names(f$tool_counts), Calls = unname(f$tool_counts),
                         Errors = ifelse(is.na(errors), 0L, errors),
                         check.names = FALSE, stringsAsFactors = FALSE))
  } else "No tool calls were logged."
  c("# Appendix: Use of AI agents", "",
    "## Purpose", "", s$purpose, "",
    "## What the logs record", "", s$run, "", s$tools, "", s$usage, "", s$checks, "",
    "## Tool calls", "", tools_tab, "",
    "## Episodes", "", .md_table(ep_rows), "",
    per_episode,
    "## Human review", "", s$review, "",
    "## Statements only the authors can make", "",
    paste("-", s$role), paste("-", s$responsibility),
    "- [State whether any confidential, identifiable or participant data were sent to cloud providers, and under which terms.]",
    paste("-", s$availability), "",
    "*Drafted from the episode logs with researchAgentR::ai_use_statement(). Edit every bracketed placeholder before use.*")
}

#' Draft an AI-use statement from episode logs
#'
#' Writes a disclosure of AI use from what the episode ledger recorded: the
#' models and providers (with model IDs), dates, number of episodes, model
#' calls, tool calls and the tools used (and the external services their
#' names indicate), tokens and cost at list price, whether calls were
#' replayed from the response cache, and the verification steps recorded
#' (calls to tools such as `verify_citations` or `check_claims`). Statements
#' the logs cannot support are left as placeholders in square brackets for
#' the author: the role of AI in the study, the human review, and
#' responsibility for the content.
#'
#' The draft never claims more than the logs show. Work done outside the
#' logged episodes (for example, manual checks) must be added by the author.
#'
#' @param episodes An episode, a list of episodes, a vector of saved episode
#'   files, or a folder of saved episodes.
#' @param purpose What the agents were used for, as a phrase (for example
#'   `"screening titles and abstracts for a systematic review"`).
#' @param human_review Optional text describing the human review of the
#'   outputs. `NULL` leaves a placeholder.
#' @param style `"short"` for one paragraph for a methods section, or
#'   `"appendix"` for a markdown appendix with a table per episode.
#' @param file Optional path; the statement is also written there.
#' @return A character string of class `"ai_use_statement"`, with the facts
#'   it was built from in the attribute `"facts"`.
#' @seealso [course_template()] for the AI-use statement template and the
#'   agent verification checklist.
#' @examples
#' \dontrun{
#' s <- ai_use_statement(list(ep1, ep2),
#'                       purpose = "screening 200 abstracts against four inclusion criteria")
#' s
#' ai_use_statement("episodes", purpose = "locating measurement instruments",
#'                  style = "appendix", file = "appendix_ai_use.md")
#' }
#' @export
ai_use_statement <- function(episodes, purpose, human_review = NULL,
                             style = c("short", "appendix"), file = NULL) {
  style <- match.arg(style)
  if (missing(purpose) || !is.character(purpose) || !nzchar(trimws(purpose[1]))) {
    .stop("Say what the agents were used for in `purpose`, for example ",
          "purpose = \"screening titles and abstracts\".")
  }
  eps <- .lens_episodes(episodes)
  if (!length(eps)) .stop("No episodes were given.")
  f <- .ai_use_facts(eps)
  s <- .ai_use_sentences(f, purpose[1], human_review)
  text <- if (style == "short") {
    paste(s$purpose, s$run, s$tools, s$usage, s$checks, s$review, s$role, s$responsibility, s$availability)
  } else {
    paste(.ai_use_appendix(eps, f, s), collapse = "\n")
  }
  if (!is.null(file)) writeLines(enc2utf8(text), file, useBytes = TRUE)
  structure(text, class = "ai_use_statement", facts = f, style = style)
}

#' @export
print.ai_use_statement <- function(x, ...) {
  if (identical(attr(x, "style"), "short")) {
    cat(strwrap(unclass(x), width = getOption("width", 80) - 2), sep = "\n")
  } else {
    cat(unclass(x), "\n")
  }
  n <- lengths(regmatches(x, gregexpr("\\[[^]]+\\]", x)))
  if (n) cat(sprintf("\n(%d placeholder%s in [square brackets] to complete before use.)\n", n, if (n == 1) "" else "s"))
  invisible(x)
}

#' Course templates: AI-use statement, verification checklist, capstone rubric
#'
#' The package ships three templates in markdown:
#' * `"ai_use_statement"`: a disclosure template to complete with
#'   [ai_use_statement()].
#' * `"agent_verification_checklist"`: checks to run before trusting an
#'   agent result.
#' * `"capstone_rubric"`: criteria for the capstone team proposal brief.
#'
#' @param name The template name (with or without `_template` and `.md`).
#'   `NULL` lists the available templates.
#' @param dest Optional folder (or file path ending in `.md`) to copy the
#'   template to, so it can be edited.
#' @param overwrite Overwrite an existing copy?
#' @return The path of the template (or of the copy when `dest` is given);
#'   with `name = NULL`, the template names.
#' @examples
#' course_template()
#' readLines(course_template("agent_verification_checklist"), n = 5)
#' copy <- course_template("capstone_rubric", dest = tempdir())
#' @export
course_template <- function(name = NULL, dest = NULL, overwrite = FALSE) {
  dir <- system.file("templates", package = "researchAgentR")
  if (!nzchar(dir)) .stop("The templates folder is missing; reinstall researchAgentR.")
  files <- list.files(dir, "\\.md$")
  available <- sub("(_template)?\\.md$", "", files)
  if (is.null(name)) return(available)
  key <- sub("(_template)?(\\.md)?$", "", name)
  i <- match(key, available)
  if (is.na(i)) .stop("Unknown template '", name, "'. Available: ", paste(available, collapse = ", "), ".")
  path <- file.path(dir, files[i])
  if (is.null(dest)) return(path)
  target <- if (grepl("\\.md$", dest)) dest else file.path(dest, files[i])
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(target) && !overwrite) .stop(target, " exists; use overwrite = TRUE to replace it.")
  file.copy(path, target, overwrite = TRUE)
  .msg("Copied the template to ", target)
  target
}
