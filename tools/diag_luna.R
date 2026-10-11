# Diagnostic: why does openai:gpt-6-luna report the wrong word count after the
# word_count tool returned the right one? Five tries for each variant; every
# episode and the exact request body of each final step are saved.
#
# Run (after restarting R):  live_script <- "diag_luna.R"; source("run_live_tests.R")
# Output: live_logs/diag-<timestamp>/diag.csv, episodes.rds, bodies.rds

library(researchAgentR)
`%||%` <- function(a, b) if (is.null(a)) b else a
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
out_dir <- file.path(getwd(), "live_logs", paste0("diag-", stamp))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
options(researchAgentR.verbose = FALSE)
stopifnot(nzchar(Sys.getenv("OPENAI_API_KEY")))

word_count <- function(text) length(strsplit(trimws(text), "\\s+")[[1]])
sentence <- "Exploratory graph analysis estimates the number of dimensions in multivariate data"
q <- paste0("How many words are in: '", sentence, "'?")
counter <- function(model, effort = NULL, untrusted = TRUE) {
  tool <- define_tool(word_count, "Count the words in a text.",
                      args = list(text = arg_string("The text to count")), untrusted = untrusted)
  agent("counter", "Use the word_count tool to count words. Never count by yourself.",
        model = model, tools = list(tool), effort = effort,
        output_schema = list(n_words = arg_integer("The number of words returned by the tool")))
}
register_provider("openai_chat", "https://api.openai.com/v1", api = "chat", key_env = "OPENAI_API_KEY")

variants <- list(
  responses_replay   = list(model = "openai:gpt-6-luna"),
  responses_noreplay = list(model = "openai:gpt-6-luna", replay = FALSE),
  chat_completions   = list(model = "openai_chat:gpt-6-luna"),
  responses_high     = list(model = "openai:gpt-6-luna", effort = "high"),
  responses_plain    = list(model = "openai:gpt-6-luna", untrusted = FALSE),
  other_model_check  = list(model = "anthropic:claude-haiku-5-5")
)

rows <- list(); episodes <- list(); bodies <- list()
for (v in names(variants)) {
  spec <- variants[[v]]
  old <- options(researchAgentR.replay_native = spec$replay %||% TRUE)
  for (k in 1:5) {
    ep <- tryCatch(run(counter(spec$model, spec$effort, spec$untrusted %||% TRUE), q,
                       budget(max_steps = 6)), error = function(e) e)
    id <- paste0(v, "_", k)
    if (inherits(ep, "error")) {
      rows[[id]] <- data.frame(variant = v, try = k, answer = NA, correct = NA,
                               stop = conditionMessage(ep))
      next
    }
    episodes[[id]] <- ep
    bodies[[id]] <- researchAgentR:::.pkg_env$last_body
    ans <- suppressWarnings(as.integer(ep$answer$n_words))
    if (length(ans) != 1) ans <- NA_integer_   # no final answer (for example an API error)
    rows[[id]] <- data.frame(variant = v, try = k, answer = ans, correct = isTRUE(ans == 11L),
                             stop = substr(ep$stop_reason, 1, 200))
    write.csv(do.call(rbind, rows), file.path(out_dir, "diag.csv"), row.names = FALSE)
    cat(sprintf("%-20s try %d: answer %s (%s)\n", v, k, ans, ep$stop_reason))
  }
  options(old)
}

# One manual trajectory with the reasoning summary requested.
a <- counter("openai:gpt-6-luna")
tools <- researchAgentR:::.agent_tools(a)
msgs <- list(list(role = "system", content = researchAgentR:::.system_prompt(a)),
             list(role = "user", content = q))
summary_steps <- list()
for (step in 1:3) {
  r <- chat(msgs, model = a$model, tools = tools, max_tokens = 2000,
            reasoning = list(effort = "medium", summary = "auto"))
  summary_steps[[step]] <- list(body = researchAgentR:::.pkg_env$last_body, reasoning = r$reasoning,
                                calls = vapply(r$tool_calls, function(t) paste0(t$name, t$arguments_raw), ""))
  cat("\nStep", step, "calls:", summary_steps[[step]]$calls, "\nReasoning summary:", r$reasoning, "\n")
  if (!length(r$tool_calls)) break
  msgs <- c(msgs, list(researchAgentR:::.assistant_message(r)))
  done <- FALSE
  for (tc in r$tool_calls) {
    res <- call_tool(tc, tools)
    msgs <- c(msgs, list(list(role = "tool", tool_call_id = tc$id, name = tc$name, content = res$output)))
    if (tc$name == "final_answer") done <- TRUE
  }
  if (done) break
}

res <- do.call(rbind, rows)
print(aggregate(correct ~ variant, res, function(x) paste0(sum(x, na.rm = TRUE), "/", length(x))))
write.csv(res, file.path(out_dir, "diag.csv"), row.names = FALSE)
saveRDS(episodes, file.path(out_dir, "episodes.rds"))
saveRDS(list(bodies = bodies, summary_steps = summary_steps), file.path(out_dir, "bodies.rds"))
cat("\nSaved in", out_dir, "\n")
