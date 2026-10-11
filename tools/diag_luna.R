# Diagnostic: does the WORDING of an agent's instructions and output schema
# decide whether gpt-6-luna reports the tool's result?
#
# First diagnostic (2026-10-11, live_logs/diag-20261011-183831): with the
# instruction "Use the word_count tool to count words. Never count by
# yourself." and a final-answer field "n_words", gpt-6-luna reported 11 in only
# 4 of 20 tries, whatever the API settings. Its reasoning summary showed why:
# it took the task to be counting the words of ITS OWN ANSWER ("call the tool
# with the actual answer text"). Claude Haiku once called final_answer in the
# same turn as word_count, guessing before the result came back (the loop now
# refuses that).
#
# Second diagnostic (live_logs/diag-20261011-184659), five tries each:
#   luna_ambiguous 1/5, luna_clear 2/5, luna_chat_none 5/5, haiku_clear 5/5,
#   gptoss120_ambiguous 5/5, gptoss120_clear 5/5.
# The trajectories explain luna_clear: in all 5 tries luna reported exactly
# what the tool returned, but in 3 it passed the WHOLE question ("How many
# words are in: '...'?", 16 words) as "the user's text". So the clear
# wording fixed faithfulness to the tool; what remained was an ambiguous
# argument. With reasoning off (Chat Completions, effort "none") luna passed
# the quoted text every time. Lesson for the course: grade the trajectory,
# not only the final number.
#
# This script compares the old wording with a clear one, five tries each.
# Run (after restarting R):  live_script <- "diag_luna.R"; source("run_live_tests.R")
# Output: live_logs/diag-<timestamp>/diag.csv, episodes.rds

library(researchAgentR)
`%||%` <- function(a, b) if (is.null(a)) b else a
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
out_dir <- file.path(getwd(), "live_logs", paste0("diag-", stamp))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
options(researchAgentR.verbose = FALSE)

word_count <- function(text) length(strsplit(trimws(text), "\\s+")[[1]])
tool <- define_tool(word_count, "Count the words in a text.",
                    args = list(text = arg_string("The text to count")))
sentence <- "Exploratory graph analysis estimates the number of dimensions in multivariate data"
q <- paste0("How many words are in: '", sentence, "'?")

wordings <- list(
  ambiguous = list(
    instructions = "Use the word_count tool to count words. Never count by yourself.",
    schema = list(n_words = arg_integer("The number of words returned by the tool"))),
  clear = list(
    instructions = paste("Count the words in the user's text with the word_count tool.",
                         "Then call final_answer with the number the tool returned."),
    schema = list(count = arg_integer("The number that the word_count tool returned for the user's text")))
)
register_provider("openai_chat", "https://api.openai.com/v1", api = "chat", key_env = "OPENAI_API_KEY")

variants <- list(
  luna_ambiguous      = list(model = "openai:gpt-6-luna", wording = "ambiguous"),
  luna_clear          = list(model = "openai:gpt-6-luna", wording = "clear"),
  # Chat Completions accepts function tools for gpt-6-luna only with reasoning off
  luna_chat_none      = list(model = "openai_chat:gpt-6-luna", wording = "clear", effort = "none"),
  haiku_clear         = list(model = "anthropic:claude-haiku-5-5", wording = "clear"),
  gptoss120_ambiguous = list(model = "groq:openai/gpt-oss-120b", wording = "ambiguous"),
  gptoss120_clear     = list(model = "groq:openai/gpt-oss-120b", wording = "clear")
)

rows <- list(); episodes <- list()
for (v in names(variants)) {
  spec <- variants[[v]]
  w <- wordings[[spec$wording]]
  a <- agent("counter", w$instructions, model = spec$model, tools = list(tool),
             effort = spec$effort, output_schema = w$schema)
  for (k in 1:5) {
    id <- paste0(v, "_", k)
    ep <- tryCatch(run(a, q, budget(max_steps = 6)), error = function(e) e)
    if (inherits(ep, "error")) {
      rows[[id]] <- data.frame(variant = v, try = k, answer = NA, correct = FALSE,
                               early_final = NA, stop = substr(conditionMessage(ep), 1, 200))
    } else {
      episodes[[id]] <- ep
      ans <- suppressWarnings(as.integer(unlist(ep$answer)[1]))
      if (length(ans) != 1) ans <- NA_integer_
      early <- any(vapply(ep$steps, function(s) isTRUE(s$type == "tool" && s$is_error &&
                                                       grepl("not accepted", s$result)), logical(1)))
      rows[[id]] <- data.frame(variant = v, try = k, answer = ans, correct = isTRUE(ans == 11L),
                               early_final = early, stop = substr(ep$stop_reason, 1, 200))
    }
    write.csv(do.call(rbind, rows), file.path(out_dir, "diag.csv"), row.names = FALSE)
    cat(sprintf("%-20s try %d: answer %s\n", v, k, rows[[id]]$answer))
  }
}

res <- do.call(rbind, rows)
cat("\nCorrect answers (out of 5):\n")
print(tapply(res$correct, res$variant, sum)[names(variants)])
saveRDS(episodes, file.path(out_dir, "episodes.rds"))
cat("\nSaved in", out_dir, "\n")
