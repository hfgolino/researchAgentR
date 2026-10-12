# Runs the activity scripts live, each in a fresh R process, and saves a full
# transcript (the script's comments and code, then the real output of every
# expression) so the EXPECTED OUTPUT lines can be checked and filled in from
# real runs. An error in one expression is recorded and the script goes on.
#
# Run (after restarting R), from the folder that holds run_live_tests.R:
#   live_script <- "run_activities.R"; source("run_live_tests.R")
# Only some scripts (by number):
#   activities <- c("01", "02"); live_script <- "run_activities.R"; source("run_live_tests.R")
#
# Every API response is recorded in the shared folder activity_cache/ (the
# scripts call use_cache("auto")), so a second run of the same code replays
# instead of paying again, and the folder can later serve the class demos.
# The date in system prompts is pinned (activity_cache/TODAY.txt) so that the
# recordings replay on other days: in class, set the same date with
#   options(researchAgentR.today = readLines("activity_cache/TODAY.txt"))
#
# Output: live_logs/activities-<timestamp>/<script>.txt and summary.csv

stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
root <- getwd()
out_dir <- file.path(root, "live_logs", paste0("activities-", stamp))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(root, "activity_cache")
dir.create(cache, showWarnings = FALSE)
today_file <- file.path(cache, "TODAY.txt")
if (!file.exists(today_file)) writeLines(format(Sys.Date()), today_file)
today <- readLines(today_file, warn = FALSE)[1]

src <- file.path(root, "researchAgentR", "inst", "activities")
scripts <- sort(list.files(src, pattern = "^[0-9]{2}[a-z]?_.*\\.R$"))
if (exists("activities")) scripts <- scripts[sub("_.*", "", scripts) %in% activities]
if (!length(scripts)) stop("No activity scripts selected.")

# The code each child process runs: echo the source (comments included) and
# evaluate one top-level expression at a time, recording errors and going on.
child <- '
# The runner keeps its own variables in a private environment, so a script
# that defines lines, refs, res or i cannot break it (and vice versa).
local({
args <- commandArgs(trailingOnly = TRUE)
script <- args[1]; cache <- args[2]; today <- args[3]
options(width = 110, researchAgentR.cache_dir = cache, researchAgentR.today = today,
        repos = c(CRAN = "https://cloud.r-project.org"),
        warn = 1, researchAgentR.verbose = TRUE,
        # nobody is at the console: approve tool calls and checkpoints in code
        # (the logs record source = "option"), as the activities explain
        researchAgentR.approve = function(tool, arguments) TRUE)
lines <- readLines(script, warn = FALSE, encoding = "UTF-8")
exprs <- parse(script, keep.source = TRUE, encoding = "UTF-8")
refs <- attr(exprs, "srcref")
shown <- 0; n_err <- 0; t_start <- Sys.time()
show_lines <- function(to) {
  if (to <= shown) return(invisible())
  for (l in lines[(shown + 1):to]) {
    if (grepl("^\\\\s*(#|$)", l)) cat(l, "\\n", sep = "") else cat("> ", l, "\\n", sep = "")
  }
  shown <<- to
}
for (i in seq_along(exprs)) {
  show_lines(refs[[i]][3])
  res <- tryCatch(
    withCallingHandlers(withVisible(eval(exprs[[i]], envir = globalenv())),
      warning = function(w) { cat("Warning: ", conditionMessage(w), "\\n", sep = ""); invokeRestart("muffleWarning") },
      message = function(m) { cat(conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) { cat("ERROR: ", conditionMessage(e), "\\n", sep = ""); n_err <<- n_err + 1; NULL })
  if (!is.null(res) && isTRUE(res$visible)) {
    out <- tryCatch(print(res$value), error = function(e) cat("ERROR in print: ", conditionMessage(e), "\\n"))
  }
}
show_lines(length(lines))
sp <- tryCatch(researchAgentR:::.session_spend(), error = function(e) list(cost = NA, unpriced_calls = NA, live_calls = NA))
cat(sprintf("\\n@@SUMMARY errors=%d expressions=%d seconds=%.0f model_cost=%.5f live_model_calls=%s unpriced_calls=%s\\n",
            n_err, length(exprs), as.numeric(difftime(Sys.time(), t_start, units = "secs")),
            sp$cost, sp$live_calls, sp$unpriced_calls))
})
'
child_file <- file.path(out_dir, "child_runner.R")
writeLines(child, child_file)
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

rows <- list()
for (s in scripts) {
  work <- file.path(out_dir, sub("\\.R$", "", s))
  dir.create(work, showWarnings = FALSE)
  log <- file.path(out_dir, sub("\\.R$", ".txt", s))
  cat(sprintf("%-46s ", s))
  t0 <- Sys.time()
  owd <- setwd(work)
  status <- system2(rscript, c(shQuote(child_file), shQuote(file.path(src, s)), shQuote(cache), shQuote(today)),
                    stdout = log, stderr = log, timeout = 45 * 60)
  setwd(owd)
  txt <- readLines(log, warn = FALSE)
  sm <- regmatches(txt, regexpr("@@SUMMARY.*", txt))
  num <- function(key) {
    v <- regmatches(sm, regexpr(paste0(key, "=[^ ]+"), sm))
    if (length(v)) suppressWarnings(as.numeric(sub(".*=", "", v))) else NA_real_
  }
  row <- data.frame(script = s, exit = status, errors = num("errors"), expressions = num("expressions"),
                    seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))),
                    model_cost = num("model_cost"), live_model_calls = num("live_model_calls"),
                    unpriced_calls = num("unpriced_calls"))
  rows[[s]] <- row
  write.csv(do.call(rbind, rows), file.path(out_dir, "summary.csv"), row.names = FALSE)
  cat(sprintf("exit %s | errors %s | %4.0f s | model $%s\n", status, row$errors, row$seconds,
              format(row$model_cost, digits = 3)))
}
res <- do.call(rbind, rows)
cat(sprintf("\nTotal: %d scripts, %d with errors, %.0f minutes, model cost $%.4f. Transcripts in %s\n",
            nrow(res), sum(res$errors > 0 | res$exit != 0, na.rm = TRUE), sum(res$seconds) / 60,
            sum(res$model_cost, na.rm = TRUE), out_dir))
