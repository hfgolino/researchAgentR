# Static check of the activity scripts (no model or API calls):
#  1. every script parses;
#  2. every call to a researchAgentR function uses argument names that exist;
#  3. reports which exported functions no script demonstrates.
# Usage: Rscript tools/check_activities.R [folder]
args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[1] else "inst/activities"
suppressMessages(devtools::load_all(".", quiet = TRUE))
ns <- asNamespace("researchAgentR")
exports <- getNamespaceExports("researchAgentR")
files <- list.files(dir, "\\.R$", full.names = TRUE, recursive = TRUE)
used <- character(0)
problems <- character(0)
walk <- function(e, file) {
  if (missing(e) || is.null(e)) return(invisible(NULL))
  if (is.call(e)) {
    fn <- e[[1]]
    name <- if (is.name(fn)) as.character(fn) else if (is.call(fn) && identical(fn[[1]], as.name("::")) &&
      identical(as.character(fn[[2]]), "researchAgentR")) as.character(fn[[3]]) else NULL
    if (!is.null(name) && name %in% exports) {
      used <<- c(used, name)
      f <- get(name, envir = ns)
      if (is.function(f) && !is.primitive(f)) {
        ok <- tryCatch({ match.call(f, e, expand.dots = FALSE); TRUE },
                       error = function(err) { problems <<- c(problems, sprintf("%s: %s -> %s", basename(file), deparse(e)[1], conditionMessage(err))); FALSE })
      }
    }
    for (i in seq_along(e)) if (!is.null(e[[i]]) && !identical(e[[i]], quote(expr = ))) walk(e[[i]], file)
  } else if (is.pairlist(e)) {
    # function formals: default values may contain calls
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]], file)
  } else if (is.expression(e) || is.list(e)) {
    for (x in e) walk(x, file)
  }
}
for (f in files) {
  ex <- tryCatch(parse(f, keep.source = FALSE), error = function(err) { problems <<- c(problems, paste(basename(f), "PARSE ERROR:", conditionMessage(err))); NULL })
  if (!is.null(ex)) walk(ex, f)
}
cat("Scripts checked:", length(files), "\n")
cat("Problems:", length(problems), "\n"); if (length(problems)) cat(problems, sep = "\n")
missing <- setdiff(exports, used)
cat("\nExported functions not demonstrated in any script (", length(missing), "):\n", sep = "")
cat(sort(missing), fill = 100)
invisible(list(problems = problems, missing = missing))
