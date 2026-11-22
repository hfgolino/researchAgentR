# The full live test, for example a week before a course: (1) the smoke test
# of every provider, service and adapter, then (2) a FRESH recorded run of all
# activity scripts (new recordings in activity_cache_<date>/, see
# run_activities.R). Expect about an hour and well under US$1.
#
# Run (after restarting R), from the folder that holds run_live_tests.R:
#   live_script <- "run_full_test.R"; source("run_live_tests.R")
#
# Results: live_logs/<timestamp>/ (smoke test) and
#          live_logs/activities-<timestamp>/ (transcripts and summary.csv)
smoke <- try(source(file.path("researchAgentR", "tools", "live_smoke.R"), echo = FALSE), silent = TRUE)
if (inherits(smoke, "try-error")) {
  while (sink.number() > 0) sink()          # the smoke test logs through sink()
  cat("The smoke test stopped early:", conditionMessage(attr(smoke, "condition")), "\n")
}
fresh <- TRUE
source(file.path("researchAgentR", "tools", "run_activities.R"), echo = FALSE)
