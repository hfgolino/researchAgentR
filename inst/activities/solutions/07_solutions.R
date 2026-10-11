# Solutions: Activity 07 (Design Advisor and Funding Scout)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Exercises 1 to 3 run offline (no key, no network). Exercises 4 and 5 use
# the episode ep_fund and the data frame `projects` from Activity 07.

library(researchAgentR)
use_cache("auto")


# ==============================================================================
# Exercise 1: power.prop.test() in the sandbox, checked in the session
# ==============================================================================

code <- "print(power.prop.test(p1 = 0.30, p2 = 0.20, sig.level = 0.05, power = 0.80)$n, digits = 15)"
r <- run_r_code(code)
r
n_sandbox <- as.numeric(sub("^\\[1\\]\\s*", "", r$output))
n_session <- power.prop.test(p1 = 0.30, p2 = 0.20, sig.level = 0.05, power = 0.80)$n
c(sandbox = n_sandbox, session = n_session)
isTRUE(all.equal(n_sandbox, n_session))
# n is PER GROUP; the total is twice the rounded-up value:
2 * ceiling(n_session)
# EXPECTED OUTPUT: the same n from both, TRUE, and the total sample size.


# ==============================================================================
# Exercise 2: the working folder is not a jail
# ==============================================================================

r2 <- run_r_code(paste(
  "f <- file.path(tempdir(), 'outside.txt')",
  "writeLines('written outside the working folder', f)",
  "cat('working folder:', getwd(), '\\n')",
  "cat('file written to:', f, '\\n')",
  "file.exists(f)", sep = "\n"))
r2
basename(r2$files)   # the file is NOT listed: it is outside the working folder
# EXPECTED OUTPUT: two different paths, TRUE, and character(0). The code can
# write anywhere your user account can write. The working folder only keeps
# the agent's files together; it does not confine them.


# ==============================================================================
# Exercise 3: approval before every call, decided by a rule in a script
# ==============================================================================

r_tool_approval <- tool_run_r(approval = TRUE)
r_tool_approval            # the print shows [ needs approval ]

options(researchAgentR.approve = function(tool, arguments) {
  code <- if (is.null(arguments$code)) "" else arguments$code
  !grepl("system\\(|unlink\\(|file\\.remove\\(", code)
})
ok <- call_tool("run_r", list(r_tool_approval), arguments = list(code = "sqrt(16)"))
cat(ok$output)
refused <- call_tool("run_r", list(r_tool_approval), arguments = list(code = "unlink('data.csv')"))
cat(refused$output)
refused$is_error
options(researchAgentR.approve = NULL)   # back to asking at the console
# EXPECTED OUTPUT: "[1] 4" for the first call; for the second, "A person
# declined this call to run_r. ..." and TRUE. In an interactive session,
# without the option, the console asks you before each call.


# ==============================================================================
# Exercise 4: one row per tool call of the funding scout (needs ep_fund)
# ==============================================================================

tool_steps <- Filter(function(s) s$type == "tool", ep_fund$steps)
calls <- data.frame(
  tool = vapply(tool_steps, function(s) s$tool, character(1)),
  arguments = vapply(tool_steps, function(s) s$arguments, character(1)),
  is_error = vapply(tool_steps, function(s) isTRUE(s$is_error), logical(1)),
  nothing_found = vapply(tool_steps, function(s) grepl("No matching", s$result), logical(1)),
  stringsAsFactors = FALSE)
calls
calls[calls$nothing_found, c("tool", "arguments")]
# EXPECTED OUTPUT: one row per call with the JSON arguments the model sent;
# the second table lists the searches that returned nothing (often searches
# with many words, because NIH requires all of them).


# ==============================================================================
# Exercise 5: compare the agent's NIH titles with the official records
# ==============================================================================

nih_ids <- projects$project_id[projects$source == "NIH"]
official <- do.call(rbind, lapply(nih_ids, function(id)
  search_nih_projects(project_nums = list(id), n = 1)))
comparison <- merge(
  projects[projects$source == "NIH", c("project_id", "title")],
  official[, c("project_num", "title")],
  by.x = "project_id", by.y = "project_num", all.x = TRUE,
  suffixes = c("_agent", "_reporter"))
comparison$same_title <- tolower(comparison$title_agent) == tolower(comparison$title_reporter)
comparison
# EXPECTED OUTPUT: one row per NIH project the agent reported. same_title is
# TRUE when the agent copied the title exactly; NA in title_reporter means
# RePORTER has no project with that number (a fabricated or mistyped id).
# Requests are spaced one second apart (RePORTER etiquette).
