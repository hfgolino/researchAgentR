# Solutions: Activity 05, Context Management and Compression
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Run the activity first (SETUP to Step 3): these solutions use lit_task,
# expected_dois, scout_instructions, make_scout(), lit_budget, SCOUT_MODEL,
# toy, tokens_sent(), call_ends, skills_dir and draft.

library(researchAgentR)
use_cache("auto")


# ==============================================================================
# EXERCISE 1: THREE TRIALS PER STRATEGY WITH run_eval()
# ==============================================================================

lit_tasks <- task_set(id = "three_dois", instruction = lit_task,
                      grader = grader_doi(min_recall = 1),
                      answer = list(expected_dois))   # one task, three expected DOIs
lit_tasks

# Each trial needs its OWN notes file: with a shared file, trial 2 would read
# the notes of trial 1 (a leak between trials). A solver function builds a
# fresh notes strategy for every trial.
notes_solver <- function(task) {
  ctx <- ctx_notes(tempfile(fileext = ".md"), keep_last = 2)
  run(make_scout(ctx, extra_tools = ctx$tools), task$instruction, budget = lit_budget,
      verbose = FALSE)
}

# Repeated trials must be independent: with the cache on, trials 2 and 3
# would replay trial 1. Switch it off; keep() saves the results.
old_cache <- use_cache("off")
res_full <- keep("act05_ex1_full", run_eval(make_scout(ctx_full()), lit_tasks, trials = 3,
                                            budget = lit_budget, name = "full"))
res_window <- keep("act05_ex1_window", run_eval(make_scout(ctx_window(keep_last = 2)), lit_tasks,
                                                trials = 3, budget = lit_budget, name = "window(2)"))
res_notes <- keep("act05_ex1_notes", run_eval(notes_solver, lit_tasks, trials = 3,
                                              budget = lit_budget, name = "notes"))
use_cache(old_cache)

eval_summary(list(res_full, res_window, res_notes), k = c(1, 3))
# EXPECTED OUTPUT: one row per strategy with success_rate, pass@1, pass^1,
# pass@3, pass^3, cost_per_attempt, cost_per_success, p50 and p95 seconds.
# With one task, pass^3 is either 0 or 1: it says whether the strategy
# succeeded in ALL three trials. Report the strategy with the best pass^3,
# and among those the lowest cost per success. Three trials of one task are
# still a small sample: say so when reporting.


# ==============================================================================
# EXERCISE 2: A SMALL "LOST IN THE MIDDLE" TEST
# ==============================================================================

data(extraversion)
haystack <- extraversion$text[1:60]
needle <- "The participant code is K7-ALPHA."
positions <- c(beginning = 0, middle = 30, end = 60)   # insert after this many texts

make_prompt <- function(after, trial) {
  texts <- append(haystack, needle, after = after)
  paste0("Read the notes below.\n\n", paste(texts, collapse = "\n"),
         "\n\nQuestion: What is the participant code? Reply with the code only.",
         "\n(Trial ", trial, ")")   # makes each trial a different request
}
count_tokens(make_prompt(30, 1))   # about 2,000 tokens per question

old_cache <- use_cache("off")
needle_test <- keep("act05_ex2_needle", {
  grid <- expand.grid(position = names(positions), trial = 1:3, stringsAsFactors = FALSE)
  grid$answer <- vapply(seq_len(nrow(grid)), function(i) {
    ask(make_prompt(positions[[grid$position[i]]], grid$trial[i]), model = SCOUT_MODEL)
  }, character(1))
  grid
})
use_cache(old_cache)

needle_test$correct <- grepl("K7-ALPHA", needle_test$answer, fixed = TRUE)
aggregate(correct ~ position, data = needle_test, FUN = sum)
# EXPECTED OUTPUT: the number of correct answers (out of 3) per position.
# A 2,000-token context is short for current models, so all positions may be
# correct; the effect reported by Liu et al. (2024) appears with longer
# contexts and many similar documents. Try 120 texts per prompt, keeping the
# free tier's 8,000 tokens per minute in mind (9 questions of about 4,000
# tokens need several minutes).


# ==============================================================================
# EXERCISE 3: A LEAN CROSSREF TOOL
# ==============================================================================

lean_crossref <- define_tool(
  function(query) {
    hits <- search_crossref(query, n = 10)
    filter_output(hits, fields = c("title", "year", "doi"), max_items = 3,
                  max_chars_per_field = 150, as_text = TRUE)
  },
  description = "Search Crossref for a known paper. Returns title, year and DOI of the top 3 matches.",
  args = list(query = arg_string("Title words, or a full reference string")),
  name = "search_crossref_lean")

crossref_scout <- function(tool) {
  agent("crossref_scout", sub("OpenAlex", "Crossref", scout_instructions),
        model = SCOUT_MODEL, tools = list(tool))
}
old_cache <- use_cache("off")
ex3 <- list(
  standard = keep("act05_ex3_standard", run(crossref_scout(tool_crossref()), lit_task, budget = lit_budget)),
  lean     = keep("act05_ex3_lean", run(crossref_scout(lean_crossref), lit_task, budget = lit_budget)))
use_cache(old_cache)

ex3_df <- episodes_df(ex3)
ex3_df$version <- names(ex3)
ex3_df$verified <- vapply(ex3, function(ep) grader_doi()(ep$answer, list(answer = expected_dois))$pass,
                          logical(1))
ex3_df[, c("version", "verified", "model_calls", "tokens", "peak_context")]
# EXPECTED OUTPUT: two rows. The lean version should bill fewer tokens; it is
# better only if "verified" is still TRUE. Three rows can be too few when the
# first match is a correction notice or a book review with a similar title.


# ==============================================================================
# EXERCISE 4: HOW BIG MUST THE WINDOW BE? (OFFLINE)
# ==============================================================================

first_result_sent <- function(keep_last) {
  sent <- ctx_window(keep_last)$prepare(toy, NULL)
  any(vapply(sent, function(m) identical(m$tool_call_id, "call_1"), logical(1)))
}
data.frame(keep_last = 0:6, first_result_sent = vapply(0:6, first_result_sent, logical(1)))
# EXPECTED OUTPUT (computed offline): FALSE for keep_last 0 to 5, TRUE for 6.
# Only a window as large as the whole conversation keeps the first result at
# the last call. A window is safe only when every fact the final answer needs
# is found in the last few steps; otherwise the agent must write it down
# (ctx_notes()) or the older turns must be summarized (ctx_compact()).


# ==============================================================================
# EXERCISE 5: A SECOND SKILL, WRITTEN BY HAND
# ==============================================================================

my_skills <- file.path(tempdir(), "my_skills")
dir.create(my_skills, showWarnings = FALSE)
file.copy(file.path(skills_dir, "summarizing-methods"), my_skills, recursive = TRUE)

rel_dir <- file.path(my_skills, "extracting-reliability")
dir.create(rel_dir, showWarnings = FALSE)
writeLines(c(
  "---",
  "name: extracting-reliability",
  "description: Extracts reliability evidence (coefficient, value, sample, and where it is reported) from a methods or results section. Use when the user asks about the reliability of a scale or measure.",
  "---",
  "",
  "# Extracting reliability",
  "",
  "For every reliability estimate stated in the text, report one line:",
  "coefficient (for example Cronbach's alpha, McDonald's omega, test-retest r),",
  "value, sample size, and the sentence it comes from, quoted.",
  "",
  "Write \"no reliability reported\" when the text gives none. Never compute or infer a value."),
  file.path(rel_dir, "SKILL.md"))

list_skills(my_skills)[, c("name", "valid", "n_files")]
# EXPECTED OUTPUT (computed offline): two valid skills, extracting-reliability
# (1 file) and summarizing-methods (2 files).

helper2 <- use_skills(agent("methods_helper", "Help researchers with their papers.",
                            model = SCOUT_MODEL), my_skills)
ep_rel <- keep("act05_ex5_reliability",
  run(helper2, paste("What reliability evidence does this text report?\n\n",
                     "Cronbach's alpha was .81 in the pilot sample of 120 students,",
                     "and the two-week test-retest correlation was .74."),
      budget = budget(max_steps = 5, max_cost = 0.01)))
steps <- episode_steps(ep_rel)
steps[steps$type == "tool", c("step", "name", "detail")]
# EXPECTED OUTPUT: a load_skill step whose result (detail) starts with
# "# Extracting reliability" (not "# Summarizing methods"); the answer then
# has two lines, one per coefficient, each with its quoted sentence.
