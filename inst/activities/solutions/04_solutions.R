# Solutions: Activity 04, A Graded Task Set
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Run 04_Graded_Task_Set.R first: these solutions use its objects (tasks,
# dev, res, scout, eval_budget, summary_scout).

library(researchAgentR)
use_cache("auto")


# ==============================================================================
# Exercise 1: regrade stored answers with a stricter DOI grader
# (no new model calls: graders are separate from the agent)
# ==============================================================================

strict_doi <- grader_doi(min_precision = 1)
doi_rows <- which(res$task_id %in% c("doi_dynega", "doi_entropy") & !is.na(res$answer))
regraded <- vapply(doi_rows, function(i) {
  task <- list(id = res$task_id[i], answer = tasks$answer[[match(res$task_id[i], tasks$id)]])
  strict_doi(res$answer[i], task)$pass
}, logical(1))

comparison <- data.frame(task_id = res$task_id[doi_rows], trial = res$trial[doi_rows],
                         lenient = res$pass[doi_rows], strict = regraded)
comparison
sum(comparison$lenient %in% TRUE & !comparison$strict)   # passed before, fail now
# A trial that passes only with the lenient grader returned extra DOIs: the
# scout lists up to 8 records, while the task asks for one DOI.


# ==============================================================================
# Exercise 2: a plain-text scout, compared with the structured scout
# ==============================================================================

scout_text <- agent(
  name = "literature_scout_text",
  description = "Finds literature and answers in the format the task asks for.",
  instructions = paste(scout$instructions,
    "6. Answer in exactly the format the task asks for (for example, the DOI only).",
    "   Do not add explanations when the task asks for one value."),
  model = scout$model,
  tools = scout$tools,          # same tools, no output_schema
  effort = "low")

res_text <- keep("act04_ex2_eval_scout_text", {
  old_mode <- use_cache("off")   # independent trials (see Step 6)
  out <- run_eval(scout_text, tasks, trials = 3, budget = eval_budget,
                  save_dir = "act04_episodes_text")
  use_cache(old_mode)
  out
})

eval_summary(list(res, res_text), k = c(1, 3))
# Per task, side by side (mean pass over the 3 trials):
merge(aggregate(pass ~ task_id, data = as.data.frame(res), FUN = mean),
      aggregate(pass ~ task_id, data = as.data.frame(res_text), FUN = mean),
      by = "task_id", suffixes = c("_structured", "_text"))
# Activity 09 compares two agents properly, with task_compare() and
# paired_bootstrap() over tasks.


# ==============================================================================
# Exercise 3: a task with a hashed answer key (offline)
# ==============================================================================

# Use a DOI you checked yourself in the published article. The DynEGA DOI
# (a dev answer, already public in this course) serves as the example.
my_task <- task_set(
  id = "my_hashed_task",
  instruction = paste("Return the DOI of the article titled 'Modeling Latent Topics in Social",
                      "Media using Dynamic Exploratory Graph Analysis' (Psychometrika, 2022)."),
  grader = grader_doi(min_precision = 1),
  answer = list(hash_answer("10.1007/s11336-021-09820-y", type = "doi")),
  split = "heldout")
my_task      # the answer column shows "(hashed)"

key <- list(id = my_task$id, answer = my_task$answer[[1]])
my_task$grader[[1]]("https://doi.org/10.1007/S11336-021-09820-Y", key)$pass   # TRUE
my_task$grader[[1]]("10.1007/s11336-021-09821-y", key)$pass                   # FALSE


# ==============================================================================
# Exercise 4: pass@k and pass^k for n = 3 (offline)
# ==============================================================================

grid <- expand.grid(c = 0:3, k = 1:3)
grid$pass_at_k  <- pass_at_k(n = 3, c = grid$c, k = grid$k)
grid$pass_hat_k <- pass_hat_k(n = 3, c = grid$c, k = grid$k)
grid[order(grid$k, grid$c), ]
# pass^3 is 0 for c = 0, 1 and 2, and 1 only when c = 3: with three trials,
# "works every time" requires all three to succeed. pass@3 is 1 as soon as
# c >= 1.
