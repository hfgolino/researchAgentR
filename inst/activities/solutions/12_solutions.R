# Solutions: Activity 12, A Screening Assistant for Systematic Reviews
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Exercise 1 and the practice part of Exercise 2 run offline. The rest needs
# the objects of Activity 12 (sample_df, texts, two, screen_a, screen_b,
# final, pipeline, adj_eps, SCREENER_A, SCREEN_INSTRUCTIONS, SCREEN_SCHEMA)
# from a live run.

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")


# ==============================================================================
# EXERCISE 1: recall, precision and kappa from a 2 x 2 table
# ==============================================================================

# A CONSTRUCTED table for practice (typed in; not results of any screening):
#                     human include   human exclude
#   pipeline include        18              10
#   pipeline exclude         2              30
tp <- 18; fp <- 10; fn <- 2; tn <- 30
n <- tp + fp + fn + tn
recall    <- tp / (tp + fn)                                    # 0.90
precision <- tp / (tp + fp)                                    # 0.643
p_o <- (tp + tn) / n                                           # 0.80
p_e <- ((tp + fp) * (tp + fn) + (fn + tn) * (fp + tn)) / n^2   # 0.511
kappa <- (p_o - p_e) / (1 - p_e)                               # 0.591
round(c(recall = recall, precision = precision, p_o = p_o, p_e = p_e, kappa = kappa), 3)

# The same table as two vectors of decisions, checked with calibrate_judge()
pipeline_dec <- c(rep(TRUE, tp), rep(TRUE, fp), rep(FALSE, fn), rep(FALSE, tn))
human_dec    <- c(rep(TRUE, tp), rep(FALSE, fp), rep(TRUE, fn), rep(FALSE, tn))
calibrate_judge(pipeline_dec, human_dec)
# kappa 0.591: "moderate" by common rules of thumb, while recall is 0.90.
# In screening, the two FALSE EXCLUDES (lost studies) matter more than the
# ten false includes (extra full texts to read).


# ==============================================================================
# EXERCISE 2: choose the uncertainty threshold on data
# ==============================================================================

# The largest JEV_LOW such that records with p_min <= JEV_LOW (auto-excluded)
# still leave recall >= target among the records of the calibration half.
choose_low <- function(p_min, truth, target = 0.95) {
  grid <- sort(unique(c(0, p_min[!is.na(p_min)])))
  ok <- vapply(grid, function(t) {
    kept <- is.na(p_min) | p_min > t          # NA (no answer) is never auto-excluded
    sum(kept & truth) / sum(truth) >= target
  }, logical(1))
  max(grid[ok])
}

# Practice with CONSTRUCTED probabilities (rbeta; not Jev output)
set.seed(2026)
truth_practice <- c(rep(TRUE, 20), rep(FALSE, 40))
p_practice <- ifelse(truth_practice, rbeta(60, 5, 2), rbeta(60, 2, 5))
half <- sample(rep(1:2, 30))
low <- choose_low(p_practice[half == 1], truth_practice[half == 1])
test_kept <- p_practice[half == 2] > low
c(chosen_low = round(low, 3),
  recall_calibration_half = mean(p_practice[half == 1 & truth_practice] > low),
  recall_test_half = sum(test_kept & truth_practice[half == 2]) / sum(truth_practice[half == 2]),
  auto_excluded_test_half = mean(!test_kept))
# The recall on the test half is the honest estimate; the calibration half
# was used to choose the threshold, so its recall is optimistic by design.

# LIVE: the same with Jev's p_min from Activity 12 (screener B = "jev")
if (exists("screen_b") && "p_min" %in% names(screen_b)) {
  truth_b <- sample_df$label[two] == 1
  set.seed(2026)
  half_b <- sample(rep(1:2, length.out = length(two)))
  low_b <- choose_low(screen_b$p_min[half_b == 1], truth_b[half_b == 1])
  kept_b <- is.na(screen_b$p_min[half_b == 2]) | screen_b$p_min[half_b == 2] > low_b
  print(c(chosen_low = low_b,
          recall_test_half = sum(kept_b & truth_b[half_b == 2]) / sum(truth_b[half_b == 2])))
}
# With a sample of 60 and few includes, both halves are small: treat the
# result as a pilot estimate, and re-estimate on a larger labeled sample.


# ==============================================================================
# EXERCISE 3: self-consistency votes for screener A (LIVE RUN NEEDED)
# ==============================================================================

# Three independent runs need the response cache OFF (otherwise runs 2 and 3
# replay run 1); keep() saves the finished votes.
votes <- keep("screen12_ex3_votes", (function() {
  old <- use_cache("off")
  on.exit(use_cache(old))
  screener <- agent("screener", SCREEN_INSTRUCTIONS, model = SCREENER_A,
                    output_schema = SCREEN_SCHEMA)
  sapply(1:3, function(run_id) vapply(texts[two], function(x) {
    e <- run(screener, x, budget(max_steps = 3, max_cost = 0.005), verbose = FALSE)
    if (is.list(e$answer) && length(e$answer$decision) == 1) e$answer$decision == "include" else NA
  }, logical(1)))
})())
p_vote <- rowMeans(votes, na.rm = TRUE)
table(share_include = round(p_vote, 2))
data.frame(id = sample_df$id[two], p_vote = p_vote,
           human = sample_df$label[two])[p_vote > 0 & p_vote < 1, ]
# Records that split 2 to 1 are natural candidates for the human queue. A vote
# share is not a calibrated probability: check it against human labels
# before using it as one.


# ==============================================================================
# EXERCISE 4: the AI-use statement for the screening run (needs the run)
# ==============================================================================

screen_eps <- c(attr(screen_a, "episodes"), adj_eps,
                if (!"p_min" %in% names(screen_b)) attr(screen_b, "episodes"))
review_text <- sprintf(paste(
  "The first author compared the pipeline's decisions with human title-and-abstract",
  "decisions on a sample of %d records: recall of human includes was %.2f, and %d records",
  "(%.0f%%) were sent to a person for review."),
  nrow(sample_df), sum(pipeline & sample_df$label == 1) / sum(sample_df$label == 1),
  sum(final$to_human), 100 * mean(final$to_human))
statement <- ai_use_statement(screen_eps,
                              purpose = "screening titles and abstracts for a systematic review",
                              human_review = review_text)
statement
# Jev calls (screener B) are not agent episodes, so the statement does not
# list them: add one sentence by hand (model jev-1.13.0, number of calls,
# date, cost from attr(..., "cost")). The statement never claims more than
# the logs show; everything else is yours to write.
