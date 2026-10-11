# Solutions: Activity 18, Review Your Own Paper
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

library(researchAgentR)
use_cache("auto")
set_groq_key()

# Objects from the activity:
manuscript <- system.file("extdata", "planted_errors", "aigenie_excerpt_with_errors.md",
                          package = "researchAgentR")
key <- read.csv(system.file("extdata", "planted_errors", "answer_key.csv", package = "researchAgentR"),
                stringsAsFactors = FALSE)
dir.create("review_work", showWarnings = FALSE)
my_copy <- file.path("review_work", basename(manuscript))
file.copy(manuscript, my_copy, overwrite = TRUE)
recall_table <- function(review, key) {
  txt <- paste(review$quote, review$suggestion)
  data.frame(id = key$id, type = key$type,
             found = vapply(key$match_pattern, function(p) any(grepl(p, txt, perl = TRUE)),
                            logical(1)),
             row.names = NULL)
}
my_rules <- rbind(
  default_style_rules(),
  data.frame(rule = "overclaim", pattern = "(?i)\\bprov(e|es|ed|en|ing)\\b",
             suggestion = "Empirical results do not prove a claim; use show, indicate or suggest.",
             severity = "medium"))
my_guidelines <- paste(
  "- Every sample size reported in the abstract matches the method and results.",
  "- Every primary result reports an effect size with a confidence interval.",
  "- Every p-value is consistent with its test statistic and degrees of freedom.",
  "- Claims about generality are no stronger than the design allows.",
  "- The method says how the sample size was determined.",
  sep = "\n")


# ==============================================================================
# Exercise 1: the sample-size mismatch, found in code (offline)
# ==============================================================================

docs <- read_documents(my_copy)
abstract <- paste(docs$text[docs$section == "Abstract"], collapse = " ")
participants <- paste(docs$text[docs$section == "Participants"], collapse = " ")

# The total claimed in the abstract ("N = 4,694 total"):
total_txt <- regmatches(abstract, regexpr("N = [0-9,]+ total", abstract))
total <- as.numeric(gsub("[^0-9]", "", total_txt))

# The number of samples and the range of final Ns in the Participants section:
n_samples <- 5     # "Five nationally representative samples"
range_txt <- regmatches(participants, regexpr("final Ns ranged from [0-9]+ to [0-9]+", participants))
ns <- as.numeric(regmatches(range_txt, gregexpr("[0-9]+", range_txt))[[1]])
possible <- n_samples * ns
c(total = total, min_possible = possible[1], max_possible = possible[2],
  consistent = total >= possible[1] && total <= possible[2])

# EXPECTED OUTPUT (computed offline): total 4694, min_possible 4945,
# max_possible 4985, consistent 0 (FALSE). Five samples of 989 to 997 people
# cannot total 4,694. A model reviewer may notice this; code checks it every
# time, for free. (The number of samples is typed in by hand here: reading
# "Five" from the text in code is possible, but checking it by eye is safer.)


# ==============================================================================
# Exercise 2: three reviews, and pass^3 per planted error (model calls)
# ==============================================================================

# The response cache would replay the first run's answers for the second and
# third run (the requests are identical), so it is switched off here; keep()
# still saves each review, so later runs of this script are offline.
old_mode <- use_cache("off")
reviews <- lapply(1:3, function(i) keep(paste0("review_planted_run_", i), review_manuscript(
  my_copy, focus = c("argument", "methods", "reporting", "references", "style"),
  guidelines = my_guidelines, rules = my_rules,
  model = "groq:openai/gpt-oss-120b", run_code = FALSE)))
use_cache(old_mode)

found <- sapply(reviews, function(rv) recall_table(rv, key)$found)
rownames(found) <- key$type
found
c_found <- rowSums(found)
data.frame(type = key$type, found_in = c_found,
           pass_at_1 = pass_at_k(n = 3, c = c_found, k = 1),
           pass_hat_3 = pass_hat_k(n = 3, c = c_found, k = 3))
sapply(reviews, function(rv) attr(rv, "cost"))

# EXPECTED OUTPUT: a 6 x 3 table of TRUE/FALSE; per error, the number of runs
# that found it (0 to 3), pass@1 (the share of runs) and pass^3 (1 only when
# all three runs found it); then the cost of each review. Errors found by a
# code check (the missing reference, "delve", the overclaim rule) are found
# in every run; errors that depend on the model vary between runs.


# ==============================================================================
# Exercise 3: why were model suggestions dropped? (after Step 5)
# ==============================================================================

# The review saved by keep() in Step 5. This helper only READS it.
kept <- function(name) {
  f <- file.path(cache_dir(), "kept", paste0(name, ".rds"))
  if (!file.exists(f)) stop("'", name, "' was not saved yet: run Activity 18 first.")
  readRDS(f)
}
review <- kept("review_planted_manuscript")
dropped <- attr(review, "dropped")
if (NROW(dropped)) {
  text_all <- paste(read_documents(my_copy)$text, collapse = " ")
  dropped$kind <- vapply(dropped$quote, function(q) {
    if (!nzchar(q)) return("empty quote")
    if (agrepl(q, text_all, max.distance = 0.1, fixed = TRUE)) return("paraphrased or retyped")
    "not in the text (invented or merged from two places)"
  }, character(1))
  dropped[, c("section", "quote", "kind")]
}

# EXPECTED OUTPUT: one row per dropped suggestion with its quote and a first
# classification. agrepl() allows about 10% of the characters to differ, so
# a quote with changed punctuation or a retyped dash is "paraphrased or
# retyped"; read the rest by hand. If no suggestion was dropped, nothing
# prints: the verifier had nothing to remove in that run.


# ==============================================================================
# Exercise 4: a rule for "p = .000" (offline)
# ==============================================================================

p_rule <- rbind(default_style_rules(),
                data.frame(rule = "p_zero", pattern = "(?i)\\bp\\s*=\\s*0?\\.000\\b",
                           suggestion = "A p-value is never zero: write p < .001.",
                           severity = "medium"))
test_file <- file.path(tempdir(), "p_values.md")
writeLines(c("# Results", "",
             "The groups differed, t(98) = 4.20, p = .000, d = 0.84.", "",
             "The correlation was small, r(120) = .15, p = .098, and the slope was large, p = 0.000."),
           test_file)
p_check <- review_checks(test_file, rules = p_rule, confidential = TRUE, run_code = FALSE)
as.data.frame(p_check)[grepl("p_zero", p_check$suggestion), c("severity", "quote")]

# EXPECTED OUTPUT (computed offline): two medium rows, one per paragraph, for
# "p = .000" and "p = 0.000". The quote is the surrounding sentence, so the
# second one also shows "p = .098", which the rule did not match.
