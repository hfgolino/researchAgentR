# Solutions: Activity 08 (Multi-Agent Manuscript Review)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Exercises 2 and 3 run offline (no key, no network). Exercise 1 uses
# rv_checks from STEP 8, Exercise 4 uses cmp from STEP 10 and Exercise 5 uses
# rv_model from STEP 9 of Activity 08.

library(researchAgentR)
use_cache("auto")

pe_dir     <- system.file("extdata", "planted_errors", package = "researchAgentR")
f_planted  <- file.path(pe_dir, "aigenie_excerpt_with_errors.md")
f_original <- file.path(pe_dir, "aigenie_excerpt_original.md")
key <- read.csv(file.path(pe_dir, "answer_key.csv"), stringsAsFactors = FALSE,
                fileEncoding = "UTF-8")
rules <- default_style_rules(first_person_plural = FALSE)


# ==============================================================================
# Exercise 1: which check caught which planted error (needs rv_checks, STEP 8)
# ==============================================================================

caught_rows <- as.data.frame(rv_checks)[!is.na(rv_checks$error_id),
                                        c("error_id", "category", "suggestion")]
caught_rows$check <- ifelse(grepl("No reference list entry", caught_rows$suggestion), "citation cross-check",
                     ifelse(grepl("DOI", caught_rows$suggestion), "DOI lookup (verify_citations)",
                     ifelse(grepl("p-value", caught_rows$suggestion), "statistics (p recomputed)",
                     ifelse(grepl("rule:", caught_rows$suggestion), "style rule", "other"))))
caught_rows[order(caught_rows$error_id), c("error_id", "check")]
# EXPECTED OUTPUT: error 1 by the citation cross-check, error 5 by style rules
# (one banned_word and two em_dash rows), error 3 by the p-value check, and,
# with network access, error 6 by the DOI lookup.


# ==============================================================================
# Exercise 2: a rule for "prove", graded with the same recipe
# ==============================================================================

rules2 <- rbind(rules, data.frame(
  rule = "prove", pattern = "(?i)\\bprov(e|es|en)\\b",
  suggestion = "Hedge the claim: data support or suggest; they do not prove.",
  severity = "medium"))

# confidential = TRUE skips the online DOI lookup: this exercise is about a
# style rule, and it then runs offline in a fraction of a second.
rv2  <- review_checks(f_planted,  rules = rules2, confidential = TRUE, run_code = FALSE)
rv2o <- review_checks(f_original, rules = rules2, confidential = TRUE, run_code = FALSE)

sig2  <- paste(rv2$quote, rv2$suggestion)
sig2o <- paste(rv2o$quote, rv2o$suggestion)
rv2$pre_existing <- sig2 %in% sig2o
rv2$error_id <- NA_integer_
for (i in seq_len(nrow(key))) {
  hit <- !rv2$pre_existing & is.na(rv2$error_id) & grepl(key$match_pattern[i], sig2, perl = TRUE)
  rv2$error_id[hit] <- key$id[i]
}
data.frame(caught = sum(key$id %in% rv2$error_id),
           recall = mean(key$id %in% rv2$error_id),
           false_alarms = sum(!rv2$pre_existing & is.na(rv2$error_id)),
           prove_flags_on_original = sum(grepl("rule: prove", rv2o$suggestion)))
# EXPECTED OUTPUT: the overclaim (4) is now caught, in addition to errors 1
# 3 and 5 (DOIs, error 6, are not checked with confidential = TRUE), no new false alarms, and 0 "prove" flags on the
# original excerpt. A rule
# catches this overclaim; it would miss "demonstrates conclusively" or
# "establishes". Rules are cheap, exact and narrow.


# ==============================================================================
# Exercise 3: a code check for the impossible total sample size
# ==============================================================================

for (f in c(original = f_original, planted = f_planted)) {
  d <- read_documents(f)
  abstract <- paste(d$text[d$section == "Abstract"], collapse = " ")
  participants <- paste(d$text[d$section == "Participants"], collapse = " ")
  n_total <- as.numeric(gsub(",", "", regmatches(abstract,
                         regexpr("(?<=N = )[0-9,]+", abstract, perl = TRUE))))
  range_n <- as.numeric(regmatches(participants,
                         regexec("ranged from ([0-9]+) to ([0-9]+)", participants))[[1]][2:3])
  k <- 5   # "Five nationally representative samples" (Participants)
  cat(basename(f), ": abstract N =", n_total, "| possible totals",
      k * range_n[1], "to", k * range_n[2], "| consistent:",
      n_total >= k * range_n[1] && n_total <= k * range_n[2], "\n")
}
# EXPECTED OUTPUT: the original is consistent (4964 lies between 4945 and
# 4985); the planted version is not (4694). Once a rule can be written down,
# code catches it every time, for free.


# ==============================================================================
# Exercise 4: cost per planted error caught, by arm (needs cmp, STEP 10)
# ==============================================================================

per_arm <- aggregate(cbind(caught, cost, tokens) ~ arm, data = cmp, FUN = sum)
per_arm$cost_per_error_caught <- ifelse(per_arm$caught > 0, per_arm$cost / per_arm$caught, NA)
per_arm$tokens_per_error_caught <- ifelse(per_arm$caught > 0, per_arm$tokens / per_arm$caught, NA)
per_arm
# EXPECTED OUTPUT: one row per arm with the total errors caught over both
# trials, total cost and tokens, and the cost and tokens per error caught.
# Choose the team only if it catches more AND the extra catches are worth
# the extra cost (and compare both with the free checks of STEP 7).


# ==============================================================================
# Exercise 5: precision of the model reviewer on a hand-labelled sample
# ==============================================================================

other <- as.data.frame(rv_model)[rv_model$source == "model" & is.na(rv_model$error_id),
                                 c("id", "section", "quote", "suggestion")]
set.seed(2026)
sample_rows <- other[sample(nrow(other), min(10, nrow(other))), ]
sample_rows
# Read each row against the manuscript, then record your labels, in order:
# TRUE = a valid comment on the article, FALSE = a false alarm.
my_labels <- rep(NA, nrow(sample_rows))      # replace NA with TRUE or FALSE
precision_sample <- mean(my_labels, na.rm = TRUE)
precision_sample
# EXPECTED OUTPUT: the share of sampled suggestions you judged valid. With 10
# rows the uncertainty is large; report the count (for example 7 of 10), not
# only the proportion.
