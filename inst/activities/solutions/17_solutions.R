# Solutions: Activity 17, Learn Your Writing Style
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

library(researchAgentR)
use_cache("auto")
set_groq_key()

# Objects from the activity (the model results are replayed by keep()):
corpus_dir <- system.file("extdata", "style_corpus", package = "researchAgentR")
docs <- read_documents(corpus_dir)
held_docs <- docs[docs$file == "04_discussion.md", ]
real_discussion <- held_docs$text


# ==============================================================================
# Exercise 1: sentence length by hand (offline)
# ==============================================================================

p1 <- real_discussion[1]
# A naive split: a period, question mark or exclamation mark, then a space
# and a capital letter.
naive <- strsplit(p1, "(?<=[.!?])\\s+(?=[A-Z])", perl = TRUE)[[1]]
words_in <- function(s) lengths(regmatches(s, gregexpr("[[:alpha:]]+(['-][[:alpha:]]+)*", s)))
length(naive)
mean(words_in(naive))
prof1 <- style_profile(p1)
c(n_sentences = prof1$n_sentences, sentence_mean = prof1$sentence_mean)

# EXPECTED OUTPUT (computed offline): both find 4 sentences; the naive mean is
# 28.5 words and style_profile() gives 28. The difference is two "words": the
# naive counter takes the letter of "2019a" and "2019b" as a word, while
# style_profile() does not count letters glued to a number. On other
# paragraphs the sentence counts can differ too. The regular expression has
# to decide about "e.g.," and "i.e.," (protected in style_profile(), not in
# the naive split),
# "et al." followed by a capitalized word, initials such as "S. M. Boker",
# and "Fig. 4" (protected); decimals such as "0.40" never end a sentence
# because no space follows the period.


# ==============================================================================
# Exercise 2: a measure of your own (offline)
# ==============================================================================

impersonal_rate <- function(texts) {
  txt <- tolower(paste(texts, collapse = " "))
  hits <- lengths(regmatches(txt, gregexpr("the present study|the current paper", txt)))
  words <- style_profile(texts)$n_words
  c(hits = hits, words = words, per_1k = round(hits / words * 1000, 2))
}
t(sapply(split(docs$text, docs$file), impersonal_rate))

# EXPECTED OUTPUT (computed offline):
#                              hits words per_1k
#   01_introduction.md            3   902   3.33
#   02_method_and_simulation.md   1  7094   0.14
#   03_applied_example.md         0  2094   0.00
#   04_discussion.md              1  1934   0.52
# The impersonal phrases cluster in the Introduction, where the paper
# announces its aims; the same paper uses "we" far more in the Discussion
# (Activity 17, Step 3).


# ==============================================================================
# Exercise 3: the classifier with citations included (model results)
# ==============================================================================

# Results saved by keep() in the activity (Steps 5 and 8b). This helper only
# READS them, and stops with a clear message if the activity was not run.
kept <- function(name) {
  f <- file.path(cache_dir(), "kept", paste0(name, ".rds"))
  if (!file.exists(f)) stop("'", name, "' was not saved yet: run Activity 17 first.")
  readRDS(f)
}
styled   <- kept("style_generated")
unstyled <- kept("style_unstyled")

loo_accuracy <- function(X, y) {
  d <- data.frame(y = y, scale(X))
  pred <- vapply(seq_len(nrow(d)), function(i) {
    fit <- suppressWarnings(glm(y ~ ., data = d[-i, ], family = binomial))
    as.integer(suppressWarnings(predict(fit, d[i, , drop = FALSE], type = "response")) > 0.5)
  }, integer(1))
  mean(pred == d$y)
}
features7 <- c("sentence_mean", "clauses_per_sentence", "passive_share", "we_per_1k",
               "hedges_per_1k", "transitions_per_1k", "citations_per_1k")
features_of <- function(texts, cols) {
  do.call(rbind, lapply(texts, function(t) as.data.frame(style_profile(t))[cols]))
}
n <- length(real_discussion)
labels <- rep(1:0, c(n, n))
X7 <- features_of(c(real_discussion, styled), features7)
loo_accuracy(X7, labels)
aggregate(citations_per_1k ~ group, FUN = mean,
          data = data.frame(group = ifelse(labels == 1, "real", "generated"), X7))

# EXPECTED OUTPUT: an accuracy, and the mean citation rate per group. If the
# generated paragraphs cite much less (they write [CITATION NEEDED] instead
# of inventing a reference), citations alone can separate the groups, and
# the accuracy rises for a reason that is not style. A fair test removes
# the feature, or removes citations from the real paragraphs too.


# ==============================================================================
# Exercise 4: is the difference between the two classifiers more than chance?
# ==============================================================================

features6 <- setdiff(features7, "citations_per_1k")
loo_correct <- function(X, y) {
  d <- data.frame(y = y, scale(X))
  vapply(seq_len(nrow(d)), function(i) {
    fit <- suppressWarnings(glm(y ~ ., data = d[-i, ], family = binomial))
    p <- suppressWarnings(predict(fit, d[i, , drop = FALSE], type = "response"))
    as.integer(p > 0.5) == d$y[i]
  }, logical(1))
}
right_styled   <- loo_correct(features_of(c(real_discussion, styled), features6), labels)
right_unstyled <- loo_correct(features_of(c(real_discussion, unstyled), features6), labels)

# Position j is the same real paragraph, or text written from the same
# outline, in both comparisons: the pairs are matched.
table(styled = right_styled, unstyled = right_unstyled)
only_unstyled <- sum(!right_styled & right_unstyled)
only_styled   <- sum(right_styled & !right_unstyled)
if (only_unstyled + only_styled > 0) {
  binom.test(only_unstyled, only_unstyled + only_styled)   # exact McNemar test
}

# EXPECTED OUTPUT: a 2 x 2 table of correct and wrong classifications, and an
# exact binomial test on the discordant pairs. A small p-value means the
# unstyled text is easier to detect than the styled text beyond chance. With
# 24 positions, only large differences reach significance.
