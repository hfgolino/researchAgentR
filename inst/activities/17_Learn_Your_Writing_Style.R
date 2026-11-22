# Activity 17: Learn Your Writing Style
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: measure a writing style in code, learn it with one model call, turn
# it into a skill, write in it, and TEST whether the result is really
# indistinguishable from the author's own paragraphs.
# OVERVIEW: the package ships a small corpus: the prose of one open-access
# paper, Golino, Christensen, Moulder, Kim and Boker (2022), "Modeling latent
# topics in social media using Dynamic Exploratory Graph Analysis",
# Psychometrika, 87(1), 156-187, https://doi.org/10.1007/s11336-021-09820-y,
# licensed CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/). The
# text was extracted from the published PDF; equations, tables, figures and
# references were removed; each file says so at the top. It is CO-AUTHORED
# text: five authors, several voices, edited together. A learned "style" of
# it is the style of that paper, not of one person.
#
# USE YOUR OWN PAPERS for your own style (Step 9 explains how, and what to
# disclose). Learning someone else's style needs their consent.
#
# The evaluation design is set BEFORE any model call: the Discussion is held
# out, the style is learned from the other sections only, and new Discussion
# paragraphs are written from outlines of the real ones. A classifier then
# tries to tell real from generated paragraphs. Accuracy near 50% means the
# style was matched; accuracy near 100% means it was not.
#
# researchAgentR functions used here:
#   read_documents()    files -> one row per paragraph, with its section
#   style_profile()     style measures computed in base R (no model)
#   style_distance()    how far a text is from a style, in standard units
#   learn_style()       a style guide from representative paragraphs (one model call)
#   style_to_skill()    the learned style as an Agent Skill (Activity 16)
#   write_in_style()    draft or rewrite in the style (one model call each)
#   validate_skill(), ask()
# TIME: about 60 minutes.  COST: free tier (Groq). Steps 1 to 4 run offline;
# Steps 5 to 8 call a model (Step 8 makes about 36 short calls).
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # first run calls the APIs and saves; later runs replay
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron (see Activity 00)

corpus_dir <- system.file("extdata", "style_corpus", package = "researchAgentR")
list.files(corpus_dir)

# EXPECTED OUTPUT: "01_introduction.md", "02_method_and_simulation.md",
# "03_applied_example.md", "04_discussion.md".

# To use your own papers instead, put them in one folder as .pdf, .md, .Rmd,
# .qmd, .tex or .txt files (Word files are not read: export them to PDF or
# Markdown first) and set corpus_dir <- "my_papers".


# ==============================================================================
# STEP 1: READ THE TEXTS AS PARAGRAPHS (OFFLINE)
# ==============================================================================

docs <- read_documents(corpus_dir)
str(docs)
table(docs$file)
head(docs[, c("file", "section", "paragraph")])
cat(read_text(file.path(corpus_dir, "04_discussion.md"), n = 4), sep = "\n")

# EXPECTED OUTPUT (computed offline): a data frame of 83 paragraphs with
# columns file, section, paragraph and text; 6 in the introduction, 56 in
# the method and simulation file, 9 in the applied example and 12 in the
# discussion. Sections come from the Markdown headings ("1. Introduction",
# "2.1. Graphical LASSO", ...). The license notice sits in the YAML header
# at the top of each file (the first lines printed by cat()), which
# read_documents() drops, so the notice is not counted as the authors' prose.
# Where an equation was removed inside a paragraph, the text shows "[...]".


# ==============================================================================
# STEP 2: MEASURE THE STYLE IN CODE (OFFLINE)
# ==============================================================================

# style_profile() computes every number with documented regular expressions
# in base R, so each one can be checked by hand (see ?style_profile).
profile_all <- style_profile(docs)
profile_all

# EXPECTED OUTPUT (computed offline on the shipped corpus):
#   <style_profile> 12,024 words, 413 sentences, 83 paragraphs
#   Sentence length (words): mean 29.1, median 27.0, sd 13.6, IQR 20.0 to 37.0
#   Words per paragraph: 144.9 | clauses per sentence (proxy): 3.65
#   Passive share (proxy): 0.39
#   Per 1,000 words: we/our/us 4.8 | I/me/my 0.9 | em dash 0.8 | en dash 0.2 | semicolon 5.2
#   Per 1,000 words: hedges 3.7 | boosters 2.7 | transitions 4.3 | citations 12.1
#   Top transitions: however (10); therefore (10); for example (10); instead (6); that is (5)
#   Lexical diversity (MATTR, 500-word window): 0.430
#
# Reading the numbers:
# - Long sentences (mean 29 words) with about 3.7 clause markers each, and
#   long paragraphs (145 words): a dense, clause-separated register.
# - "I/me/my 0.9" is NOT first person singular: the applied example labels
#   its topics A to K, and "topic I" matches the pattern. Proxies have known
#   false positives; read the matches before trusting a rate.
grep("\\bI\\b", docs$text)
unique(docs$file[grep("\\bI\\b", docs$text)])

# EXPECTED OUTPUT (computed offline): paragraphs 68 to 71, all in
# "03_applied_example.md".

# The profile also stores how much each measure varies across chunks of
# about 300 words of this text. style_distance() uses it as the scale.
round(attr(profile_all, "chunk_sd"), 2)


# ==============================================================================
# STEP 3: ONE PAPER, SEVERAL STYLES (OFFLINE)
# ==============================================================================

by_section <- do.call(rbind, lapply(split(docs, docs$file), function(d) {
  p <- style_profile(d)
  data.frame(file = d$file[1], words = p$n_words, sentence_mean = round(p$sentence_mean, 1),
             words_per_paragraph = round(p$words_per_paragraph, 1),
             passive_share = round(p$passive_share, 2), we_per_1k = round(p$we_per_1k, 1),
             citations_per_1k = round(p$citations_per_1k, 1))
}))
by_section

# EXPECTED OUTPUT (computed offline):
#   file                         words  sentence_mean  words/para  passive  we/1k  citations/1k
#   01_introduction.md             902           36.1       150.3     0.40    7.8          27.7
#   02_method_and_simulation.md   7094           27.3       126.7     0.39    3.5          11.4
#   03_applied_example.md         2094           30.3       232.7     0.48    2.4           6.7
#   04_discussion.md              1934           32.8       161.2     0.27   10.9          12.9
# The Discussion uses "we" three times as often as the Method and fewer
# passives (0.27 against 0.39): a section has a style of its own. That is
# why the test in Step 8 compares Discussion paragraphs with Discussion
# paragraphs.


# ==============================================================================
# STEP 4: THE EVALUATION DESIGN, FIXED BEFORE ANY MODEL CALL (OFFLINE)
# ==============================================================================

# Hold out the Discussion. The style is learned from the other three files.
train_docs <- docs[docs$file != "04_discussion.md", ]
held_docs  <- docs[docs$file == "04_discussion.md", ]
nrow(train_docs); nrow(held_docs)

# A reference value: how far is the authors' OWN held-out Discussion from
# the style of their other sections? Generated text should not be judged
# against zero, but against this.
train_profile <- style_profile(train_docs)
own_distance <- style_distance(held_docs, train_profile)
own_distance

# EXPECTED OUTPUT (computed offline): 71 and 12 paragraphs; "<style_distance>
# 0.58 (root mean square of standardized differences)", then a table sorted
# by |z|. The largest difference is we_per_1k (10.86 in the Discussion
# against 3.67 in the training sections, z = 1.50). Values near 1 are
# typical of an author's own variation; values well above 2 are clearly
# different.

# The classifier used in Step 8: a logistic regression on six style
# measures per paragraph, with leave-one-out cross-validation (each
# paragraph is predicted by a model fitted without it).
features <- c("sentence_mean", "clauses_per_sentence", "passive_share",
              "we_per_1k", "hedges_per_1k", "transitions_per_1k")
# citations_per_1k is left out on purpose: write_in_style() is told never to
# invent citations, so generated paragraphs cite less for a reason that has
# nothing to do with style.

paragraph_features <- function(texts) {
  do.call(rbind, lapply(texts, function(t) as.data.frame(style_profile(t))[features]))
}
loo_accuracy <- function(X, y) {
  d <- data.frame(y = y, scale(X))
  pred <- vapply(seq_len(nrow(d)), function(i) {
    fit <- suppressWarnings(glm(y ~ ., data = d[-i, ], family = binomial))
    p <- suppressWarnings(predict(fit, d[i, , drop = FALSE], type = "response"))
    as.integer(p > 0.5)
  }, integer(1))
  mean(pred == d$y)
}

# Check the classifier on REAL text only, before any generated text exists.
real_discussion <- held_docs$text
applied <- docs$text[docs$file == "03_applied_example.md"]
loo_accuracy(paragraph_features(c(real_discussion, applied)),
             rep(1:0, c(length(real_discussion), length(applied))))

method <- docs$text[docs$file == "02_method_and_simulation.md"]
method_words <- vapply(method, function(t) style_profile(t)$n_words, numeric(1), USE.NAMES = FALSE)
set.seed(2026)
method12 <- sample(method[method_words >= 40], 12)
loo_accuracy(paragraph_features(c(real_discussion, method12)), rep(1:0, c(12, 12)))

# How much does accuracy vary by chance alone with 24 paragraphs?
qbinom(c(0.025, 0.975), size = 24, prob = 0.5) / 24

# EXPECTED OUTPUT (computed offline): 0.571 (Discussion against the applied
# example, 21 paragraphs), 0.667 (Discussion against 12 Method paragraphs),
# and 0.29 0.71: a classifier that guesses gets an accuracy between 0.29
# and 0.71 in 95% of samples of this size. Even two sections of the SAME
# paper approach the upper edge of that range, so a small accuracy gap
# proves little, in either direction.


# ==============================================================================
# STEP 5: LEARN THE STYLE (ONE MODEL CALL)
# ==============================================================================

# learn_style() measures the training text, picks representative paragraphs
# (the most central ones when embeddings are available, otherwise those
# with the median length), and asks the model for a style guide that quotes
# them. Every quotation in the guide is then checked IN CODE against the
# source, and quotations that are not found verbatim are removed and counted.
my_style <- keep("style_dynega_train", learn_style(
  train_docs,
  model = "groq:openai/gpt-oss-120b",
  n_exemplars = 8,
  author = "Golino, Christensen, Moulder, Kim and Boker (2022)"))
my_style
cat(my_style$guide)
my_style$quotes
my_style$exemplars[, c("file", "section", "paragraph")]

# EXPECTED OUTPUT: "<writing_style> Golino, Christensen, Moulder, Kim and
# Boker (2022) | groq:openai/gpt-oss-120b | <date>", the number of source
# files and exemplars and how they were picked ("embedding centrality" or
# "length closest to the median"), "Quotations: N checked, M removed as not
# found" and the cost. The guide has the sections Sentence architecture,
# Register, Recurring moves, Vocabulary, What the author avoids and
# Checklist. my_style$quotes lists every quotation with found = TRUE or
# FALSE; a removed quotation appears in the guide as
# "[quotation removed: not found in the source]". Recorded run (November 22,
# 2026): 3 source files, 8 exemplars by embedding centrality, 29 quotations
# checked and 10 removed, cost $0.00252.
#
# Read the guide critically: does every claim match the numbers of Step 2?


# ==============================================================================
# STEP 6: THE STYLE AS A SKILL (OFFLINE AFTER STEP 5)
# ==============================================================================

dir.create("skills", showWarnings = FALSE)
style_check <- style_to_skill(my_style, "writing-like-dynega-paper", dir = "skills",
                              overwrite = TRUE)
style_check
validate_skill(attr(style_check, "path"))
cat(read_text(file.path(attr(style_check, "path"), "SKILL.md"), n = 30), sep = "\n")

# EXPECTED OUTPUT: "Valid skill." The SKILL.md has a third-person description
# ("Writes and revises academic prose in the style of ... Use when ..."), a
# table of measured targets (mean sentence length, words per paragraph,
# passive share, we/our/us, em dashes, semicolons, hedges and citations per
# 1,000 words, taken from the training profile), the style guide, and a
# pointer to references/exemplars.md (the exemplar paragraphs, verbatim).
# The targets are computed in code (no model): 28.6 (sd 13.4), 150, 0.41,
# 3.7, 0.6, 5.1, 3.4 and 11.9, in the order above.
# Give it to any agent with use_skills(my_agent, "skills") (Activity 16).


# ==============================================================================
# STEP 7: WRITE AND REWRITE IN THE STYLE (TWO MODEL CALLS)
# ==============================================================================

brief <- paste(
  "One paragraph for a Discussion section: why the number of embedded dimensions",
  "used to compute derivatives should be tuned before a dynamic network model is",
  "applied to text data, and how a fit index can guide that choice.")
drafted <- keep("style_draft", write_in_style(brief, my_style, model = "groq:openai/gpt-oss-120b"))
cat(drafted)
attr(drafted, "style_distance")
attr(drafted, "cost")

# A plain paragraph (written for this activity) to rewrite in the style:
plain <- paste(
  "We looked at how topics change over time in tweets. Our method found more topics",
  "than the old method. It was faster too. We think this is useful for other kinds",
  "of text, like diaries. But we only tested it on simulated data and one data set,",
  "so more work is needed.")
rewritten <- keep("style_rewrite", write_in_style(plain, my_style,
                                                  model = "groq:openai/gpt-oss-120b",
                                                  mode = "rewrite"))
cat(rewritten)
style_distance(plain, my_style)$distance
attr(rewritten, "style_distance")$distance

# EXPECTED OUTPUT: a single paragraph each time. Where a claim needs a
# source, the text says [CITATION NEEDED] instead of inventing one. The
# rewrite keeps every claim of the plain paragraph and changes only the
# wording. Compare the two distances: the rewrite should be closer to the
# style than the plain paragraph. Distances of short texts are noisy (a
# paragraph is far below the 300-word chunks used for the scale).
# Recorded run (November 22, 2026): draft distance 1.92 (largest z 6.22, for
# transitions_per_1k), cost 0.0023307; plain 4.633849, rewrite 1.987679. The
# draft had one [CITATION NEEDED] but also reused TEFI values of the source
# paper and pointed to a "Figure 3"; the rewrite added "(see Figure 1)".


# ==============================================================================
# STEP 8: CAN A CLASSIFIER TELL GENERATED FROM REAL? (ABOUT 36 MODEL CALLS)
# ==============================================================================

# 8a. An outline of each real Discussion paragraph (content only, no wording).
#     A small model is enough for this stable task.
outlines <- keep("style_outlines", vapply(real_discussion, function(p) {
  ask(paste0("List the main points of this paragraph as 3 to 5 short notes of a few ",
             "words each. Do not copy phrases and do not write sentences.\n\n<paragraph>\n",
             p, "\n</paragraph>"),
      model = "groq:openai/gpt-oss-20b")
}, character(1), USE.NAMES = FALSE))
cat(outlines[1])

# 8b. One paragraph per outline WITH the learned style, and one WITHOUT it
#     (same model, same outline): the baseline that shows what the style adds.
frame <- "One paragraph for the Discussion section of a paper that introduces Dynamic Exploratory Graph Analysis for topic modeling of tweets, covering these notes:\n"
styled <- keep("style_generated", vapply(outlines, function(o) {
  write_in_style(paste0(frame, o), my_style, model = "groq:openai/gpt-oss-120b")
}, character(1), USE.NAMES = FALSE))
unstyled <- keep("style_unstyled", vapply(outlines, function(o) {
  ask(paste0(frame, o, "\nReturn only the paragraph."), model = "groq:openai/gpt-oss-120b")
}, character(1), USE.NAMES = FALSE))

# 8c. The test: real against styled, and real against unstyled.
n <- length(real_discussion)
labels <- rep(1:0, c(n, n))            # 1 = real, 0 = generated
acc_styled   <- loo_accuracy(paragraph_features(c(real_discussion, styled)), labels)
acc_unstyled <- loo_accuracy(paragraph_features(c(real_discussion, unstyled)), labels)
c(styled = acc_styled, unstyled = acc_unstyled)

# 8d. The same comparison in distances, against the reference of Step 4.
c(own_discussion = own_distance$distance,
  styled = style_distance(styled, train_profile)$distance,
  unstyled = style_distance(unstyled, train_profile)$distance)

# EXPECTED OUTPUT: two accuracies between 0 and 1, and three distances. The
# style worked if acc_styled is inside the chance range of Step 4 (0.29 to
# 0.71) and below acc_unstyled, and if the styled distance is close to the
# authors' own 0.58. With 12 paragraphs per group, report the numbers with
# that range, not as a verdict. Recorded run (November 22, 2026): accuracy
# 0.958 (styled) and 0.667 (unstyled); distances 0.58 (own), 1.35 (styled)
# and 1.55 (unstyled). By the rule above the style did not pass: the styled
# paragraphs were told apart from the real ones more often than the unstyled.

# 8e. Optional: a bag-of-words classifier from llmClassificR (the sister
#     package of the text classification course) looks at WORDS instead of
#     style measures: TF-IDF features and a cross-validated logistic regression.
if (requireNamespace("llmClassificR", quietly = TRUE)) {
  bow <- llmClassificR::bow_baseline(c(real_discussion, styled),
                                     rep(c("real", "generated"), each = n), k = 5)
  print(bow$evaluation$accuracy)
  print(head(bow$top_terms, 10))    # the words that give the generated text away
}

# EXPECTED OUTPUT (only if llmClassificR and its optional packages tm,
# glmnet and SnowballC are installed): "Bag-of-words matrix: 24 texts x N
# terms", repeated warnings that a class has fewer than 8 observations
# ("dangerous ground"), an evaluation table with the accuracy, the accuracy
# alone, and the top terms. Recorded run (November 22, 2026): 879 terms,
# accuracy 0.917, and five top terms, all of the class "generated":
# exploratori, graph, indic, see, figure.


# ==============================================================================
# STEP 9: ETHICS AND DOCUMENTATION
# ==============================================================================

# - Learn the style of YOUR OWN writing, or of co-authors who agreed. A
#   learned style is a record of someone's voice.
# - The texts are sent to the model's provider. For unpublished or
#   confidential manuscripts use a local model (Activity 11), for example
#   learn_style("my_papers", model = "ollama:qwen3:8b").
# - Record the use in the AI-use statement of the work: what was learned,
#   from which texts, with which model and when, and which passages were
#   drafted or rewritten. The facts are in the objects:
my_style$model
my_style$date
my_style$sources
my_style$cost
attr(rewritten, "model")
#   The template: read_text(course_template("ai_use_statement"))
# - Never present text written with write_in_style() as unassisted writing.
#   Every claim, number and citation remains the author's responsibility.

# EXPECTED OUTPUT: the model string, the date and time of learn_style(), the
# source labels (the three training files when the data frame of Step 4 is
# used), the cost in US dollars, and the model string of the rewrite.
# Recorded run (November 22, 2026): "groq:openai/gpt-oss-120b" twice,
# "2026-11-22T12:40:03.254-0500" and a cost of 0.0025155.


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Run Steps 1 to 4 on a folder with two or three of your own papers.
#    Which of your measures differs most from the DynEGA paper?
# 2. Learn the style from ALL four files and compare the guide with the one
#    of Step 5. Does holding out the Discussion change the guide?
# 3. Rewrite a paragraph of your own with write_in_style(..., mode =
#    "rewrite") and compare style_distance() before and after.


# ==============================================================================
# EXERCISES (solutions in solutions/17_solutions.R)
# ==============================================================================

# Exercise 1: Compute the mean sentence length of the Discussion by hand
# from the sentences of its first paragraph and compare it with
# style_profile(). Which sentence boundaries could the regular expression
# get wrong? (Offline.)
#
# Exercise 2: Add a rule of your own to the measures: the rate of the words
# "the present study", "the current paper" and "in the current paper" per
# 1,000 words in each section. Which section uses them most? (Offline.)
#
# Exercise 3: Repeat Step 8c with all seven style measures, citations_per_1k
# included. Does the accuracy change, and why?
#
# Exercise 4: Is the difference between acc_styled and acc_unstyled more
# than chance? With the predictions of both classifiers, count the
# paragraphs each got right and use a paired comparison (an exact McNemar
# test on the generated paragraphs, binom.test on the discordant pairs).


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - read_documents() on a PDF needs the pdftools package; on a scanned PDF it
#   returns no text (there is no text layer). Two-column PDFs can mix columns:
#   check a few paragraphs, or export your own papers to .md or .txt.
# - Running heads, page numbers and figure captions in your PDFs inflate the
#   paragraph counts and distort the measures. The shipped corpus was cleaned
#   by hand for that reason.
# - learn_style() stops with "Found fewer than 3 paragraphs of at least 40
#   words": the folder is too small or the reader split paragraphs at every
#   line (lower min_words, or fix the file).
# - The exemplars are picked "by length closest to the median" instead of
#   "embedding centrality": the embedding model (default_model("embed"), a
#   local Ollama model) was not available. The guide still works; start
#   Ollama (Activity 11) for the embedding-based choice. Without Ollama,
#   learn_style() first waits through a few connection retries ("Waiting Ns
#   for retry backoff"): that pause is expected.
# - "N quotations were not found in the source and were removed": the model
#   paraphrased or invented a quotation. The guide is still usable; the
#   removed spots are marked.
# - write_in_style() returns text with [CITATION NEEDED]: by design. Add the
#   real citation yourself.
# - Every generated paragraph is classified correctly (accuracy 1): a single
#   feature may give the generator away (see Exercise 3). Look at
#   paragraph_features() for the two groups side by side.
# - 429 errors in Step 8: the free tier allows a limited number of tokens per
#   minute. keep() saves each finished block; rerun the script and only the
#   missing block is computed again.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# - The corpus is one paper by five authors. Whose style was learned? What
#   would change with five single-author papers by the same person?
# - The instructor's later writing rules forbid em dashes and "we", yet this
#   2022 paper uses both (Steps 2 and 3). A style is a snapshot of a time,
#   a journal and a team, not a fixed property of a person.
# - The classifier sees six measures. A reader sees much more (argument,
#   precision, what is left unsaid). What would a stronger test of "the same
#   style" look like, and who should be the judge?
# - When is it acceptable to submit a paragraph drafted in your own learned
#   style? What must the AI-use statement say, and what must you verify?
