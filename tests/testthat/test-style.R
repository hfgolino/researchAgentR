# style_profile() numbers are checked against values computed by hand on
# short texts written for these tests.

test_that("style_profile matches hand-computed values (text A)", {
  # Sentences: "We ran the model." (4 words) and
  # "It was estimated twice; the results were stable." (8 words).
  p <- style_profile("We ran the model. It was estimated twice; the results were stable.")
  expect_s3_class(p, "style_profile")
  expect_equal(p$n_words, 12)
  expect_equal(p$n_sentences, 2)
  expect_equal(p$sentence_mean, 6)
  expect_equal(p$sentence_median, 6)
  expect_equal(p$sentence_sd, sqrt(8))          # ((4-6)^2 + (8-6)^2) / 1
  expect_equal(p$sentence_q25, 5)               # type-7 quantile: 4 + 0.25 * 4
  expect_equal(p$sentence_q75, 7)
  expect_equal(p$words_per_paragraph, 12)
  expect_equal(p$clauses_per_sentence, 1.5)     # 1 + mean(0, 1 semicolon)
  expect_equal(p$passive_share, 0.5)            # "was estimated" in sentence 2
  expect_equal(p$we_per_1k, 1000 / 12)          # "We"
  expect_equal(p$semicolon_per_1k, 1000 / 12)
  expect_equal(p$i_per_1k, 0)
  expect_equal(p$hedges_per_1k, 0)
  expect_equal(p$mattr, 11 / 12)                # "the" twice: 11 types in 12 words
  expect_equal(p$mattr_window, 12)              # text shorter than 500 words
})

test_that("style_profile matches hand-computed values (text B)", {
  p <- style_profile(c(
    "However, results may differ (Lee, 2019; Kim & Park, 2020a). Clearly, I disagree \u2014 see Lee (2019).",
    "Thus our view is shown here."))
  # Words: 7 + 5 + 6 = 18 (numbers and the "a" of 2020a are not words).
  expect_equal(p$n_words, 18)
  expect_equal(p$n_sentences, 3)
  expect_equal(p$n_paragraphs, 2)
  expect_equal(p$sentence_mean, 6)
  expect_equal(p$sentence_sd, 1)
  expect_equal(p$words_per_paragraph, 9)
  # Marks per sentence: 3 commas + 1 semicolon; 1 comma; 0 -> 1 + 5/3.
  expect_equal(p$clauses_per_sentence, 1 + 5 / 3)
  expect_equal(p$passive_share, 1 / 3)          # "is shown"
  per_k <- 1000 / 18
  expect_equal(p$we_per_1k, per_k)              # "our"
  expect_equal(p$i_per_1k, per_k)               # "I"
  expect_equal(p$em_dash_per_1k, per_k)
  expect_equal(p$en_dash_per_1k, 0)
  expect_equal(p$semicolon_per_1k, per_k)
  expect_equal(p$hedges_per_1k, per_k)          # "may"
  expect_equal(p$boosters_per_1k, 2 * per_k)    # "clearly", "shown"
  expect_equal(p$transitions_per_1k, 2 * per_k) # "however", "thus"
  expect_match(p$top_transitions, "however (1)", fixed = TRUE)
  expect_match(p$top_transitions, "thus (1)", fixed = TRUE)
  expect_equal(p$citations_per_1k, 2 * per_k)   # Lee 2019, Kim & Park 2020a; "(2019)" alone is not counted
  expect_equal(p$mattr, 17 / 18)                # "lee" twice
})

test_that("sentence splitting protects abbreviations and counts dashes", {
  p <- style_profile("Scales were compared, e.g. in Fig. 2. The fit was good.")
  expect_equal(p$n_sentences, 2)
  expect_equal(p$sentence_mean, (7 + 4) / 2)
  d <- style_profile("Alpha---beta and gamma--delta are here.")
  expect_equal(d$em_dash_per_1k * d$n_words / 1000, 1)
  expect_equal(d$en_dash_per_1k * d$n_words / 1000, 1)
  # "US" (the country) is not a first-person pronoun; "us" is.
  u <- style_profile("The US sample let us test it.")
  expect_equal(u$we_per_1k, 1000 / 7)
})

test_that("parenthetical citations and MATTR are computed as documented", {
  txt <- "A (Lee, 2019, 2021) and (Kim & Park, 2020a; Ortiz, 2018) but not (2019) or (Table 2)."
  expect_equal(.count_parenthetical_citations(txt), 4)
  expect_equal(unname(.mattr(rep(c("a", "b"), 300), 500)["mattr"]), 2 / 500)
  expect_equal(unname(.mattr(paste0("w", 1:600), 500)["mattr"]), 1)
  # Window 3 over a b a c: windows {a b a} = 2/3 and {b a c} = 3/3.
  expect_equal(unname(.mattr(c("a", "b", "a", "c"), 3)["mattr"]), (2 / 3 + 1) / 2)
})

test_that("style_profile accepts a read_documents-like data frame and stores chunk variability", {
  set.seed(1)
  sentences <- c("The sample was drawn from a larger panel.", "Items were scored on a five-point scale.",
                 "However, the estimates may differ across waves; this is expected.",
                 "Reliability was high in every group.", "We report every model that was fitted.")
  paras <- vapply(1:40, function(i) paste(sample(sentences, 5, replace = TRUE), collapse = " "), "")
  df <- data.frame(file = "a.txt", section = "", paragraph = 1:40, text = paras)
  p <- style_profile(df)
  expect_gt(p$n_words, 900)
  expect_false(is.null(attr(p, "chunk_sd")))
  expect_true(all(names(.distance_floor) %in% names(attr(p, "chunk_sd"))))
})

test_that("style_distance is zero for the same text and grows with difference", {
  ref <- c("The model was estimated twice; the estimates were stable across samples.",
           "However, the second sample may differ (Lee, 2019; Kim & Park, 2020).",
           "The present study reports every model that was fitted.")
  same <- style_distance(ref, ref)
  expect_s3_class(same, "style_distance")
  expect_equal(same$distance, 0)
  near <- style_distance(c("The scale was scored twice; the scores were stable across groups.",
                           "However, the third group may differ (Ortiz, 2018)."), ref)
  far <- style_distance("We love it! We really, really do. I think we nailed it, and our team rocks; we cheer.", ref)
  expect_gte(near$distance, 0)
  expect_gt(far$distance, near$distance)
  expect_true(all(c("measure", "text", "style", "scale", "z") %in% names(far$table)))
  # MATTR is left out when the windows differ (texts shorter than 500 words differ in length).
  expect_false("mattr" %in% far$table$measure)
})

test_that("quotation checks find verbatim spans and reject invented ones", {
  src <- .norm_text("The model was estimated twice;  the estimates were \u201Cstable\u201D across samples.")
  expect_true(.quote_found("estimated twice; the estimates", src))
  expect_true(.quote_found("The model was ... across samples", src))
  expect_true(.quote_found("were \"stable\"", src))
  expect_false(.quote_found("the model converged quickly", src))
  expect_false(.quote_found("across samples ... The model", src))  # wrong order
  guide <- "Uses \"estimated twice\" and \u201Cinvented phrase\u201D often."
  q <- .extract_quotes(guide)
  expect_setequal(q, c("estimated twice", "invented phrase"))
  cleaned <- .remove_quotes(guide, "invented phrase")
  expect_false(grepl("invented phrase", cleaned))
  expect_match(cleaned, "quotation removed")
})

test_that("the plain-text reader drops YAML and code and keeps sections", {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "title: x", "---", "", "# Intro", "", "First paragraph here.", "",
               "```{r}", "x <- 1", "```", "", "# Method", "", "Second paragraph", "continues here."), f)
  d <- .read_plain_paragraphs(f)
  expect_equal(d$text, c("First paragraph here.", "Second paragraph\ncontinues here."))
  expect_equal(d$section, c("Intro", "Method"))
})

test_that("learn_style and write_in_style work live", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")), "GROQ_API_KEY not set")
  sentences <- c("The sample was drawn from a larger panel of adults recruited online.",
                 "Items were scored on a five-point scale and summed into a total score.",
                 "However, the estimates may differ across waves, which is expected given attrition.",
                 "Reliability was high in every group, and the factor structure replicated.",
                 "The present study reports every model that was fitted, including the rejected ones.")
  paras <- vapply(1:6, function(i) paste(sentences[c(i %% 5 + 1, (i + 1) %% 5 + 1, (i + 2) %% 5 + 1)],
                                         collapse = " "), "")
  df <- data.frame(file = "synthetic.txt", section = "", paragraph = 1:6, text = paras)
  st <- learn_style(df, model = "groq:openai/gpt-oss-20b", n_exemplars = 3, min_words = 20)
  expect_s3_class(st, "writing_style")
  expect_true(nzchar(st$guide))
  expect_equal(st$quotes_dropped, sum(!st$quotes$found))
  txt <- write_in_style("One paragraph on why attrition matters in panel studies.", st,
                        model = "groq:openai/gpt-oss-20b")
  expect_s3_class(attr(txt, "style_distance"), "style_distance")
})
