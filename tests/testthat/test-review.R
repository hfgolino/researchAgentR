# The manuscript used here is a synthetic fixture written for these tests
# (inst/extdata/synthetic_methods_draft.md): invented text and references.

fixture <- function() system.file("extdata", "synthetic_methods_draft.md", package = "researchAgentR")

test_that("in-text citations are extracted in the documented patterns", {
  txt <- paste("As Novak et al. (2018) and Lee and Kim (2019) showed (see Park, 2021a, p. 4;",
               "Ortiz, 2020, 2022), results in May 2020 differ (Table 2) from (van Dijk, n.d.).")
  ct <- .intext_citations(txt)
  expect_setequal(ct$key, c("novak_2018", "lee_2019", "park_2021a", "ortiz_2020", "ortiz_2022", "dijk_n.d."))
  expect_true("Novak et al. (2018)" %in% ct$span)
  expect_equal(nrow(.intext_citations("No citations (N = 2020) or (Table 3, 2019 2020).")), 0)
})

test_that("reference entries are parsed to first author and year", {
  r <- .parse_reference("van Dijk, T., & Lee, K. (2019). A title. Journal, 1, 1-2.")
  expect_equal(r$key, "dijk_2019")
  expect_equal(.parse_reference("Park, H. (2021a). Title.")$key, "park_2021a")
  expect_equal(.parse_reference("Smith, A. 2018. Title without parentheses.")$key, "smith_2018")
  expect_null(.parse_reference("A line without any year"))
  e <- .reference_entries("Lee, J. (2019). One. Journal. Kim, S. (2020). Two.\nPark, H. (2021). Three.")
  expect_length(e, 3)
})

test_that("checks in code find the planted problems in the synthetic draft", {
  rv <- review_checks(fixture(), confidential = TRUE, run_code = FALSE)
  expect_s3_class(rv, "manuscript_review")
  expect_true(all(rv$source == "check"))
  # Cited but not in the reference list (high), listed but not cited (medium).
  miss <- rv[grepl("Ortiz", rv$suggestion), ]
  expect_equal(nrow(miss), 1)
  expect_equal(miss$severity, "high")
  expect_equal(miss$quote, "(Ortiz, 2020)")
  uncited <- rv[grepl("not cited", rv$suggestion), ]
  expect_equal(nrow(uncited), 1)
  expect_match(uncited$quote, "^Quinn, R\\. \\(2017\\)")
  # Matched citations are not flagged.
  expect_false(any(grepl("Lee|Novak|Park", rv$suggestion)))
  # Style rules: one em dash, two first-person plural forms ("we", "Our"), one banned word.
  expect_equal(sum(grepl("rule: em_dash", rv$suggestion)), 1)
  expect_equal(sum(grepl("rule: first_person_plural", rv$suggestion)), 2)
  expect_equal(sum(grepl("rule: banned_word", rv$suggestion)), 1)
  # The figure label is never referenced; the table label is.
  expect_true("fig:design" %in% rv$quote)
  expect_false("tbl:items" %in% rv$quote)
  # Every quoted span is really in the text; ranking puts high first.
  expect_true(all(rv$found[nzchar(rv$quote)]))
  expect_equal(rv$severity[1], "high")
  expect_equal(rv$id, seq_len(nrow(rv)))
  expect_true(any(grepl("confidential", attr(rv, "notes"))))
})

test_that("style rules are configurable and spare 'US'", {
  r <- default_style_rules(em_dash = FALSE, banned_words = character(0))
  expect_equal(r$rule, "first_person_plural")
  body <- data.frame(file = "f", section = "s", paragraph = 1,
                     text = "The US sample let us test it. Ours was larger.")
  hits <- .check_style_rules(body, r)
  expect_equal(nrow(hits), 2)
  expect_true(all(vapply(hits$quote, grepl, logical(1), x = body$text, fixed = TRUE)))
  expect_equal(nrow(default_style_rules(FALSE, FALSE, character(0))), 0)
})

test_that("near-duplicates are merged and ranked, quotes verified", {
  docs <- data.frame(file = "f.md", section = "Results", paragraph = 1,
                     text = "The results were stable across both samples.")
  rows <- rbind(
    .suggestion("model", "f.md", "Results", "clarity", "low", "results were stable", "Say how stable."),
    .suggestion("model", "f.md", "Results", "clarity", "medium", "The results were stable", "Quantify."),
    .suggestion("model", "f.md", "Results", "methods", "high", "both samples", "Describe the samples."))
  out <- .finalize_review(rows, docs)
  expect_equal(nrow(out), 2)
  expect_equal(out$severity, c("high", "medium"))
  expect_equal(out$merged, c(0L, 1L))
  expect_true(all(out$found))
  empty <- .finalize_review(NULL, docs)
  expect_equal(nrow(empty), 0)
  expect_true(all(c("id", "quote", "found", "merged") %in% names(empty)))
})

test_that("figure and table labels are checked in R Markdown and LaTeX", {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c("See Figure \\@ref(fig:used).", "", "```{r used, fig.cap = 'A'}", "plot(1)", "```",
               "", "```{r unused, fig.cap = 'B'}", "plot(2)", "```"), f)
  out <- .check_labels(f)
  expect_equal(out$quote, "fig:unused")
  tex <- tempfile(fileext = ".tex")
  writeLines(c("\\begin{table}\\label{tab:a}\\end{table}", "\\begin{figure}\\label{fig:b}\\end{figure}",
               "As shown in Table~\\ref{tab:a}."), tex)
  expect_equal(.check_labels(tex)$quote, "fig:b")
})

test_that("confidential reviews refuse remote models before sending anything", {
  expect_error(review_manuscript(fixture(), model = "groq:openai/gpt-oss-20b", confidential = TRUE),
               "confidential")
  expect_true(.require_local("ollama:qwen3:4b"))
})

test_that("review_report writes a self-contained HTML file", {
  rv <- review_checks(fixture(), confidential = TRUE, run_code = FALSE)
  out <- review_report(rv, tempfile(fileext = ".html"))
  html <- paste(readLines(out), collapse = "\n")
  expect_match(html, "<style>", fixed = TRUE)
  expect_false(grepl("<script|<link|src=", html))
  expect_match(html, "(Ortiz, 2020)", fixed = TRUE)
  expect_match(html, "&quot;|&lt;|&amp;|Ortiz")
})

test_that("apply_revisions edits a copy, never the original", {
  f <- tempfile(fileext = ".md")
  original <- c("# Method", "", "We delve into the data.", "The sample was small and", "stable over time.")
  writeLines(original, f)
  rv <- review_checks(f, confidential = TRUE, run_code = FALSE)
  k <- which(grepl("banned_word", rv$suggestion))
  rv$revision[k] <- sub("We delve into", "The present study examines", rv$quote[k])
  # A quote spread over a line break is found with flexible whitespace.
  rv[nrow(rv) + 1, ] <- rv[k, ]
  rv$id[nrow(rv)] <- nrow(rv)
  rv$quote[nrow(rv)] <- "small and stable"
  rv$revision[nrow(rv)] <- "small but stable"
  # A quote that is not in the text is skipped and reported.
  rv[nrow(rv) + 1, ] <- rv[k, ]
  rv$id[nrow(rv)] <- nrow(rv)
  rv$quote[nrow(rv)] <- "a sentence that is not there"
  res <- apply_revisions(rv, f, accept = c(k, nrow(rv) - 1, nrow(rv)))
  expect_s3_class(res, "revision_result")
  expect_equal(readLines(f), original)
  revised <- readLines(res$out)
  expect_true(any(grepl("The present study examines the data.", revised, fixed = TRUE)))
  expect_true(any(grepl("small but stable", revised, fixed = TRUE)))
  expect_equal(res$changes$status, c("applied", "applied", "not_found"))
  expect_equal(res$changes$line[1:2], c(3L, 4L))
  expect_output(print(res), "2 applied")
  expect_error(apply_revisions(rv, f, accept = k), "exists")
  expect_error(apply_revisions(rv, f, accept = k, out = f, overwrite = TRUE), "differ")
  expect_error(apply_revisions(rv, "paper.docx", accept = k), "plain-text")
  expect_error(apply_revisions(rv, f, accept = 999), "No suggestion")
  # Rows without a revision are reported, not applied.
  res2 <- apply_revisions(rv, f, accept = 1, out = tempfile(fileext = ".md"))
  expect_true(res2$changes$status %in% c("no_revision", "applied"))
})

test_that("review_manuscript works live", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")), "GROQ_API_KEY not set")
  rv <- review_manuscript(fixture(), focus = c("methods", "reporting"), model = "groq:openai/gpt-oss-20b",
                          run_code = FALSE)
  expect_s3_class(rv, "manuscript_review")
  model_rows <- rv[rv$source == "model", ]
  expect_true(all(model_rows$found))
  expect_equal(attr(rv, "counts")$quotes_dropped, NROW(attr(rv, "dropped")))
})
