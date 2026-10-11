# Offline tests: question building, answer parsing and the tool schema.
# Classification itself calls a paid API and is tested live only when
# TYPESAFE_API_KEY is set.

test_that("questions follow the Jev format", {
  q <- .jev_question("Which agent?", c(lit = "papers", fund = "grants"), "choice")
  expect_equal(q$type, "choice")
  expect_equal(q$criteria, list(lit = "papers", fund = "grants"))
  u <- .jev_question("Which?", c("yes", "no"), "choice")     # unnamed options name themselves
  expect_equal(names(u$criteria), c("yes", "no"))
  n <- .jev_question("The abstract reports original data.", NULL, "noul")
  expect_null(n$criteria)
  n2 <- .jev_question("A statement.", c(true = "it holds", false = "it does not"), "noul")
  expect_equal(n2$criteria, list(true = "it holds", false = "it does not"))
  expect_error(.jev_question("Which?", "only one", "choice"), "between 2 and 255")
  expect_error(.jev_question("Which?", NULL, "choice"), "needs `options`")
  expect_error(.jev_question("Which?", c(a = "x", a = "y"), "choice"), "unique")
  expect_error(.jev_question("", NULL, "noul"), "non-empty")
  expect_error(.jev_question("A statement.", c(yes = "a", no = "b"), "noul"), "true")
})

test_that("answers become rows, and invalid answers become NA", {
  # Answer shapes as returned by POST https://api.typesafe.ai/v1/systemone and
  # parsed by llmClassificR (jev_parse_choice(), jev_parse_noul()).
  choice <- list(type = "choice", choice = "fund", confidence = 0.8,
                 probabilities = list(lit = 0.2, fund = 0.8))
  r <- .jev_row(choice, "choice", c("lit", "fund"))
  expect_equal(r$prediction, "fund")
  expect_equal(r$confidence, 0.8)
  expect_equal(unlist(r[, c("lit", "fund")]), c(lit = 0.2, fund = 0.8))
  odd <- .jev_row(modifyList(choice, list(choice = "other")), "choice", c("lit", "fund"))
  expect_true(is.na(odd$prediction))                       # never a guessed label
  failed <- .jev_row(NULL, "choice", c("lit", "fund"))
  expect_true(all(is.na(failed)))
  noul <- .jev_row(list(type = "noul", noul = 0.3), "noul", c("true", "false"))
  expect_equal(noul$prediction, "false")
  expect_equal(noul$confidence, 0.7)
  expect_equal(noul$true, 0.3)
  expect_equal(noul$false, 0.7)
})

test_that("a missing key gives instructions before any request", {
  withr::local_envvar(TYPESAFE_API_KEY = "")
  skip_if(nzchar(.get_key("typesafe")))
  expect_error(classify_texts("a text", "A statement.", type = "noul"), "TYPESAFE_API_KEY")
})

test_that("the classification tool fixes the question and takes texts", {
  t <- tool_classify("The abstract reports original data from human participants.",
                     type = "noul", name = "screen_empirical", approval = TRUE)
  expect_equal(t$name, "screen_empirical")
  expect_equal(t$schema$required, list("texts"))
  expect_equal(t$schema$properties$texts$type, "array")
  expect_true(t$approval)
  expect_match(t$description, "original data", fixed = TRUE)
  ch <- tool_classify("Which agent?", options = c(lit = "papers", fund = "grants"))
  expect_match(ch$description, "lit, fund", fixed = TRUE)
  expect_error(tool_classify("Which agent?", options = "one"), "between 2 and 255")
})

# ---- Live test -----------------------------------------------------------------

test_that("Jev classifies texts (live, paid)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("TYPESAFE_API_KEY")), "TYPESAFE_API_KEY not set")
  skip_if_offline("api.typesafe.ai")
  res <- classify_texts(c("We surveyed 1,204 adults about loneliness.",
                          "This essay argues for a new theory of measurement."),
                        "The text reports original data collected from people.", type = "noul")
  expect_equal(nrow(res), 2)
  expect_equal(attr(res, "n_failed"), 0)
  expect_true(all(res$true >= 0 & res$true <= 1))
  expect_gt(attr(res, "usage")$input_tokens, 0)
})
