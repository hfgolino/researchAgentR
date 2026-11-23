# Graders are pure R (except grader_judge, tested live at the end). The
# answers below are hand-written strings, not model output.

test_that("DOIs are normalized before comparison", {
  expect_equal(researchAgentR:::.normalize_doi(c(
    "https://doi.org/10.1080/00273171.2020.1779642",
    "doi:10.1080/00273171.2020.1779642",
    "http://dx.doi.org/10.1080/00273171.2020.1779642.",
    "10.1080/00273171.2020.1779642")),
    rep("10.1080/00273171.2020.1779642", 4))
  expect_equal(researchAgentR:::.extract_dois(
    "See https://doi.org/10.1007/S11336-021-09820-Y, and doi:10.1080/00273171.2020.1779642."),
    c("10.1007/s11336-021-09820-y", "10.1080/00273171.2020.1779642"))
})

test_that("grader_doi reports recall and precision", {
  task <- list(id = "t", answer = c("10.1007/s11336-021-09820-y", "10.1080/00273171.2020.1779642"))
  g <- grader_doi()
  both <- g("10.1007/s11336-021-09820-y and https://doi.org/10.1080/00273171.2020.1779642", task)
  expect_true(both$pass)
  expect_equal(both$score, 1)
  one <- g("Only 10.1007/S11336-021-09820-Y", task)
  expect_false(one$pass)
  expect_equal(one$score, 0.5)
  expect_true(grader_doi(min_recall = 0.5)("Only 10.1007/s11336-021-09820-y", task)$pass)
  # An extra, unexpected DOI lowers precision.
  extra <- grader_doi(min_precision = 1)(
    "10.1007/s11336-021-09820-y 10.1080/00273171.2020.1779642 10.9999/not-expected", task)
  expect_false(extra$pass)
  expect_match(extra$reason, "precision 0.67")
  expect_false(g("I could not find it.", task)$pass)
  expect_false(g(NULL, task)$pass)
})

test_that("hashed answers are graded without revealing them", {
  h <- hash_answer("10.1080/00273171.2020.1779642", type = "doi")
  expect_match(h, "^md5:[0-9a-f]{32}$")
  # Known MD5 of the bare string, computed independently with `md5sum`.
  expect_equal(hash_answer("10.1037/met0000255", type = "doi"),
               "md5:7031ea26d741d220d0b71a5c2ef3ade6")
  expect_equal(hash_answer("https://doi.org/10.1080/00273171.2020.1779642", type = "doi"), h)
  task <- list(id = "t", answer = h)
  expect_true(grader_doi()("doi:10.1080/00273171.2020.1779642", task)$pass)
  expect_false(grader_doi()("10.1080/00273171.2020.9999999", task)$pass)
  ht <- list(answer = hash_answer("Multivariate Behavioral Research"))
  expect_true(grader_exact()("multivariate behavioral research.", ht)$pass)
  expect_error(grader_contains()("x", list(answer = h)), "hashed")
})

test_that("grader_exact normalizes case, spaces, quotes and a final period", {
  task <- list(answer = "Multivariate Behavioral Research")
  expect_true(grader_exact()("  'Multivariate   behavioral research.' ", task)$pass)
  expect_false(grader_exact(normalize = FALSE)("multivariate behavioral research", task)$pass)
  expect_true(grader_exact()("Psychometrika", list(answer = list(c("Psychometrika", "Psychometrika (journal)"))))$pass)
  expect_false(grader_exact()("Psychological Methods", task)$pass)
})

test_that("grader_contains checks every pattern and scores the share found", {
  g <- grader_contains(c("pass@k", "pass\\^k", "bootstrap"))
  r <- g("Report pass@k and pass^k.", list())
  expect_false(r$pass)
  expect_equal(r$score, 2 / 3)
  expect_match(r$reason, "bootstrap")
  expect_true(grader_contains()("Cohen's kappa and AGREEMENT", list(answer = c("kappa", "agreement")))$pass)
})

test_that("grader_code accepts logical, numeric and list results", {
  lg <- grader_code(function(answer, task) nchar(answer) > 3)
  expect_true(lg("long", list())$pass)
  expect_false(lg("no", list())$pass)
  num <- grader_code(function(answer, task) 0.4)("x", list())
  expect_false(num$pass)
  expect_equal(num$score, 0.4)
  ls <- grader_code(function(answer, task) list(pass = TRUE, score = 0.9, reason = "fine"))("x", list())
  expect_equal(ls[c("pass", "score", "reason")], list(pass = TRUE, score = 0.9, reason = "fine"))
  undecided <- grader_code(function(answer, task) list(pass = NA, reason = "API down"))("x", list())
  expect_true(is.na(undecided$pass))
  expect_error(grader_code(function(answer, task) "yes")("x", list()), "must return")
  expect_s3_class(lg, "grader")
})

test_that("calibrate_judge matches a hand-computed Cohen's kappa", {
  judge <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  human <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  # By hand: p_o = 8/10; both raters say TRUE 7 times out of 10, so
  # p_e = 0.7 * 0.7 + 0.3 * 0.3 = 0.58 and kappa = (0.8 - 0.58) / 0.42.
  cal <- calibrate_judge(judge, human)
  expect_equal(cal$agreement, 0.8)
  expect_equal(cal$kappa, (0.8 - 0.58) / 0.42)
  expect_equal(nrow(cal$disagreements), 2)
  expect_output(print(cal), "kappa 0.524")
  # Data frames are matched on task_id and trial, whatever their order.
  j <- data.frame(task_id = c("a", "a", "b"), trial = c(1, 2, 1), pass = c(TRUE, FALSE, TRUE))
  h <- data.frame(task_id = c("b", "a", "a"), trial = c(1, 2, 1), pass = c(TRUE, FALSE, TRUE))
  expect_equal(calibrate_judge(j, h)$agreement, 1)
  expect_error(calibrate_judge(c(TRUE, FALSE), TRUE), "same length")
})

test_that("grader_judge freezes its rubric and model", {
  g <- grader_judge("Pass if the answer names two threats to validity.",
                    model = "groq:openai/gpt-oss-120b")
  expect_s3_class(g, "grader")
  expect_equal(attr(g, "model"), "groq:openai/gpt-oss-120b")
  expect_match(attr(g, "rubric_hash"), "^[0-9a-f]{12}$")
  expect_match(attr(g, "label"), attr(g, "rubric_hash"))
  g2 <- grader_judge("Pass if the answer names three threats to validity.",
                     model = "groq:openai/gpt-oss-120b")
  expect_false(identical(attr(g, "rubric_hash"), attr(g2, "rubric_hash")))
})

test_that("grader_judge grades live and records grading cost (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")), "GROQ_API_KEY not set")
  g <- grader_judge("Pass if the answer names at least two threats to internal validity. Fail otherwise.",
                    model = "groq:openai/gpt-oss-120b")
  v <- g("History and maturation.", list(id = "t", instruction = "Name two threats to internal validity."))
  expect_true(is.logical(v$pass))
  expect_true(is.numeric(v$cost))
})

test_that("typographic punctuation is replaced, in general and before grading", {
  x <- "p = .03 on a five‑point scale, “never” to ‘often’, 3–5 items​…"
  expect_equal(plain_punctuation(x), "p = .03 on a five-point scale, \"never\" to 'often', 3-5 items...")
  expect_equal(plain_punctuation(c("a b", NA)), c("a b", NA))
  expect_equal(plain_punctuation("café ≥ 2"), "café ≥ 2")     # accents and symbols are kept
  expect_identical(plain_punctuation(3), 3)
  # A code grader sees the plain answer (text or structured), so a pattern
  # written with ordinary spaces matches what the model wrote.
  g <- grader_code(function(answer, task) grepl("d = 0.71", paste(unlist(answer), collapse = " "), fixed = TRUE))
  expect_true(g("d = 0.71", list())$pass)
  expect_true(g(list(result = "d = 0.71"), list())$pass)
  expect_false(g("d = 0.17", list())$pass)
  expect_true(grader_exact()("Multivariate Behavioral Research", list(answer = "Multivariate Behavioral Research"))$pass)
  expect_true(grader_exact()("test–retest", list(answer = "test-retest"))$pass)
  expect_equal(grader_contains("five-point")("a five‑point scale", list())$score, 1)
  # the graders stored in the benchmark data do the same
  data("research_tasks", package = "researchAgentR", envir = environment())
  i <- which(research_tasks$id == "journal_entropy")
  expect_true(research_tasks$grader[[i]]("Multivariate Behavioral Research", as.list(research_tasks[i, ]))$pass)
})
