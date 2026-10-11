# Cost helpers. Offline tests cover the ledger arithmetic and argument
# checks; the cascade itself calls models and is tested live.

test_that("cost_ledger keeps runtime and offline costs apart", {
  # Hand-made results (not model output).
  tuning <- data.frame(solver = "v2", task_id = c("a", "b"), trial = 1, pass = c(TRUE, FALSE),
                       cost = c(0.004, 0.006), grading_cost = c(0.001, 0.001),
                       tokens = c(5200, 7100), seconds = c(6, 9), model = "groq:openai/gpt-oss-20b",
                       episode_id = c("e1", "e2"))
  deployed <- transform(tuning, cost = c(0.002, 0.003), grading_cost = 0)
  led <- rbind(cost_ledger(tuning, "offline", note = "tuning"), cost_ledger(deployed, "runtime"))
  expect_s3_class(led, "cost_ledger")
  expect_equal(sum(led$cost[led$kind == "offline"]), 0.010)
  expect_equal(sum(led$cost[led$kind == "runtime"]), 0.005)
  expect_equal(sum(led$grading_cost), 0.002)
  expect_output(print(led), "runtime .* 2 rows")
  expect_output(print(led), "offline .* 2 rows")
  expect_error(cost_ledger(tuning, "monthly"))
})

test_that("a cascade answer carries the ledger of every attempt", {
  # Two hand-made attempt rows: a cheap failure, then a verified answer.
  attempts <- data.frame(model = c("groq:openai/gpt-oss-20b", "groq:openai/gpt-oss-120b"),
                         stop_reason = "final_answer", verified = c(FALSE, TRUE),
                         cost = c(0.0004, 0.0021), tokens = c(900, 1500), seconds = c(2, 4),
                         episode_id = c("e1", "e2"))
  ans <- researchAgentR:::.with_ledger("10.1080/00273171.2020.1779642", attempts, list(),
                                       extra_cost = 0.0001, stop_reason = "final_answer")
  expect_equal(attr(ans, "cost"), 0.0026)          # failed attempt and verifier included
  expect_equal(attr(ans, "tokens"), 2400)
  expect_equal(attr(ans, "model"), "groq:openai/gpt-oss-120b")
  expect_true(attr(ans, "verified"))
  expect_equal(as.character(researchAgentR:::.strip_ledger(ans)), "10.1080/00273171.2020.1779642")
  expect_null(attributes(researchAgentR:::.strip_ledger(ans)))
  attempts$cost[1] <- NA
  expect_true(is.na(attr(researchAgentR:::.with_ledger("x", attempts, list(), 0, "final_answer"), "cost")))
})

test_that("verifier results are read as pass and cost", {
  chk <- researchAgentR:::.as_check
  expect_equal(chk(TRUE), list(pass = TRUE, cost = 0))
  expect_equal(chk(NA), list(pass = FALSE, cost = 0))
  expect_equal(chk(list(pass = TRUE, cost = 0.01)), list(pass = TRUE, cost = 0.01))
})

test_that("cascade and baselines check their arguments before any call", {
  expect_error(cascade("gpt-oss-20b", verifier = function(a, t) TRUE), "provider:model")
  expect_error(cascade("groq:openai/gpt-oss-20b", verifier = "yes"), "function")
  expect_true(is.function(cascade("groq:openai/gpt-oss-20b", function(a, t) TRUE)))
  maker <- researchAgentR:::.agent_maker(agent("x", "Do it.", model = "groq:openai/gpt-oss-20b"))
  expect_equal(maker("groq:openai/gpt-oss-120b")$model, "groq:openai/gpt-oss-120b")
  expect_equal(researchAgentR:::.agent_maker(NULL)("groq:openai/gpt-oss-20b")$model,
               "groq:openai/gpt-oss-20b")
  expect_error(researchAgentR:::.agent_maker(42), "agent")
  ts <- task_set("t", "Say x.", grader = grader_exact(), answer = "x")
  expect_error(baselines(function(m) NULL, ts, models = list(cheap = "groq:openai/gpt-oss-20b")),
               "cheap.*strong")
})

test_that("a cascade escalates and records each attempt (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")), "GROQ_API_KEY not set")
  # The verifier never accepts, so both models run: the ledger must show two.
  never <- function(answer, task) FALSE
  solve <- cascade(c("groq:openai/gpt-oss-20b", "groq:openai/gpt-oss-120b"), never,
                   budget = budget(max_steps = 1))
  ans <- solve("Reply with the word ok.")
  expect_equal(nrow(attr(ans, "attempts")), 2)
  expect_equal(attr(ans, "stop_reason"), "unverified")
  expect_equal(attr(ans, "cost"), sum(attr(ans, "attempts")$cost))
})
