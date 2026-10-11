# Offline tests of the multi-agent helpers: construction, arithmetic and
# bookkeeping only. Nothing here calls a model; episode-like lists below are
# built by hand for the arithmetic tests and are not model output.

test_that("router softmax: probabilities from similarities and temperature", {
  sim <- c(literature = 0.80, funding = 0.75, measurement = 0.60)
  p <- .route_probabilities(sim, temperature = 0.05)
  # z = sim / 0.05 = 16, 15, 12; softmax is proportional to exp(0), exp(-1), exp(-4)
  w <- exp(c(0, -1, -4))
  expect_equal(as.numeric(p), w / sum(w))
  expect_equal(sum(p), 1)
  expect_equal(p[1, 1] / p[1, 2], exp(1))   # 0.05 apart at temperature 0.05 -> e times as likely
  # several inputs: one row each, rows sum to one
  m <- rbind(c(0.9, 0.1), c(0.2, 0.2))
  pm <- .route_probabilities(m, temperature = 0.1)
  expect_equal(rowSums(pm), c(1, 1))
  expect_equal(pm[2, ], c(0.5, 0.5))
  # extreme ratios do not overflow
  expect_false(anyNA(.route_probabilities(c(1, -1), temperature = 0.001)))
  # higher temperature flattens the distribution
  expect_lt(max(.route_probabilities(sim, 1)), max(p))
})

test_that("route() needs named routes", {
  expect_error(route("x", c("a", "b")), "named character vector")
})

test_that("checkpoint() uses the option, then the default when not interactive", {
  skip_if(interactive())
  checkpoint_log(clear = TRUE)
  old <- options(researchAgentR.approve = NULL, researchAgentR.verbose = TRUE)
  on.exit(options(old), add = TRUE)
  expect_message(d1 <- checkpoint("Send 40 records on?"), "non-interactive")
  expect_false(d1)
  expect_true(suppressMessages(checkpoint("Continue?", default = TRUE)))

  seen <- NULL
  options(researchAgentR.approve = function(tool, arguments) {
    seen <<- list(tool = tool, arguments = arguments)
    FALSE
  })
  expect_false(checkpoint("Spend $2 on full texts?", data = data.frame(id = 1:3), default = TRUE))
  expect_equal(seen$tool, "checkpoint")
  expect_equal(seen$arguments$message, "Spend $2 on full texts?")
  expect_equal(nrow(seen$arguments$data), 3)

  log <- checkpoint_log(clear = TRUE)
  expect_equal(nrow(log), 3)
  expect_equal(log$source, c("default", "default", "option"))
  expect_equal(log$decision, c(FALSE, TRUE, FALSE))
  expect_true(is.na(log$data[1]))
  expect_equal(nrow(checkpoint_log()), 0)
})

# Hand-built episode-like objects for the summary arithmetic (not model output).
.fake_episode <- function(task, steps, seconds, stop_reason = "final_answer") {
  structure(list(id = paste0("test-", task), agent = "a", model = "groq:openai/gpt-oss-20b",
                 task = task, steps = steps, answer = "x", stop_reason = stop_reason,
                 seconds = seconds, started = "2026-10-11T00:00:00"), class = "episode")
}
.model_step <- function(input, output, cost) {
  list(type = "model", input = input, cache_read = 0, cache_write = 0, output = output,
       cost = cost, context_tokens = input)
}
.tool_step <- function(cost) list(type = "tool", cost = cost, is_error = FALSE)

test_that("fan_out summary: per-task rows, totals, failures and wall-clock time", {
  eps <- list(
    .fake_episode("t1", list(.model_step(100, 20, 0.001), .tool_step(0.0005), .model_step(300, 10, 0.002)), 4),
    .fake_episode("t2", list(.model_step(50, 5, 0.0004)), 1),
    .failed_episode(list(name = "a", model = "groq:openai/gpt-oss-20b"), "t3", "HTTP 429"))
  s <- .fan_out_summary(eps, wall_seconds = 2.5)
  expect_equal(s$tokens, c(430, 55, 0))
  expect_equal(s$cost, c(0.0035, 0.0004, NA))
  expect_equal(s$model_calls, c(2, 1, 0))
  expect_equal(attr(s, "total_cost"), 0.0039)
  expect_equal(attr(s, "summed_seconds"), 5)
  expect_equal(attr(s, "wall_seconds"), 2.5)
  expect_equal(attr(s, "failures"), 1)
  expect_equal(s$stop_reason[3], "error: HTTP 429")
  x <- structure(eps, class = "fan_out", summary = s)
  expect_identical(summary(x), s)
  expect_output(print(x), "summed time 5.0 s \\| wall-clock 2.5 s")
})

test_that("as_tool() wraps an agent as a one-argument tool", {
  a <- agent("lit scout", "Find papers.", model = "groq:openai/gpt-oss-20b",
             description = "Finds papers on a topic.")
  t <- as_tool(a, budget = budget(max_steps = 3, max_seconds = 60))
  expect_s3_class(t, "agent_tool")
  expect_equal(t$name, "lit_scout")
  expect_equal(names(t$args), "task")
  expect_equal(t$schema$required, list("task"))
  expect_true(t$untrusted)
  expect_equal(t$timeout, 90)
  expect_equal(t$description, "Finds papers on a topic.")
  t2 <- as_tool(agent("w", "Do it.", model = "groq:openai/gpt-oss-20b"))
  expect_match(t2$description, "Delegate a self-contained task")
  expect_error(as_tool(list(name = "x")), "must be an agent")
})

test_that("team() gives the lead one tool per worker", {
  w1 <- agent("scout", "Find papers.", model = "groq:openai/gpt-oss-20b", description = "Finds papers.",
              output_schema = list(dois = arg_array("DOIs", arg_string("A DOI"))))
  w2 <- agent("finder", "Find measures.", model = "groq:openai/gpt-oss-20b", description = "Finds measures.")
  lead <- team("brief", "Write a brief.", list(w1, w2), model = "groq:openai/gpt-oss-120b",
               output_schema = list(brief = arg_string("The brief")))
  expect_s3_class(lead, "agent")
  expect_equal(vapply(lead$tools, `[[`, character(1), "name"), c("scout", "finder"))
  expect_match(lead$instructions, "self-contained tasks")
  expect_match(lead$description, "scout, finder")
  expect_equal(names(lead$output_schema), "brief")
  expect_error(team("t", "x", list(1, 2)), "list of agent")
})

test_that("the critic gets the ok/feedback schema", {
  critic <- agent("critic", "Review.", model = "groq:openai/gpt-oss-20b")
  c2 <- .critic_schema(critic)
  expect_equal(names(c2$output_schema), c("ok", "feedback"))
  expect_equal(c2$output_schema$ok$type, "boolean")
  bad <- agent("critic", "Review.", model = "groq:openai/gpt-oss-20b",
               output_schema = list(score = arg_number("Score")))
  expect_error(.critic_schema(bad), "must have the fields")
})

test_that("sub-episodes are kept and retrievable by id", {
  sub_episodes(clear = TRUE)
  ep <- .fake_episode("sub", list(.model_step(10, 2, 0.0001)), 1)
  .register_sub_episode(ep, "scout")
  df <- sub_episodes()
  expect_equal(nrow(df), 1)
  expect_equal(df$called_as_tool, "scout")
  expect_equal(sub_episodes(ep$id)$task, "sub")
  expect_error(sub_episodes("no-such-id"), "No sub-episode")
  sub_episodes(clear = TRUE)
  expect_equal(nrow(sub_episodes()), 0)
})

test_that("live: a team delegates to a worker (Groq)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")))
  old <- options(researchAgentR.verbose = FALSE)
  on.exit(options(old))
  sub_episodes(clear = TRUE)
  adder <- define_tool(function(a, b) a + b, "Add two numbers.",
                       args = list(a = arg_number("First"), b = arg_number("Second")), name = "add")
  worker <- agent("calculator", "Use the add tool and report the sum.", model = "groq:openai/gpt-oss-20b",
                  tools = list(adder), description = "Adds numbers with a tool.")
  lead <- team("lead", "Answer the user's arithmetic question by delegating it.", list(worker),
               model = "groq:openai/gpt-oss-20b", worker_budget = budget(max_steps = 4))
  ep <- run(lead, "What is 17 + 25?", budget(max_steps = 4))
  steps <- Filter(function(s) s$type == "tool", ep$steps)
  expect_true(length(steps) >= 1)
  expect_false(is.na(steps[[1]]$child_episode))
  expect_s3_class(sub_episodes(steps[[1]]$child_episode), "episode")
})
