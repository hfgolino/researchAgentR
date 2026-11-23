# Hand-built episodes (data structures, not model output) to check how a
# team's tree of episodes is saved, read and costed.
make_ep <- function(id, cost, child = NA_character_) {
  steps <- list(list(step = 1, type = "model", model = "groq:openai/gpt-oss-20b", input = 100,
                     cache_read = 0, cache_write = 0, output = 10, cost = cost, latency = 1,
                     context_tokens = 100, tool_calls = character(0), executed_tools = character(0)))
  if (!is.na(child)) {
    steps[[2]] <- list(step = 2, type = "tool", tool = "worker", arguments = "{}", latency = 1,
                       cost = 0.5, is_error = FALSE, result = "done", child_episode = child)
  }
  structure(list(id = id, agent = id, model = "groq:openai/gpt-oss-20b", task = "t", steps = steps,
                 stop_reason = "final_answer", started = "2026-10-11T10:00:00", seconds = 2,
                 package_version = "0.1.0", r_version = "R"), class = "episode")
}

test_that("sub-episodes are saved with the lead and restored on read", {
  sub_episodes(clear = TRUE)
  child <- make_ep("child-1", 0.5)
  researchAgentR:::.register_sub_episode(child, "worker")
  lead <- make_ep("lead-1", 0.1, child = "child-1")
  f <- tempfile(fileext = ".json")
  save_episode(lead, f)
  sub_episodes(clear = TRUE)
  expect_equal(nrow(sub_episodes()), 0)
  back <- read_episode(f)
  expect_equal(back$id, "lead-1")
  expect_null(back$sub_episodes)
  expect_equal(sub_episodes("child-1")$id, "child-1")
  sub_episodes(clear = TRUE)
})

test_that("a worker's cost is not counted twice", {
  sub_episodes(clear = TRUE)
  child <- make_ep("child-2", 0.5)
  researchAgentR:::.register_sub_episode(child, "worker")
  lead <- make_ep("lead-2", 0.1, child = "child-2")   # lead total = 0.1 + 0.5 (tool step)
  led <- cost_ledger(list(lead, child))
  expect_equal(nrow(led), 2)
  expect_equal(led$within_parent, c(FALSE, TRUE))
  expect_equal(sum(led$cost[!led$within_parent]), 0.6)
  led2 <- cost_ledger(lead)   # the worker is added from the registry
  expect_equal(nrow(led2), 2)
  st <- ai_use_statement(lead, purpose = "a test")
  expect_equal(attr(st, "facts")$cost, 0.6)
  sub_episodes(clear = TRUE)
})

test_that("an episode without steps gives a typed empty step table", {
  ep <- make_ep("empty", 0); ep$steps <- list()
  d <- episode_steps(ep)
  expect_equal(nrow(d), 0)
  expect_true(all(c("step", "type", "cost") %in% names(d)))
})

test_that("contamination_check reads provider-side tool outputs", {
  ep <- make_ep("browse", 0.01)
  ep$steps[[1]]$executed_tools <- "browser_search"
  ep$steps[[1]]$builtin_outputs <- "Search results: github.com/hfgolino/researchAgentR/data-raw/research_tasks.R"
  hits <- contamination_check(ep, patterns = "data-raw/research_tasks")
  expect_equal(nrow(hits), 1)
  expect_equal(hits$where, "builtin_result")
  expect_equal(hits$tool, "browser_search")
})

test_that("recorded response headers are saved as plain JSON", {
  ep <- make_ep("hdr", 0.01)
  reply <- list(usage = list(input = 1, cache_read = 0, cache_write = 0, output = 1, reasoning = 0),
                cost = 0, content = "x", tool_calls = list(), executed_tools = list(),
                headers = structure(list(`x-ratelimit-limit-tokens` = "8000"), class = "httr2_headers"))
  ep <- researchAgentR:::.record_model(ep, reply, list(list(role = "user", content = "q")), FALSE)
  f <- tempfile(fileext = ".json")
  expect_silent(save_episode(ep, f, children = FALSE))
  expect_equal(read_episode(f)$steps[[2]]$rate_limits$`x-ratelimit-limit-tokens`, "8000")
})

test_that("sub_episodes() and route() return typed empty tables", {
  sub_episodes(clear = TRUE)
  d <- sub_episodes()
  expect_true(all(c("agent", "called_as_tool", "cost") %in% names(d)))
  r <- route(character(0), c(a = "first", b = "second"))
  expect_equal(nrow(r), 0)
  expect_true("route" %in% names(r))
})


test_that("the comparison of one agent with a team counts the workers' tokens", {
  sub_episodes(clear = TRUE)
  researchAgentR:::.register_sub_episode(make_ep("child-3", 0.5), "worker")            # 110 tokens
  researchAgentR:::.register_sub_episode(make_ep("grandchild-3", 0.1), "worker")       # 110 tokens
  researchAgentR:::.register_sub_episode(make_ep("child-4", 0.2, child = "grandchild-3"), "worker")
  expect_equal(researchAgentR:::.worker_tokens(make_ep("alone", 0.1)), 0)
  expect_equal(researchAgentR:::.worker_tokens(make_ep("lead-3", 0.1, child = "child-3")), 110)
  expect_equal(researchAgentR:::.worker_tokens(make_ep("lead-4", 0.1, child = "child-4")), 220)   # workers of workers too
  expect_equal(researchAgentR:::.worker_tokens(make_ep("lead-5", 0.1, child = "not-kept")), 0)
  sub_episodes(clear = TRUE)
})

test_that("compare_single_vs_team gives each run its own place in the cache", {
  skip_on_cran()   # tries a connection (nothing listens on port 9)
  old <- options(researchAgentR.max_tries = 1); on.exit(options(old))
  suppressMessages(register_provider("nothing_here", "http://localhost:9/v1", local = TRUE))
  a <- agent("a", "Answer briefly.", model = "nothing_here:none")
  env <- researchAgentR:::.pkg_env
  env$cache_salt <- "outer"
  cmp <- compare_single_vs_team(a, a, rep("Same task.", 2), budget(max_steps = 1, max_seconds = 20))
  expect_equal(env$cache_salt, "outer"); env$cache_salt <- NULL                   # restored
  expect_equal(cmp$arm, c("single", "single", "team", "team"))
  expect_true(all(c("tokens", "worker_tokens", "total_tokens") %in% names(cmp)))
  expect_equal(cmp$total_tokens, cmp$tokens + cmp$worker_tokens)
  expect_true(all(grepl("^api_error: Nothing answered", cmp$stop_reason)))        # no model here: counted, not dropped
})
