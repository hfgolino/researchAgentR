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
