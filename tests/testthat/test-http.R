# The response cache and the messages for requests that cannot be made.
# No model and no network: requests go to a port where nothing listens.

test_that("the cache key ignores optional keys and e-mail addresses in the URL", {
  f <- researchAgentR:::.url_without_secrets
  plain <- "https://api.openalex.org/works?search=x&per_page=5"
  expect_equal(f(paste0(plain, "&api_key=SECRET")), plain)
  expect_equal(f("https://api.openalex.org/works?api_key=SECRET&search=x&per_page=5"), plain)
  expect_equal(f("https://api.crossref.org/works?query=x&mailto=a@b.edu"),
               "https://api.crossref.org/works?query=x")
  expect_equal(f("https://api.unpaywall.org/v2/10.1/x?email=a@b.edu"),
               "https://api.unpaywall.org/v2/10.1/x")
  expect_equal(f(plain), plain)
})

test_that("a missing key stops a live call with a clear message, before any request", {
  withr::local_envvar(TAVILY_API_KEY = "")
  need <- researchAgentR:::.key_needed("tavily", how = "see the setup guide")
  expect_match(need, "TAVILY_API_KEY=your-key", fixed = TRUE)
  expect_match(need, "see the setup guide", fixed = TRUE)
  expect_null(researchAgentR:::.key_needed("tavily", key = "abc"))
  req <- researchAgentR:::.request("http://localhost:9/none", body = list(q = 1), auth = "none")
  expect_error(researchAgentR:::.perform(req, need = need), need, fixed = TRUE)
})

test_that("a recorded response replays without the key", {
  d <- withr::local_tempdir()
  env <- researchAgentR:::.pkg_env
  old_dir <- env$cache_dir
  old <- suppressMessages(use_cache("replay", dir = d))
  on.exit({ suppressMessages(use_cache(old)); env$cache_dir <- old_dir }, add = TRUE)
  req <- researchAgentR:::.request("http://localhost:9/none", body = list(q = 1), auth = "none")
  key <- researchAgentR:::.hash(list(url = req$url, body = req$body$data, method = req$method,
                                     salt = researchAgentR:::.pkg_env$cache_salt))
  researchAgentR:::.cache_write(key, list(body = list(answer = 42), status = 200, latency = 0.1,
                                          headers = list(), recorded_at = "2026-11-22"))
  r <- researchAgentR:::.perform(req, need = "no key")
  expect_true(r$cached)
  expect_equal(r$body$answer, 42)
  # another request has no recording: replay mode says so
  req2 <- researchAgentR:::.request("http://localhost:9/none", body = list(q = 2), auth = "none")
  expect_error(researchAgentR:::.perform(req2, need = "no key"), "No recording")
})

test_that("no answer from a server is reported in plain words", {
  f <- researchAgentR:::.connection_message
  e <- simpleError("Failed to perform HTTP request.")
  e$parent <- simpleError("Couldn't connect to server")
  expect_match(f("http://localhost:11434/api/tags", e), "Ollama is not running")
  expect_match(f("http://localhost:1234/v1/chat/completions", e), "Nothing answered at http://localhost:1234")
  expect_match(f("https://api.groq.com/openai/v1/chat/completions", e),
               "Could not reach https://api.groq.com (Couldn't connect to server)", fixed = TRUE)
  skip_on_cran()
  expect_error(ollama_status("http://localhost:9"), "Nothing answered at http://localhost:9")
})

test_that("OpenAI prompts over 272,000 tokens are priced at the long-context rates", {
  m <- parse_model("openai:gpt-6-luna")
  short <- researchAgentR:::.cost(m, list(input = 272000, cache_read = 0, cache_write = 0, output = 1e6))
  long <- researchAgentR:::.cost(m, list(input = 272001, cache_read = 0, cache_write = 0, output = 1e6))
  expect_equal(short, 272000 * 0.10 / 1e6 + 0.50)
  expect_equal(long, 272001 * 0.20 / 1e6 + 0.75)
})

test_that("live tokens and the largest prompt are tracked per model", {
  env <- researchAgentR:::.pkg_env
  old <- list(env$live_tokens, env$max_prompt)
  on.exit({ env$live_tokens <- old[[1]]; env$max_prompt <- old[[2]] })
  env$live_tokens <- NULL; env$max_prompt <- NULL
  expect_null(researchAgentR:::.session_spend()$live_tokens)
  u <- function(input, output) list(input = input, cache_read = 100, cache_write = 0, output = output)
  researchAgentR:::.track_tokens("groq:openai/gpt-oss-20b", u(900, 50))
  researchAgentR:::.track_tokens("groq:openai/gpt-oss-20b", u(4900, 200))
  researchAgentR:::.track_tokens("ollama:qwen3:4b", u(10, 5))
  sp <- researchAgentR:::.session_spend()
  expect_equal(sp$live_tokens[["groq:openai/gpt-oss-20b"]], 1000 + 50 + 5000 + 200)
  expect_equal(sp$max_prompt[["groq:openai/gpt-oss-20b"]], 5000)
  expect_equal(sp$live_tokens[["ollama:qwen3:4b"]], 115)
})

test_that("a time limit reaches the request: timeout and retries end with the deadline", {
  env <- researchAgentR:::.pkg_env
  old <- env$deadline; on.exit(env$deadline <- old)
  env$deadline <- NULL
  expect_equal(researchAgentR:::.seconds_left(), Inf)
  req <- researchAgentR:::.request("http://localhost:9/none", body = list(q = 1), auth = "none", timeout = 120)
  expect_identical(researchAgentR:::.within_deadline(req), req)          # no deadline: no change

  earlier <- researchAgentR:::.set_deadline(30)
  expect_null(earlier)
  expect_gt(researchAgentR:::.seconds_left(), 28); expect_lte(researchAgentR:::.seconds_left(), 30)
  researchAgentR:::.set_deadline(300)                                     # a looser limit does not replace a tighter one
  expect_lte(researchAgentR:::.seconds_left(), 30)
  researchAgentR:::.set_deadline(Inf); expect_lte(researchAgentR:::.seconds_left(), 30)
  limited <- researchAgentR:::.within_deadline(req)
  expect_lte(limited$options$timeout_ms, 30000)                           # was 120 seconds

  researchAgentR:::.set_deadline(5)
  short <- researchAgentR:::.request("http://localhost:9/none", auth = "none", timeout = 2)
  expect_lte(researchAgentR:::.within_deadline(short)$options$timeout_ms, 2000)   # the request's own, shorter timeout stays

  env$deadline <- Sys.time() - 1                                          # the limit has passed
  expect_error(researchAgentR:::.perform(req, cache = FALSE), "time limit was reached")
})

test_that("a tool call sets the deadline for its requests and restores the earlier one", {
  env <- researchAgentR:::.pkg_env
  old <- env$deadline; on.exit(env$deadline <- old)
  env$deadline <- NULL
  left_inside <- NULL
  slow <- define_tool(function(x) { left_inside <<- researchAgentR:::.seconds_left(); x },
                      "Echo.", args = list(x = arg_string("Text")), name = "echo", timeout = 20)
  out <- call_tool("echo", list(slow), list(x = "a"))
  expect_false(out$is_error)
  expect_lte(left_inside, 20); expect_gt(left_inside, 18)
  expect_null(env$deadline)                                               # restored after the call
})
