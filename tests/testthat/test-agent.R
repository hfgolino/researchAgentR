
test_that("a provider-rejected tool call is recognized and summarized", {
  e <- simpleError("HTTP 400 Bad Request.\nℹ Tool call validation failed: attempted to call tool 'json' which was not\n  in request.tools")
  expect_true(researchAgentR:::.invalid_tool_call(e))
  expect_false(researchAgentR:::.invalid_tool_call(simpleError("HTTP 401 Unauthorized.")))
  expect_match(researchAgentR:::.error_line(e), "^Tool call validation failed: .*request.tools$")
})

test_that("OpenAlex sort accepts both field:desc and -field", {
  expect_equal(researchAgentR:::.openalex_sort("-cited_by_count"), "cited_by_count:desc")
  expect_equal(researchAgentR:::.openalex_sort("publication_date:desc"), "publication_date:desc")
  expect_null(researchAgentR:::.openalex_sort(NULL))
})

test_that("local models cost zero and twin providers share prices", {
  expect_equal(researchAgentR:::.cost(parse_model("ollama:some-new-model"),
                                      list(input = 100, cache_read = 0, cache_write = 0, output = 10)), 0)
  env <- researchAgentR:::.pkg_env
  old <- env$providers
  on.exit(env$providers <- old)
  register_provider("groq_responses_test", "https://api.groq.com/openai/v1", api = "responses",
                    key_env = "GROQ_API_KEY")
  u <- list(input = 1e6, cache_read = 0, cache_write = 0, output = 0)
  expect_equal(researchAgentR:::.cost(parse_model("groq_responses_test:openai/gpt-oss-120b"), u),
               researchAgentR:::.cost(parse_model("groq:openai/gpt-oss-120b"), u))
})

test_that("an early final_answer is refused with an explanation", {
  r <- researchAgentR:::.early_final(list(id = "c1", name = "final_answer"))
  expect_true(r$is_error)
  expect_match(r$output, "same turn as other tools")
  expect_equal(researchAgentR:::.plain("\033[38;5;232mHTTP 400\033[0m"), "HTTP 400")
})

test_that("a tool that exceeds its time limit cannot stop the R session", {
  busy <- define_tool(function(x) { s <- 0; for (k in 1:5e7) s <- s + k; s }, "Busy work.",
                      args = list(x = arg_string("x")), name = "busy", timeout = 1)
  r <- call_tool("busy", list(busy), arguments = list(x = "a"))
  expect_true(r$is_error)
  expect_match(r$output, "longer than 1 seconds")
  Sys.sleep(1.2)   # no lingering limit: this would error if one were left in force
  expect_true(TRUE)
})

test_that("plain text is not parsed as HTML", {
  x <- html_to_text("Title: A page\nURL Source: https://example.org\nSome text")
  expect_match(as.character(x), "Some text")
})

test_that("a namespaced tool name is matched to the tool", {
  tools <- list(list(name = "final_answer"), list(name = "search_openalex"))
  calls <- list(list(id = "1", name = "functions.final_answer"), list(id = "2", name = "search_openalex"),
                list(id = "3", name = "functions.nope"), list(id = "4", name = "final_answer<|channel|>commentary"))
  out <- researchAgentR:::.clean_tool_calls(calls, tools)
  expect_equal(vapply(out, `[[`, "", "name"),
               c("final_answer", "search_openalex", "functions.nope", "final_answer"))
})

test_that("the forced tool choice follows each API's format", {
  expect_equal(researchAgentR:::.force_tool("groq:openai/gpt-oss-20b", "final_answer")$tool_choice$`function`$name,
               "final_answer")
  expect_equal(researchAgentR:::.force_tool("anthropic:claude-haiku-5-5", "final_answer")$tool_choice$type, "tool")
  expect_length(researchAgentR:::.force_tool("ollama:qwen3:4b", "final_answer"), 0)
  # Anthropic models that always think reject a forced tool (HTTP 400)
  expect_length(researchAgentR:::.force_tool("anthropic:claude-sonnet-5-5", "final_answer"), 0)
  expect_length(researchAgentR:::.force_tool("anthropic:claude-opus-5-5", "final_answer"), 0)
  expect_equal(researchAgentR:::.force_tool("openai:gpt-6-luna", "final_answer")$tool_choice,
               list(type = "function", name = "final_answer"))
})
