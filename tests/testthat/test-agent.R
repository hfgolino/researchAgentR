
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
