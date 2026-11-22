# Message translation for Ollama's own chat API (no server needed).
test_that("messages are translated for /api/chat", {
  msgs <- list(
    list(role = "system", content = "s"),
    list(role = "user", content = "u"),
    list(role = "assistant", content = "", reasoning = "thought",
         tool_calls = list(list(id = "call_1", name = "word_count", arguments = list(text = "a b")))),
    list(role = "tool", tool_call_id = "call_1", name = "word_count", content = "2"))
  out <- researchAgentR:::.to_ollama_messages(msgs)
  expect_equal(vapply(out, `[[`, "", "role"), c("system", "user", "assistant", "tool"))
  expect_equal(out[[3]]$thinking, "thought")
  expect_equal(out[[3]]$tool_calls[[1]]$`function`$arguments$text, "a b")
  expect_equal(out[[4]]$tool_name, "word_count")
  json <- researchAgentR:::.to_json(out[[3]])
  expect_match(json, '"arguments":\\{"text":"a b"\\}')
})

test_that("ollama uses its native adapter by default", {
  expect_equal(providers()$api[providers()$provider == "ollama"], "ollama")
})

test_that("live: thinking can be switched off in Ollama", {
  skip_if_not(isTRUE(tryCatch("qwen3:4b" %in% ollama_status()$name, error = function(e) FALSE)))
  r <- chat("Reply with OK.", model = "ollama:qwen3:4b", effort = "none", max_tokens = 200)
  expect_equal(nchar(r$reasoning), 0)
  expect_true(nzchar(r$content))
})

test_that("thinking that leaks into the answer is moved to the reasoning", {
  f <- researchAgentR:::.split_thinking
  out <- f("Okay, let me work this out.\nThe user wants one word.\n</think>\n\nOK")
  expect_equal(out$content, "OK")
  expect_match(out$reasoning, "let me work this out")
  both <- f("<think>second thoughts</think>Answer.", reasoning = "first thoughts")
  expect_equal(both$content, "Answer.")
  expect_equal(both$reasoning, "first thoughts\nsecond thoughts")
  plain <- f("A plain answer.", "kept")
  expect_equal(plain, list(content = "A plain answer.", reasoning = "kept"))
})
