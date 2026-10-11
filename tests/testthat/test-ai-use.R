# CONSTRUCTED TEST FIXTURES, NOT MODEL OUTPUT.
# Episodes are assembled by the core ledger functions (R/episode.R) from
# hand-written inputs, so they have the structure of real episodes. Every
# number and text is invented for testing; none came from a model.

ai_fixture_episode <- function(model = "groq:openai/gpt-oss-120b", replay = FALSE, verify = TRUE) {
  withr::local_options(researchAgentR.verbose = FALSE)
  a <- agent("measure_finder", instructions = "Find validated instruments.", model = model)
  ep <- researchAgentR:::.new_episode(a, "List instruments for grit with a reliability estimate.", budget())
  reply <- function(tool_calls = list(), content = "", cached = FALSE) {
    list(content = content, tool_calls = tool_calls, reasoning = "",
         usage = list(input = 1500, cache_read = 200, cache_write = 0, output = 120, reasoning = 0),
         cost = if (cached) 0 else 0.0004, latency = 1, model = model, stop_reason = "stop",
         cached = cached, notes = character(0), executed_tools = list())
  }
  call <- function(id, name) list(id = id, name = name, arguments = list(q = "grit"),
                                  arguments_raw = "{\"q\":\"grit\"}", parse_error = NULL)
  sent <- list(list(role = "user", content = "task"))
  ep <- researchAgentR:::.record_model(ep, reply(list(call("a", "openalex_search"))), sent, FALSE)
  ep <- researchAgentR:::.record_tool(ep, call("a", "openalex_search"),
                                      list(output = "3 works", is_error = FALSE, latency = 0.5, cost = 0), FALSE)
  if (verify) {
    ep <- researchAgentR:::.record_model(ep, reply(list(call("b", "verify_citations"))), sent, FALSE)
    ep <- researchAgentR:::.record_tool(ep, call("b", "verify_citations"),
                                        list(output = "3 of 3 verified", is_error = FALSE, latency = 2, cost = 0), FALSE)
  }
  ep <- researchAgentR:::.record_model(ep, reply(content = "Short Grit Scale (Duckworth & Quinn, 2009).",
                                                 cached = replay), sent, FALSE)
  researchAgentR:::.finish(ep, list(), "final_answer", "Short Grit Scale")
}

# "US dollars" is fine; "we", "our" and lower-case "us" are not.
no_we <- function(x) !grepl("\\b(we|our)\\b", x, ignore.case = TRUE) && !grepl("\\bus\\b", x)

test_that("the short statement reports what the logs show, impersonally", {
  eps <- list(ai_fixture_episode(), ai_fixture_episode(model = "ollama:qwen3:4b", replay = TRUE))
  s <- ai_use_statement(eps, purpose = "locating measurement instruments for grit")
  expect_s3_class(s, "ai_use_statement")
  expect_match(s, "groq:openai/gpt-oss-120b", fixed = TRUE)
  expect_match(s, "ollama:qwen3:4b", fixed = TRUE)
  expect_match(s, "locating measurement instruments for grit", fixed = TRUE)
  expect_match(s, "2 episodes", fixed = TRUE)
  expect_match(s, "6 model calls", fixed = TRUE)
  expect_match(s, "verify_citations (2)", fixed = TRUE)
  expect_match(s, "OpenAlex", fixed = TRUE)
  expect_match(s, "1 of the 6 model calls was replayed", fixed = TRUE)
  expect_match(s, "3 model calls went to local models and 3 to cloud providers", fixed = TRUE)
  expect_match(s, "[Describe the human review", fixed = TRUE)
  expect_match(s, "[Describe the role of AI", fixed = TRUE)
  expect_true(no_we(s))
  expect_false(grepl("\u2014", s))
  expect_output(print(s), "placeholders in \\[square brackets\\]")
})

test_that("missing verification is stated, never invented", {
  s <- ai_use_statement(ai_fixture_episode(verify = FALSE), purpose = "a test",
                        human_review = "Two authors checked every instrument against its source.")
  expect_match(s, "record no calls to verification tools", fixed = TRUE)
  expect_match(s, "Two authors checked every instrument", fixed = TRUE)
  expect_false(grepl("[Describe the human review", s, fixed = TRUE))
  expect_match(s, "No model call was replayed", fixed = TRUE)
})

test_that("the appendix has a table per episode and can be written to a file", {
  e1 <- ai_fixture_episode()
  e2 <- ai_fixture_episode()
  e2$id <- paste0(e1$id, "-b")
  f <- tempfile(fileext = ".md")
  s <- ai_use_statement(list(e1, e2), purpose = "a test", style = "appendix", file = f)
  expect_match(s, "# Appendix: Use of AI agents", fixed = TRUE)
  expect_match(s, paste("### Episode", e1$id), fixed = TRUE)
  expect_match(s, paste("### Episode", e2$id), fixed = TRUE)
  expect_match(s, "| Step | Type | Model or tool |", fixed = TRUE)
  expect_match(s, "| verify_citations | 2 | 0 |", fixed = TRUE)
  expect_true(no_we(s))
  expect_equal(paste(readLines(f), collapse = "\n"), as.character(unclass(s)))
})

test_that("statements work from saved JSON episodes", {
  d <- tempfile()
  save_episode(ai_fixture_episode(), dir = d)
  s <- ai_use_statement(d, purpose = "a test")
  expect_match(s, "groq:openai/gpt-oss-120b", fixed = TRUE)
  expect_match(s, "1 episode with 3 model calls", fixed = TRUE)
})

test_that("purpose is required", {
  expect_error(ai_use_statement(ai_fixture_episode()), "purpose")
})

test_that("course templates exist and can be copied", {
  expect_setequal(course_template(),
                  c("agent_verification_checklist", "ai_use_statement", "capstone_rubric"))
  for (n in course_template()) {
    p <- course_template(n)
    expect_true(file.exists(p))
    txt <- paste(readLines(p, encoding = "UTF-8"), collapse = "\n")
    expect_true(no_we(txt))
    expect_false(grepl("\u2014|delve", txt))
  }
  expect_equal(basename(course_template("ai_use_statement_template.md")), "ai_use_statement_template.md")
  d <- tempfile()
  withr::local_options(researchAgentR.verbose = FALSE)
  copy <- course_template("capstone_rubric", dest = d)
  expect_true(file.exists(copy))
  expect_error(course_template("capstone_rubric", dest = d), "exists")
  expect_error(course_template("nope"), "Unknown template")
})
