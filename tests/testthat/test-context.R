# Context strategies, turn splitting and output helpers. Everything here is
# offline logic, except the two live tests at the end (skipped without a key).

sys <- list(role = "system", content = "You are a research assistant.")
task <- list(role = "user", content = "Find the DOI of the EGA tutorial.")

call_msg <- function(ids, content = "") {
  list(role = "assistant", content = content,
       tool_calls = lapply(ids, function(id) list(id = id, name = "search",
                                                  arguments = list(q = id),
                                                  arguments_raw = paste0("{\"q\":\"", id, "\"}"))))
}
result_msg <- function(id, content = paste("result", id)) {
  list(role = "tool", tool_call_id = id, name = "search", content = content, is_error = FALSE)
}

# A random but valid conversation: turns of one assistant message with 0-3
# tool calls, their results, and sometimes a user nudge.
random_conversation <- function(n_turns, seed) {
  set.seed(seed)
  msgs <- list(sys, task)
  for (t in seq_len(n_turns)) {
    ids <- if (stats::runif(1) < 0.2) character(0) else paste0("c", t, "_", seq_len(sample(1:3, 1)))
    msgs <- c(msgs, list(call_msg(ids, content = paste("turn", t))))
    for (id in ids) msgs <- c(msgs, list(result_msg(id)))
    if (stats::runif(1) < 0.2) msgs <- c(msgs, list(list(role = "user", content = "Please continue.")))
  }
  msgs
}

test_that(".split_turns keeps the head and groups tool results with their call", {
  msgs <- list(sys, task, call_msg(c("a", "b")), result_msg("a"), result_msg("b"),
               call_msg("c"), result_msg("c"), list(role = "user", content = "Use final_answer."),
               list(role = "assistant", content = "Done."))
  p <- .split_turns(msgs)
  expect_equal(p$head, 1:2)
  expect_equal(p$turns, list(3:5, 6:8, 9L))
})

test_that(".split_turns works without a system message and with no turns", {
  expect_equal(.split_turns(list(task))$head, 1L)
  expect_length(.split_turns(list(sys, task))$turns, 0)
  p <- .split_turns(list(task, call_msg("x"), result_msg("x")))
  expect_equal(p$head, 1L)
  expect_equal(p$turns, list(2:3))
  expect_equal(.split_turns(list())$head, integer(0))
})

test_that(".check_tool_pairs finds orphaned results and missing results", {
  expect_length(.check_tool_pairs(list(sys, task, call_msg("a"), result_msg("a"))), 0)
  expect_match(.check_tool_pairs(list(sys, task, result_msg("a"))), "orphaned result a")
  expect_match(.check_tool_pairs(list(sys, task, call_msg(c("a", "b")), result_msg("a"))),
               "no result for call b")
  expect_match(.check_tool_pairs(list(call_msg("a"), call_msg("b"), result_msg("b"))),
               "no result for call a")
})

test_that(".drop_orphans removes only results without a call", {
  msgs <- list(sys, task, result_msg("zz"), call_msg("a"), result_msg("a"))
  out <- .drop_orphans(msgs)
  expect_length(out, 4)
  expect_length(.check_tool_pairs(out), 0)
})

test_that("ctx_window keeps everything while the conversation is short", {
  msgs <- random_conversation(3, 1)
  expect_identical(ctx_window(6)$prepare(msgs, NULL), msgs)
})

test_that("ctx_window keeps the head, a note and the last turns", {
  msgs <- list(sys, task, call_msg("a"), result_msg("a"), call_msg(c("b", "c")),
               result_msg("b"), result_msg("c"), call_msg("d"), result_msg("d"))
  sent <- ctx_window(1)$prepare(msgs, NULL)
  expect_identical(sent[[1]], sys)
  expect_identical(sent[[2]], task)
  expect_equal(sent[[3]]$content, "[5 earlier messages removed to save context]")
  expect_identical(sent[4:5], msgs[8:9])
  expect_equal(ctx_window(1)$name, "window(1)")
  sent0 <- ctx_window(0)$prepare(msgs, NULL)
  expect_length(sent0, 3)
})

test_that("window and notes strategies never break a tool call from its results", {
  notes_file <- tempfile(fileext = ".md")
  notes_write("A note.", notes_file)
  for (seed in 1:40) {
    msgs <- random_conversation(sample(0:12, 1), seed)
    expect_length(.check_tool_pairs(msgs), 0)
    for (strategy in list(ctx_window(0), ctx_window(1), ctx_window(3), ctx_notes(notes_file, 2))) {
      sent <- strategy$prepare(msgs, NULL)
      expect_length(.check_tool_pairs(sent), 0)
      expect_identical(sent[[1]], sys)
      expect_identical(sent[[2]], task)
      if (length(.split_turns(msgs)$turns) && strategy$keep_last > 0) {
        expect_identical(sent[[length(sent)]], msgs[[length(msgs)]])
      }
    }
  }
})

test_that("ctx_compact sends everything below the trigger (no model call)", {
  msgs <- random_conversation(4, 2)
  strat <- ctx_compact(trigger_tokens = 1e6, model = "groq:openai/gpt-oss-20b")
  expect_identical(strat$prepare(msgs, NULL), msgs)
  expect_null(attr(strat$prepare(msgs, NULL), "extra_cost"))
})

test_that("ctx_compact reuses a remembered summary keyed by the messages it covers", {
  msgs <- random_conversation(8, 3)
  p <- .split_turns(msgs)
  strat <- ctx_compact(trigger_tokens = 200, keep_last = 2, model = "groq:openai/gpt-oss-20b")
  covered <- 6
  strat$memo$entries <- list(list(
    n = covered, key = .turns_key(msgs[p$head], msgs[unlist(p$turns[1:covered])]),
    summary = "Found DOI 10.1037/met0000255."))
  sent <- strat$prepare(msgs, NULL)   # under the trigger after compaction: no call
  expect_identical(sent[1:2], msgs[1:2])
  expect_match(sent[[3]]$content, "^Summary of earlier work \\(6 earlier turns")
  expect_match(sent[[3]]$content, "trust=\"untrusted\"", fixed = TRUE)
  expect_match(sent[[3]]$content, "10.1037/met0000255", fixed = TRUE)
  expect_identical(sent[4:length(sent)], msgs[unlist(p$turns[7:8])])
  expect_length(.check_tool_pairs(sent), 0)
  expect_null(attr(sent, "extra_cost"))
  # the same turns under another task get another key, so a summary is never
  # reused across episodes
  other_head <- list(sys, list(role = "user", content = "Another task."))
  expect_false(identical(.turns_key(other_head, msgs[unlist(p$turns[1:covered])]),
                         strat$memo$entries[[1]]$key))
})

test_that(".transcript shows calls, results and errors, and cuts long outputs", {
  msgs <- list(call_msg("a", "Searching."),
               list(role = "tool", tool_call_id = "a", name = "search",
                    content = strrep("x", 20000), is_error = TRUE))
  tr <- .transcript(msgs, max_tool_tokens = 100)
  expect_match(tr, "[assistant] Searching.", fixed = TRUE)
  expect_match(tr, "called search({\"q\":\"a\"})", fixed = TRUE)
  expect_match(tr, "[result of search, ERROR]", fixed = TRUE)
  expect_lt(nchar(tr), 1000)
})

test_that(".record_context adds a context step only when the strategy did work", {
  ep <- structure(list(steps = list()), class = "episode")
  expect_identical(.record_context(ep, list(sys)), ep)
  sent <- structure(list(sys), extra_usage = list(input = 1000, cache_read = 0, cache_write = 0, output = 200),
                    extra_cost = 0.0001, extra_model = "groq:openai/gpt-oss-20b", extra_latency = 0.5)
  ep2 <- .record_context(ep, sent, "compact(6000)")
  expect_length(ep2$steps, 1)
  s <- ep2$steps[[1]]
  expect_equal(s$type, "context")
  expect_equal(s$cost, 1e-4)
  expect_equal(s$input + s$output, 1200)
  expect_false(s$is_error)
})

test_that("ctx_notes shows the notes and provides the note tools", {
  f <- tempfile(fileext = ".md")
  strat <- ctx_notes(f, keep_last = 1)
  expect_equal(vapply(strat$tools, `[[`, "", "name"), c("note_write", "note_read"))
  msgs <- list(sys, task, call_msg("a"), result_msg("a"), call_msg("b"), result_msg("b"))
  expect_length(strat$prepare(msgs, NULL), 5)       # no notes yet: head, removal note, last turn
  call_tool("note_write", strat$tools, arguments = list(text = "DOI 10.1037/met0000255", section = "Sources"))
  sent <- strat$prepare(msgs, NULL)
  expect_match(sent[[3]]$content, "Your notes so far", fixed = TRUE)
  expect_match(sent[[3]]$content, "## Sources", fixed = TRUE)
  expect_match(sent[[4]]$content, "2 earlier messages removed", fixed = TRUE)
  expect_identical(sent[5:6], msgs[5:6])
})

test_that("subagent builds a tool with a clean name and one argument", {
  a <- agent("doi finder!", "Find DOIs.", model = "groq:openai/gpt-oss-20b",
             description = "Finds the DOI of a paper.")
  tool <- subagent(a, summary_words = 50, save_dir = NULL)
  expect_s3_class(tool, "agent_tool")
  expect_equal(tool$name, "doi_finder_")
  expect_equal(names(tool$args), "task")
  expect_match(tool$description, "Finds the DOI", fixed = TRUE)
  expect_error(subagent("not an agent"), "agent")
})

test_that(".cut_words cuts long text and marks the cut", {
  expect_equal(.cut_words("a b c", 5), "a b c")
  expect_equal(.cut_words("a b c d e f", 3), "a b c [... cut at 3 words]")
})

test_that("context_report splits the conversation and keeps billed tokens", {
  ep <- structure(list(
    task = strrep("t", 40), instructions = strrep("s", 400),
    messages = list(list(role = "system", content = strrep("s", 800))),
    steps = list(
      list(step = 1, type = "model", context_tokens = 300, input = 290, cache_read = 0, output = 20,
           cost = 0.001, content = "", tool_calls = "search({\"q\":\"x\"})"),
      list(step = 2, type = "tool", tool = "search", result = "r", result_chars = 4000),
      list(step = 3, type = "context", tool = "compact", model = "m", input = 500, cache_read = 0,
           output = 50, cost = 0.0002),
      list(step = 4, type = "model", context_tokens = 700, input = 400, cache_read = 300, output = 10,
           cost = 0.002, content = "Done.", tool_calls = character(0)))),
    class = "episode")
  r <- context_report(ep)
  expect_equal(nrow(r), 2)
  expect_equal(r$call, 1:2)
  expect_equal(r$system_tokens, c(200, 200))   # from the system message actually sent
  expect_equal(r$task_tokens, c(10, 10))
  expect_equal(r$tool_tokens, c(0, 1000))
  expect_equal(r$assistant_tokens[1], 0)
  expect_gt(r$assistant_tokens[2], 0)
  expect_equal(r$conversation_tokens, r$system_tokens + r$task_tokens + r$assistant_tokens + r$tool_tokens)
  expect_equal(attr(r, "context_steps")$tokens, 550)
})

test_that("filter_output trims data frames by columns, rows and characters", {
  df <- data.frame(title = c(strrep("a", 50), "b", "c"), doi = c("10.1/x", "10.2/y", "10.3/z"), n = 1:3)
  out <- filter_output(df, fields = c("title", "doi", "missing"), max_items = 2, max_chars_per_field = 10)
  expect_equal(names(out), c("title", "doi"))
  expect_equal(nrow(out), 2)
  expect_equal(out$title[1], paste0(strrep("a", 10), "..."))
  expect_match(attr(out, "note"), "unknown fields: missing")
  expect_match(attr(out, "note"), "first 2 of 3 rows")
  txt <- filter_output(df, max_items = 1, as_text = TRUE)
  expect_type(txt, "character")
  expect_match(txt, "[first 1 of 3 rows shown]", fixed = TRUE)
})

test_that("filter_output keeps the head and tail of logs and errors", {
  log <- paste(c("start", paste("line", 1:100), "Error: failed at the end"), collapse = "\n")
  out <- filter_output(log, max_items = 2)
  lines <- strsplit(out, "\n")[[1]]
  expect_equal(lines[1], "start")
  expect_equal(lines[length(lines)], "Error: failed at the end")
  expect_match(out, "[... 98 lines removed ...]", fixed = TRUE)
  long <- strrep("word ", 2000)
  expect_lt(nchar(filter_output(long, max_items = 2, max_chars_per_field = 100)), 1000)
  expect_equal(filter_output(simpleError("boom")), "boom")
})

test_that("filter_output trims vectors and lists of records", {
  v <- filter_output(c(strrep("x", 20), "y", "z"), max_items = 2, max_chars_per_field = 5)
  expect_equal(as.vector(v), c("xxxxx...", "y"))
  recs <- list(list(title = strrep("a", 30), doi = "10.1/x", authors = list("A", "B")),
               list(title = "b", doi = "10.2/y"))
  out <- filter_output(recs, fields = c("title", "authors"), max_items = 1, max_chars_per_field = 10)
  expect_length(out, 1)
  expect_equal(names(out[[1]]), c("title", "authors"))
  expect_equal(out[[1]]$authors, "[\"A\",\"B\"]")
  expect_match(attr(out, "note"), "first 1 of 2 records")
  expect_equal(filter_output(list(a = strrep("z", 9), b = 2), max_chars_per_field = 3), list(a = "zzz...", b = 2))
})

test_that("offload stores an object and recall returns parts of it", {
  df <- data.frame(id = 1:30, title = paste("paper", 1:30), stringsAsFactors = FALSE)
  h <- offload(df, name = "papers_test")
  expect_s3_class(h, "offload_handle")
  expect_match(h, "handle \"papers_test\": data frame, 30 rows x 2 columns", fixed = TRUE)
  expect_equal(recall(h, rows = "3-5")$id, 3:5)
  expect_equal(recall("papers_test", rows = c(1, 30))$id, c(1L, 30L))
  expect_equal(recall("papers_test", pattern = "paper 2[0-9]")$id, 20:29)
  expect_equal(recall("papers_test", pattern = "paper", rows = "1-10", max_items = 4)$id, 1:4)
  expect_match(attr(recall("papers_test", pattern = "paper 1$"), "note"), "1 of 30 items")
  expect_identical(attr(offload(df), "handle"), attr(offload(df), "handle"))   # same content, same handle
  expect_error(recall("no_such_handle"), "No offloaded object")
  expect_error(offload(df, name = "bad name"), "letters")
})

test_that("offload works with text and lists, and the recall tool returns text", {
  h <- offload(paste(c("a", "b", "Error here", "c"), collapse = "\n"))
  expect_match(h, "text, 4 lines")
  expect_equal(as.vector(recall(h, pattern = "error")), "Error here")
  hl <- offload(list(list(doi = "10.1/a"), list(doi = "10.2/b")), name = "recs_test")
  expect_equal(recall(hl, pattern = "10.2")[[1]]$doi, "10.2/b")
  out <- call_tool("recall", offload_tools(), arguments = list(handle = "recs_test", rows = "1"))
  expect_false(out$is_error)
  expect_match(out$output, "10.1/a", fixed = TRUE)
  expect_match(out$output, "[1 of 2 items selected]", fixed = TRUE)
})

test_that(".parse_rows reads ranges and lists", {
  expect_equal(.parse_rows("3-5"), 3:5)
  expect_equal(.parse_rows("1, 4,7:8"), c(1L, 4L, 7L, 8L))
  expect_equal(.parse_rows(c(2, 9)), c(2L, 9L))
  expect_equal(.parse_rows("x"), integer(0))
})

test_that(".extract_facts and .lost_facts find DOIs, numbers and quotes", {
  txt <- paste("The tutorial (doi 10.1037/met0000255.) reports alpha = .85 for N = 1,234",
               "(p < .001) and 3 factors; it says \"the number of dimensions is estimated\".")
  f <- .extract_facts(txt)
  expect_equal(f$value[f$type == "doi"], "10.1037/met0000255")
  expect_setequal(f$value[f$type == "number"], c(".85", "1,234", ".001"))
  expect_equal(f$value[f$type == "quote"], "the number of dimensions is estimated")
  lost <- .lost_facts(txt, "Alpha was .85 with N = 1234; p < .001.")
  expect_setequal(lost$value, c("10.1037/met0000255", "the number of dimensions is estimated"))
  expect_equal(nrow(.lost_facts("no facts here", "x")), 0)
  curly <- .extract_facts("They wrote \u201cnetworks estimate dimensions well\u201d.")
  expect_equal(curly$value, "networks estimate dimensions well")
})

# ---- live tests (need a Groq key) ------------------------------------------------

test_that("ctx_compact summarizes with a real model and reports the cost (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")))
  msgs <- list(sys, task)
  for (t in 1:6) {
    msgs <- c(msgs, list(call_msg(paste0("c", t))),
              list(result_msg(paste0("c", t), paste("Paper", t, "has DOI 10.1000/test", t, strrep("filler ", 200)))))
  }
  strat <- ctx_compact(trigger_tokens = 800, keep_last = 2, model = "groq:openai/gpt-oss-20b")
  sent <- strat$prepare(msgs, NULL)
  expect_match(sent[[3]]$content, "^Summary of earlier work")
  expect_length(.check_tool_pairs(sent), 0)
  expect_true(is.numeric(attr(sent, "extra_cost")))
  # the next step reuses the summary: no new call
  expect_null(attr(strat$prepare(msgs, NULL), "extra_cost"))
})

test_that("compress_text returns derived text and lists lost facts (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")))
  s <- compress_text("Golino and Epskamp (2017), doi 10.1371/journal.pone.0174035, found that EGA recovered the number of factors in 93.7% of conditions.",
                     max_tokens = 60, model = "groq:openai/gpt-oss-20b")
  expect_s3_class(s, "derived_text")
  expect_s3_class(attr(s, "lost"), "data.frame")
})
