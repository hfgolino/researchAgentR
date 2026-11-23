# Live smoke test: calls every network-dependent function of researchAgentR
# once, against the real services, and records what happened.
#
# Run from the package's parent folder with run_live_tests.R (which installs
# the package first). Keys come from ~/.Renviron. Every raw API response is
# saved with use_cache("record") so the response shapes can be inspected and
# the runs replayed offline. Nothing here is mocked.
#
# Output: live_logs/<timestamp>/results.csv, log.txt, objects.rds, cache/

library(researchAgentR)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
out_dir <- file.path(getwd(), "live_logs", stamp)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
use_cache("record", dir = file.path(out_dir, "cache"))
options(researchAgentR.rpm.groq = 20, researchAgentR.verbose = TRUE,
        researchAgentR.approve = function(tool, arguments) TRUE)

results <- list()
objects <- list()
has <- function(var) nzchar(Sys.getenv(var))

# Run one check: never stop the whole run; record status, seconds and detail.
check <- function(id, expr, needs = NULL, summary = function(x) "") {
  if (!is.null(needs) && !all(vapply(needs, has, logical(1)))) {
    results[[id]] <<- data.frame(id = id, status = "SKIP", seconds = NA,
                                 detail = paste("missing", paste(needs, collapse = ", ")))
    cat(sprintf("%-34s SKIP (%s)\n", id, paste(needs, collapse = ", ")))
    return(invisible(NULL))
  }
  t0 <- Sys.time()
  value <- tryCatch(expr, error = function(e) e)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (inherits(value, "error")) {
    status <- "FAIL"; detail <- conditionMessage(value)
  } else {
    status <- "OK"
    detail <- tryCatch(paste(summary(value), collapse = " | "), error = function(e) paste("summary failed:", conditionMessage(e)))
    objects[[id]] <<- value
  }
  detail <- substr(gsub("\\s+", " ", detail), 1, 600)
  results[[id]] <<- data.frame(id = id, status = status, seconds = secs, detail = detail)
  cat(sprintf("%-34s %-4s %6.1fs  %s\n", id, status, secs, substr(detail, 1, 140)))
  invisible(value)
}

ep_summary <- function(ep) {
  t <- researchAgentR:::.totals(ep)
  sprintf("stop=%s calls=%d tools=%d errors=%d tokens=%d cost=%s answer=%s",
          ep$stop_reason, t$model_calls, t$tool_calls, t$tool_errors, as.integer(t$tokens),
          format(t$cost, digits = 3), substr(paste(format(ep$answer), collapse = " "), 1, 200))
}
resp_summary <- function(r) {
  sprintf("content=%s | tool_calls=%d | in=%d cached=%d out=%d | cost=%s | %.1fs | notes=%s",
          substr(r$content, 1, 120), length(r$tool_calls), as.integer(r$usage$input),
          as.integer(r$usage$cache_read), as.integer(r$usage$output), format(r$cost, digits = 3),
          r$latency, paste(r$notes, collapse = "; "))
}

log_con <- file(file.path(out_dir, "log.txt"), open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("researchAgentR", as.character(packageVersion("researchAgentR")), "| httr2",
    as.character(packageVersion("httr2")), "|", R.version.string, "|", stamp, "\n\n")

# ---- 1. Setup -------------------------------------------------------------------
check("verify_setup", verify_setup(test_apis = TRUE),
      summary = function(x) paste(x$check, x$status, sep = "=", collapse = "; "))
for (p in c("groq", "openai", "anthropic")) {
  check(paste0("models_", p), list_provider_models(p), needs = paste0(toupper(p), "_API_KEY"),
        summary = function(x) paste(nrow(x), "models:", paste(head(x$id, 40), collapse = ", ")))
}
check("ollama_status", ollama_status(), summary = function(x) paste(x$name, collapse = ", "))

# ---- 2. Plain chat on every provider ------------------------------------------
chat_models <- c(groq = "groq:openai/gpt-oss-20b", groq = "groq:openai/gpt-oss-120b",
                 groq = "groq:qwen/qwen3.8-27b", openai = "openai:gpt-6-luna",
                 anthropic = "anthropic:claude-haiku-5-5")
for (i in seq_along(chat_models)) {
  m <- chat_models[[i]]
  check(paste0("chat_", m), chat("Reply with exactly one word: OK", model = m, max_tokens = 400),
        needs = paste0(toupper(names(chat_models)[i]), "_API_KEY"), summary = resp_summary)
}

# ---- 3. Structured output -----------------------------------------------------
schema <- list(type = "object", properties = list(answer = list(type = "string"),
               confidence = list(type = "number")), required = list("answer", "confidence"),
               additionalProperties = FALSE)
for (i in c(1, 4, 5)) {
  m <- chat_models[[i]]
  check(paste0("structured_", m), chat("What is the capital of Brazil? Give a confidence from 0 to 1.",
        model = m, output_schema = schema, max_tokens = 600),
        needs = paste0(toupper(names(chat_models)[i]), "_API_KEY"), summary = resp_summary)
}

# ---- 4. The agent loop with a local tool, every provider ------------------------
word_count <- function(text) length(strsplit(trimws(text), "\\s+")[[1]])
counter_tool <- define_tool(word_count, "Count the words in a text.",
                            args = list(text = arg_string("The text to count")))
sentence <- "Exploratory graph analysis estimates the number of dimensions in multivariate data"
expected <- word_count(sentence)
# Wording matters (tools/diag_luna.R): with "Never count by yourself" and a
# field called n_words, gpt-6-luna often counted the words of its OWN answer;
# with "the user's text" it sometimes counted the whole question.
make_counter <- function(m) agent("counter",
  "Count the words of the quoted text in the user's message (only the part between quotes) with the word_count tool. Then call final_answer with the number the tool returned.",
  model = m, tools = list(counter_tool),
  output_schema = list(count = arg_integer("The number that the word_count tool returned for the user's text")))
loop_summary <- function(ep) paste(ep_summary(ep), "| correct =",
                                   identical(as.integer(ep$answer$count), as.integer(expected)))
for (i in seq_along(chat_models)) {
  m <- chat_models[[i]]
  check(paste0("loop_", m), run(make_counter(m), paste0("How many words are in: '", sentence, "'?"),
        budget(max_steps = 6, max_cost = 0.05)),
        needs = paste0(toupper(names(chat_models)[i]), "_API_KEY"), summary = loop_summary)
}
# gpt-oss reasoning pass-back on Groq: does sending `reasoning` back work, and does it matter?
check("loop_groq_pass_reasoning", {
  # restored with `finally`: on.exit() does nothing in an expression that is
  # evaluated at top level, and the option would stay on for every later check
  old <- options(researchAgentR.pass_reasoning.groq = TRUE)
  tryCatch(run(make_counter("groq:openai/gpt-oss-120b"), paste0("How many words are in: '", sentence, "'?"),
               budget(max_steps = 6)),
           finally = options(old))
}, needs = "GROQ_API_KEY", summary = loop_summary)

# ---- 5. Provider built-in tools and remote MCP ---------------------------------
check("groq_browser_search", chat("Find the DOI of the 2022 Psychometrika article on Dynamic Exploratory Graph Analysis by Golino and colleagues. Answer with the DOI only.",
      model = "groq:openai/gpt-oss-120b", tools = list(builtin_tool("browser_search")),
      max_tokens = 2000, effort = "low"),
      needs = "GROQ_API_KEY", summary = function(r) paste(resp_summary(r), "| executed:",
        paste(vapply(r$executed_tools, function(x) paste(names(x), collapse = "/"), ""), collapse = "; ")))
check("openai_web_search", chat("Find the DOI of the 2022 Psychometrika article on Dynamic Exploratory Graph Analysis by Golino and colleagues. Answer with the DOI only.",
      model = "openai:gpt-6-luna", tools = list(builtin_tool("web_search",
      filters = list(allowed_domains = list("springer.com", "doi.org", "psychometrika.org")))),
      max_tokens = 2000), needs = "OPENAI_API_KEY", summary = resp_summary)
register_provider("groq_responses", "https://api.groq.com/openai/v1", api = "responses",
                  key_env = "GROQ_API_KEY")
check("groq_remote_mcp", chat("Using the deepwiki tools, what does the R package in the GitHub repository hfgolino/EGAnet do? One sentence.",
      model = "groq_responses:openai/gpt-oss-120b",
      tools = list(remote_mcp_tool("https://mcp.deepwiki.com/mcp", "deepwiki",
                                   allowed_tools = c("ask_wiki_question"))), max_tokens = 2000),
      needs = "GROQ_API_KEY", summary = resp_summary)

# ---- 6. Literature and verification --------------------------------------------
doi_dev <- "10.1007/s11336-021-09820-y"
check("search_openalex", search_openalex("exploratory graph analysis dimensionality", n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(d$doi, collapse = ", ")))
check("get_work", get_work(doi_dev), summary = function(d) paste(d$title, d$year))
check("get_references", get_references(doi_dev, n = 5), summary = function(d) paste(nrow(d), "rows"))
check("get_citations", get_citations(doi_dev, n = 5), summary = function(d) paste(nrow(d), "rows;", paste(d$cited_by_count, collapse = ",")))
check("search_openalex_sorted", search_openalex("dynamic exploratory graph analysis", n = 3, sort = "-cited_by_count"),
      summary = function(d) paste(nrow(d), "rows;", paste(d$cited_by_count, collapse = ",")))
check("search_crossref", search_crossref("exploratory graph analysis", n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(d$doi, collapse = ", ")))
check("search_semantic_scholar", search_semantic_scholar("exploratory graph analysis", n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(d$title, collapse = " / ")))
check("search_europepmc", search_europepmc("network psychometrics", n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(d$title, collapse = " / ")))
check("search_arxiv", search_arxiv("large language model agents evaluation", n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(d$title, collapse = " / ")))
check("find_open_access", find_open_access(doi_dev), summary = function(d) paste(names(d), d[1, ], collapse = "; "))
check("verify_citations", verify_citations(data.frame(
        doi = c(doi_dev, "10.1080/00273171.2020.1779642", "10.9999/invented.2024.001", doi_dev),
        title = c("Modeling latent topics in social media using dynamic exploratory graph analysis",
                  "Entropy fit indices", "A paper that does not exist", "Modeling latent topics in social media"),
        year = c(2022, 2020, 2024, 2015))),
      summary = function(d) paste(d$doi, d$status, d$detail, sep = ": ", collapse = " || "))
check("check_claims", check_claims(
        data.frame(claim = c("DynEGA was more accurate than LDA in estimating the number of topics.",
                             "DynEGA was invented in 1990."),
                   quote = c("DynEGA is substantially more accurate than several different LDA algorithms when estimating the number of simulated topics",
                             "DynEGA was first proposed in 1990"),
                   source = c("dynega", "dynega")),
        sources = c(dynega = "The results show that DynEGA is substantially more accurate than several different LDA algorithms when estimating the number of simulated topics."),
        model = "groq:openai/gpt-oss-20b"),
      needs = "GROQ_API_KEY", summary = function(d) paste(d$status, collapse = ", "))

scout <- agent("scout", "Find papers with the tools. Return real DOIs only, copied from tool results.",
               model = "groq:openai/gpt-oss-120b",
               tools = list(tool_openalex(n = 5), tool_crossref(n = 5), tool_verify_citations()),
               output_schema = list(papers = arg_array("Papers found", items = arg_object("A paper",
                 fields = list(title = arg_string("Title"), year = arg_integer("Year"), doi = arg_string("DOI"))))))
check("agent_scout", run(scout, "Find two papers on exploratory graph analysis for estimating dimensionality.",
      budget(max_steps = 8, max_cost = 0.05)), needs = "GROQ_API_KEY", summary = ep_summary)

# ---- 7. Funding, web, code, classification -------------------------------------
check("search_nih_projects", search_nih_projects("ecological momentary assessment", fiscal_years = 2025, n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(names(d), collapse = ","), ";", paste(d[[1]], collapse = ", ")))
# The benchmark's NIH grader looks a project up by its number: a project that
# the search above returned must pass, and a number that does not exist must not.
check("nih_grader_by_number", {
  found <- objects[["search_nih_projects"]]
  if (is.null(found) || !nrow(found)) stop("no NIH project to check with (the search above failed)")
  core <- sub("-.*$", "", sub("^[1-9]", "", found$project_num[1]))
  by_number <- search_nih_projects(project_nums = paste0("*", core, "*"), fiscal_years = 2025, n = 10)
  grade <- researchAgentR:::.grader_nih_fy(2025, "ecological momentary assessment")
  real <- grade(paste("Project", found$project_num[1]), list(id = "smoke"))
  fake <- grade("Project 5R01MH000000-01", list(id = "smoke"))
  if (!isTRUE(real$pass)) stop("a project returned by the search did not pass its own grader: ", real$reason)
  if (isTRUE(fake$pass)) stop("a project number that does not exist passed the grader")
  list(core = core, rows = nrow(by_number), reason = real$reason)
}, summary = function(x) paste0(x$core, ": ", x$rows, " row(s) with *number*; ", x$reason))
check("search_nsf_awards", search_nsf_awards("psychometrics", n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(names(d), collapse = ",")))
# Several words: NSF matches any of them, so the package asks for the rarest
# word and keeps the awards that contain every word. The totals NSF reports
# for other ways of writing the query are printed for information.
check("search_nsf_every_word", {
  res <- search_nsf_awards("psychometric machine learning", n = 5)
  total_of <- function(...) attr(search_nsf_awards(..., n = 1), "total")
  list(res = res, any = total_of("psychometric machine learning", match = "any"),
       phrase = total_of("machine learning", match = "phrase"),
       and = total_of("psychometric AND \"machine learning\""))
}, summary = function(x) sprintf(
  "%d rows with every word (read %d of %s awards for '%s') | NSF totals: any word %s; \"machine learning\" %s; with AND %s | %s",
  nrow(x$res), attr(x$res, "scanned"), attr(x$res, "total"), attr(x$res, "query")$keyword,
  x$any, x$phrase, x$and, paste(substr(x$res$title, 1, 40), collapse = " / ")))
gg <- check("search_grants_gov", search_grants_gov("mental health", n = 3),
      summary = function(d) paste(nrow(d), "rows;", paste(names(d), collapse = ",")))
if (is.data.frame(gg) && nrow(gg)) {
  check("grants_gov_opportunity", grants_gov_opportunity(gg[[intersect(c("id", "opportunity_id"), names(gg))[1]]][1]),
        summary = function(x) substr(paste(capture.output(str(x, max.level = 1)), collapse = " "), 1, 400))
}
check("web_search_tavily", web_search("exploratory graph analysis tutorial", n = 3,
      include_domains = c("cran.r-project.org", "github.com")), needs = "TAVILY_API_KEY",
      summary = function(d) paste(nrow(d), "rows;", paste(d$url, collapse = ", "), "| cost", format(attr(d, "cost"))))
check("read_page_direct", read_page("https://cran.r-project.org/web/packages/EGAnet/index.html", max_chars = 2000),
      summary = function(d) substr(paste(d$title, d$chars, substr(d$text, 1, 200)), 1, 400))
check("read_page_jina", read_page("https://cran.r-project.org/web/packages/EGAnet/index.html", max_chars = 2000, via = "jina"),
      summary = function(d) substr(paste(d$title, d$chars, substr(d$text, 1, 200)), 1, 400))
check("run_r_code", run_r_code("x <- rnorm(100); round(mean(x) < 1, 2)"),
      summary = function(x) substr(paste(capture.output(str(x)), collapse = " "), 1, 300))
check("classify_texts_jev", classify_texts(c("I love meeting new people.", "I prefer quiet evenings alone."),
      question = "Which facet does the text express?", options = c(Social = "Enjoys people", Reserved = "Prefers solitude")),
      needs = "TYPESAFE_API_KEY", summary = function(d) paste(d$prediction, collapse = ", "))

# ---- 8. Embeddings ----------------------------------------------------------------
texts <- c("Exploratory graph analysis", "Network psychometrics", "Baking sourdough bread")
for (m in c("jina:jina-embeddings-v3", "voyage:voyage-4", "openai:text-embedding-3-small",
            "ollama:nomic-embed-text", "ollama:all-minilm")) {
  key <- switch(sub(":.*", "", m), jina = "JINA_API_KEY", voyage = "VOYAGE_API_KEY",
                openai = "OPENAI_API_KEY", ollama = NULL)
  if (startsWith(m, "ollama:")) {
    pulled <- tryCatch(sub(":latest$", "", ollama_status()$name), error = function(e) character(0))
    if (!sub("^ollama:", "", m) %in% pulled) {
      results[[paste0("embed_", m)]] <- data.frame(id = paste0("embed_", m), status = "SKIP", seconds = NA,
        detail = paste("not pulled: ollama pull", sub("^ollama:", "", m)))
      cat(sprintf("%-34s SKIP (ollama pull %s)\n", paste0("embed_", m), sub("^ollama:", "", m)))
      next
    }
  }
  check(paste0("embed_", m), embed_texts(texts, model = m), needs = key,
        summary = function(e) paste(dim(e)[1], "x", dim(e)[2], "| cos(1,2) =",
                                    round(sum(e[1, ] * e[2, ]), 3), "cos(1,3) =", round(sum(e[1, ] * e[3, ]), 3)))
}

# ---- 9. Context management --------------------------------------------------------
long <- paste(rep("Golino et al. (2022) proposed DynEGA (doi 10.1007/s11336-021-09820-y) and reported that it was more accurate than LDA in 1,000 simulated conditions.", 12), collapse = " ")
check("compress_text", compress_text(long, max_tokens = 80, model = "groq:openai/gpt-oss-20b"),
      needs = "GROQ_API_KEY", summary = function(x) paste(substr(x, 1, 200), "| lost:", paste(unlist(attr(x, "lost")), collapse = ",")))
compact_agent <- scout
compact_agent$context <- ctx_compact(trigger_tokens = 800, keep_last = 2, model = "groq:openai/gpt-oss-20b")
check("agent_ctx_compact", run(compact_agent, "Find two papers on exploratory graph analysis.", budget(max_steps = 8)),
      needs = "GROQ_API_KEY", summary = function(ep) paste(ep_summary(ep), "| context steps:",
        sum(vapply(ep$steps, function(s) s$type == "context", logical(1)))))

# ---- 10. MCP from R ----------------------------------------------------------------
client <- check("mcp_connect", mcp_connect("https://mcp.deepwiki.com/mcp"),
                summary = function(x) substr(paste(capture.output(print(x)), collapse = " "), 1, 300))
if (inherits(client, "mcp_client")) {
  check("mcp_list_tools", mcp_list_tools(client), summary = function(d) paste(d$name, collapse = ", "))
  check("mcp_call", mcp_call(client, "ask_wiki_question", list(repoName = "hfgolino/EGAnet",
        question = "What does EGAnet do, in one sentence?")), summary = function(x) substr(x$text, 1, 300))
  check("agent_mcp_tools", run(agent("wiki", "Answer using the deepwiki tools.", model = "groq:openai/gpt-oss-120b",
        tools = mcp_as_tools(client, tools = "ask_wiki_question")),
        "What does the GitHub repository hfgolino/EGAnet provide? One sentence.", budget(max_steps = 5)),
        needs = "GROQ_API_KEY", summary = ep_summary)
}

# ---- 11. Multi-agent -------------------------------------------------------------
worker <- agent("finder", "Find one paper with OpenAlex and return its title and DOI.",
                model = "groq:openai/gpt-oss-20b", tools = list(tool_openalex(n = 3)),
                description = "Finds one paper on a topic with OpenAlex.")
check("team", run(team("lead", "Delegate the search to the finder, then report its result.",
      workers = list(worker), model = "groq:openai/gpt-oss-120b"),
      "Find one paper on network psychometrics.", budget(max_steps = 5)),
      needs = "GROQ_API_KEY", summary = ep_summary)
check("route_llm", route("I need funding opportunities for a study on loneliness.",
      routes = c(literature = "Finds published papers", funding = "Finds grants and funded projects"),
      method = "llm", model = "groq:openai/gpt-oss-20b"),
      needs = "GROQ_API_KEY", summary = function(d) paste(d$route, d$reason))
check("fan_out", fan_out(worker, c("Find one paper on DynEGA.", "Find one paper on entropy fit indices."),
      budget = budget(max_steps = 4), parallel = FALSE), needs = "GROQ_API_KEY",
      summary = function(x) substr(paste(capture.output(print(x)), collapse = " "), 1, 300))
writer <- agent("writer", "Write one sentence defining exploratory graph analysis.", model = "groq:openai/gpt-oss-20b")
critic <- agent("critic", "Check the sentence for accuracy and clarity.", model = "groq:openai/gpt-oss-120b",
                output_schema = list(ok = arg_boolean("Is it acceptable?"), feedback = arg_string("What to fix")))
check("critique_loop", critique_loop(writer, critic, "Define exploratory graph analysis in one sentence.", max_rounds = 2),
      needs = "GROQ_API_KEY", summary = function(x) substr(paste(capture.output(str(x, max.level = 1)), collapse = " "), 1, 400))

# ---- 12. Evaluation and cost -----------------------------------------------------
data(research_tasks)
dev <- research_tasks[research_tasks$split == "dev", ][1:2, ]
check("run_eval", run_eval(scout, dev, trials = 2, budget = budget(max_steps = 8)),
      needs = "GROQ_API_KEY", summary = function(r) paste(capture.output(print(eval_summary(r))), collapse = " "))
judge <- grader_judge("Pass if the answer names at least one real psychometric method.", model = "groq:openai/gpt-oss-120b")
check("grader_judge", judge("Exploratory graph analysis and factor analysis.", list(id = "t", instruction = "Name a method")),
      needs = "GROQ_API_KEY", summary = function(x) paste(x$pass, x$score, x$reason))
check("cascade", cascade(c("groq:openai/gpt-oss-20b", "groq:openai/gpt-oss-120b"),
      # a real verifier: the DOI must exist AND match the title (verify_citations)
      verifier = function(answer, task) {
        txt <- paste(unlist(answer), collapse = " ")
        doi <- normalize_doi(regmatches(txt, regexpr("10\\.[0-9]{4,}/\\S+", txt)))
        length(doi) == 1 && !is.na(doi) && verify_citations(data.frame(doi = doi,
          title = "Modeling latent topics in social media using dynamic exploratory graph analysis"))$status == "verified"
      })(
      "Return the DOI of the 2022 Psychometrika article 'Modeling latent topics in social media using Dynamic Exploratory Graph Analysis'. Use your knowledge; answer with the DOI only."),
      needs = "GROQ_API_KEY", summary = function(x) paste(format(x), "| cost", format(attr(x, "cost"))))

# ---- 13. Skills, style, review ---------------------------------------------------
sk_dir <- file.path(out_dir, "skills")
check("create_skill", create_skill("Check that every statistical result is reported in APA style with an effect size and a 95% confidence interval.",
      dir = sk_dir, model = "groq:openai/gpt-oss-120b"), needs = "GROQ_API_KEY",
      summary = function(x) substr(paste(capture.output(print(x)), collapse = " "), 1, 400))
corpus <- system.file("extdata", "style_corpus", package = "researchAgentR")
if (nzchar(corpus)) {
  st <- check("learn_style", learn_style(corpus, model = "groq:openai/gpt-oss-120b", n_exemplars = 4),
              needs = "GROQ_API_KEY", summary = function(x) substr(x$guide, 1, 300))
  if (inherits(st, "writing_style")) {
    check("write_in_style", write_in_style("Two sentences on why researchers should verify citations produced by AI agents.",
          st, model = "groq:openai/gpt-oss-120b"), needs = "GROQ_API_KEY",
          summary = function(x) paste(substr(x, 1, 300), "| distance", format(attr(x, "style_distance")$overall %||% attr(x, "style_distance"))))
  }
}
planted <- system.file("extdata", "planted_errors", "aigenie_excerpt_with_errors.md", package = "researchAgentR")
if (nzchar(planted)) {
  check("review_manuscript", review_manuscript(planted, model = "groq:openai/gpt-oss-120b", run_code = FALSE),
        needs = "GROQ_API_KEY", summary = function(x) substr(paste(capture.output(print(x)), collapse = " "), 1, 400))
}

# ---- 14. Local models (only when Ollama is running) ---------------------------------
ol <- tryCatch(ollama_status(), error = function(e) NULL)
if (!is.null(ol) && nrow(ol)) {
  # every pulled model that is not an embedding model
  for (m in ol$name[!grepl("embed|minilm|bge|e5", ol$name)]) {
    for (eff in c("default", "none")) {
      a <- make_counter(paste0("ollama:", m))
      if (eff == "none") a$effort <- "none"
      check(paste0("loop_ollama_", m, "_think_", eff), run(a,
            paste0("How many words are in: '", sentence, "'?"), budget(max_steps = 6, max_seconds = 240)),
            summary = function(ep) paste(loop_summary(ep), "| seconds =", round(ep$seconds, 1)))
    }
  }
}

# ---- 15. Checks added November 22, 2026 ---------------------------------------------
# (a) Models added to the registry exist and answer (IDs read from the
#     providers' model pages on 2026-11-22).
new_models <- c(openai = "openai:gpt-6.2-sol", openai = "openai:gpt-6.1-terra",
                anthropic = "anthropic:claude-sonnet-5-5", anthropic = "anthropic:claude-fable-5-2")
for (i in seq_along(new_models)) {
  m <- new_models[[i]]
  check(paste0("chat_", m), chat("Reply with exactly one word: OK", model = m, max_tokens = 400),
        needs = paste0(toupper(names(new_models)[i]), "_API_KEY"), summary = resp_summary)
}
# (b) The final-answer call when the step budget runs out (budget(final_call =
#     TRUE)), on every API: forced through tool choice where the provider
#     accepts it (Groq, OpenAI, Claude Haiku), by instruction alone where it
#     does not (Claude Sonnet answers a forced tool with HTTP 400).
#     Expected: stop=max_steps_final_answer and the correct count.
final_models <- c(groq = "groq:openai/gpt-oss-20b", groq = "groq:openai/gpt-oss-120b",
                  openai = "openai:gpt-6-luna", anthropic = "anthropic:claude-haiku-5-5",
                  anthropic = "anthropic:claude-sonnet-5-5")
for (i in seq_along(final_models)) {
  m <- final_models[[i]]
  check(paste0("final_call_", m), run(make_counter(m), paste0("How many words are in: '", sentence, "'?"),
        budget(max_steps = 1, max_cost = 0.05, final_call = TRUE)),
        needs = paste0(toupper(names(final_models)[i]), "_API_KEY"), summary = loop_summary)
}
# (c) Provider-side web search limited to chosen domains, through builtin_tool():
#     Anthropic's web search tool (US$10 per 1,000 searches; max_uses caps it).
check("anthropic_web_search_domains", chat(
  "Which NIH institute funds most research on aging? Answer in one sentence and name the page you used.",
  model = "anthropic:claude-haiku-5-5", max_tokens = 1500,
  tools = list(builtin_tool("web_search_20250305", name = "web_search", max_uses = 1,
                            allowed_domains = list("nih.gov")))),
  needs = "ANTHROPIC_API_KEY", summary = function(r) paste(resp_summary(r), "| blocks:",
    paste(vapply(r$executed_tools, function(x) if (is.null(x$type)) "?" else x$type, ""), collapse = ", ")))
# (d) A live call without its key stops with the key's name, before any request.
check("missing_key_message", {
  env <- researchAgentR:::.pkg_env
  old_env <- Sys.getenv("TAVILY_API_KEY"); old_key <- env$keys$tavily
  Sys.setenv(TAVILY_API_KEY = ""); env$keys$tavily <- NULL
  msg <- tryCatch(web_search("a query that was never recorded 20261122", n = 1),
                  error = function(e) conditionMessage(e),
                  finally = { Sys.setenv(TAVILY_API_KEY = old_env); env$keys$tavily <- old_key })
  stopifnot(is.character(msg), grepl("TAVILY_API_KEY", msg))
  msg
}, summary = function(x) x)

# ---- Save ---------------------------------------------------------------------------
res <- do.call(rbind, results)
write.csv(res, file.path(out_dir, "results.csv"), row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(objects, file.path(out_dir, "objects.rds"))
cat("\n", sum(res$status == "OK"), "OK |", sum(res$status == "FAIL"), "FAIL |",
    sum(res$status == "SKIP"), "SKIP. Results in", out_dir, "\n")
sink()
close(log_con)
