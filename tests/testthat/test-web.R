# Offline tests: HTML to text, domain filtering, URL checks and tool schemas.
# Live tests (real searches and page reads) need the network and, for
# Tavily, a key; they are skipped otherwise.

page <- paste0(
  "<html><head><title>Demo &amp; Test</title><script>var secret = 1;</script>",
  "<style>p { color: red; }</style></head><body>",
  "<nav>Home | About</nav>",
  "<main><h1>Results</h1><h2>Reliability</h2>",
  "<p>Cronbach&#39;s alpha was &#945; = .91 &amp; stable.</p>",
  "<ul><li><p>Item 1</p></li><li>Item 2</li></ul></main>",
  "<footer>Copyright 2026</footer></body></html>")

test_that("html_to_text keeps content and drops scripts, styles, navigation and footers", {
  skip_if_not_installed("xml2")
  x <- html_to_text(page, engine = "xml2")
  expect_equal(attr(x, "title"), "Demo & Test")
  expect_match(x, "# Results", fixed = TRUE)
  expect_match(x, "## Reliability", fixed = TRUE)
  expect_match(x, "Cronbach's alpha was \u03b1 = .91 & stable.", fixed = TRUE)
  expect_match(x, "- Item 1", fixed = TRUE)
  expect_false(grepl("secret|color: red|Home \\| About|Copyright", x))
  # A <p> inside an <li> is printed once, not twice.
  expect_equal(lengths(regmatches(x, gregexpr("Item 1", x))), 1)
})

test_that("the regular-expression fallback gives the same text on simple pages", {
  x <- html_to_text(page, engine = "regex")
  expect_equal(attr(x, "title"), "Demo & Test")
  expect_match(x, "# Results", fixed = TRUE)
  expect_match(x, "## Reliability", fixed = TRUE)
  expect_match(x, "Cronbach's alpha was \u03b1 = .91 & stable.", fixed = TRUE)
  expect_match(x, "- Item 2", fixed = TRUE)
  expect_false(grepl("secret|color: red|Home \\| About|Copyright|Demo & Test", x))
  skip_if_not_installed("xml2")
  expect_identical(as.character(x), as.character(html_to_text(page, engine = "xml2")))
})

test_that("pages without block elements still give text", {
  skip_if_not_installed("xml2")
  x <- html_to_text("<html><body><div>Only <b>a div</b> here</div></body></html>")
  expect_equal(as.character(x), "Only a div here")
})

test_that("domains match themselves and their subdomains only", {
  expect_equal(.domain_of(c("https://www.NIH.gov/news?x=1", "http://user@osf.io:8080/abc",
                            "apa.org")), c("nih.gov", "osf.io", "apa.org"))
  urls <- c("https://www.nih.gov/news", "https://example.com/a",
            "https://pubmed.ncbi.nlm.nih.gov/123", "https://notnih.gov")
  res <- data.frame(url = urls, title = letters[1:4])
  kept <- filter_domains(res, include_domains = "nih.gov")
  expect_equal(kept$url, urls[c(1, 3)])
  expect_equal(attr(kept, "n_filtered"), 2)
  dropped <- filter_domains(res, exclude_domains = c("https://example.com", "notnih.gov"))
  expect_equal(dropped$url, urls[c(1, 3)])
  both <- filter_domains(res, include_domains = "nih.gov", exclude_domains = "pubmed.ncbi.nlm.nih.gov")
  expect_equal(both$url, urls[1])
  expect_equal(nrow(filter_domains(res)), 4)
})

test_that("local and private addresses are refused before any request", {
  expect_error(read_page("http://localhost:11434/api/tags"), "local or private")
  expect_error(read_page("http://169.254.169.254/latest/meta-data"), "local or private")
  expect_error(read_page("http://192.168.1.1"), "local or private")
  expect_error(read_page("file:///etc/passwd"), "http")
})

test_that("search engines explain what is missing", {
  withr::local_envvar(TAVILY_API_KEY = "", BRAVE_API_KEY = "")
  withr::local_options(researchAgentR.searxng_url = NULL)
  skip_if(nzchar(.get_key("tavily")))   # a key set in this session with set_tavily_key()
  expect_error(web_search("x"), "TAVILY_API_KEY")
  expect_error(web_search("x", engine = "searxng"), "searxng_url")
  expect_error(web_search("x", engine = "brave"), "BRAVE_API_KEY")
  expect_error(web_search(""), "non-empty")
})

test_that("tool_text keeps the full result for R and short text for the model", {
  df <- data.frame(a = 1:3)
  v <- .tool_text(c("line 1", "line 2"), data = df, cost = 0.008)
  expect_equal(as.character(.as_text(v)), "line 1\nline 2")
  expect_identical(attr(v, "data"), df)
  expect_equal(attr(v, "cost"), 0.008)
})

test_that("web tools have the expected schemas and flags", {
  s <- tool_web_search(include_domains = c("nih.gov", "apa.org"))
  expect_s3_class(s, "agent_tool")
  expect_equal(s$name, "web_search")
  expect_equal(s$schema$required, list("query"))
  expect_equal(s$schema$properties$domains$type, "array")
  expect_true(s$untrusted)
  expect_match(s$description, "nih.gov, apa.org", fixed = TRUE)
  p <- tool_read_page(max_chars = 4000)
  expect_equal(p$name, "read_page")
  expect_true(p$untrusted)
  expect_equal(p$schema$required, list("url"))
})

test_that("the model cannot widen the researcher's domain allow list", {
  s <- tool_web_search(include_domains = "nih.gov")
  out <- call_tool("web_search", list(s), list(query = "x", domains = list("example.com")))
  expect_true(out$is_error)
  expect_match(out$output, "outside the allowed list")
})

# ---- Live tests ------------------------------------------------------------

test_that("Tavily search returns results and a cost (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("TAVILY_API_KEY")), "TAVILY_API_KEY not set")
  skip_if_offline("api.tavily.com")
  res <- web_search("exploratory graph analysis", n = 3, include_domains = "nih.gov")
  expect_true(all(c("title", "url", "content", "score", "engine") %in% names(res)))
  expect_lte(nrow(res), 3)
  expect_true(all(.in_domains(res$url, "nih.gov")))
  expect_gte(attr(res, "cost"), 0)
})

test_that("read_page reads a real page (live)", {
  skip_on_cran()
  skip_if_offline("cran.r-project.org")
  p <- read_page("https://cran.r-project.org/web/packages/EGAnet/index.html")
  expect_equal(nrow(p), 1)
  expect_match(p$text, "EGAnet")
})

test_that("both HTML engines decode named and numeric entities alike", {
  h <- "<html><title>A &amp; B</title><body><p>Caf&eacute; &#233; &alpha; = .91 &le; 1 &amp;lt;</p></body></html>"
  a <- html_to_text(h, engine = "regex")
  expect_equal(as.character(a), "Café é α = .91 ≤ 1 &lt;")
  expect_equal(attr(a, "title"), "A & B")
  skip_if_not_installed("xml2")
  expect_equal(as.character(html_to_text(h, engine = "xml2")), as.character(a))
})

test_that("save_table writes a BOM and plain punctuation", {
  d <- data.frame(a = "Five‑point ‘never’ ≥ 3 items", n = 1)
  f <- save_table(d, tempfile(fileext = ".csv"))
  expect_equal(readBin(f, "raw", 3), as.raw(c(0xEF, 0xBB, 0xBF)))
  back <- read.csv(f, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  expect_equal(back$a, "Five-point 'never' ≥ 3 items")
  expect_equal(names(back), c("a", "n"))
})
