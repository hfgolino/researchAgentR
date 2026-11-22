# Web tools: search the web (Tavily, a self-hosted SearXNG, or Brave) and read
# a web page as plain text. Both are classic agent tools and both bring text
# from strangers into the context, so their outputs are always marked as
# untrusted data (see define_tool()).
#
# This file also holds two small helpers shared by the funding, web, code and
# classification tools.

# ---- Shared helpers ------------------------------------------------------------

# The tools in this module give the model a SHORT text and keep the full
# result for R. call_tool() sends `.as_text(value)` to the model; for a
# character string that is the string itself, and the whole object (with its
# attributes) stays available as the R value. So a tool returns text with the
# full data frame in attr(, "data") and, when the service charges, the dollar
# cost in attr(, "cost") (recorded in the episode ledger).
.tool_text <- function(text, data = NULL, cost = NULL) {
  out <- paste(text, collapse = "\n")
  if (!is.null(data)) attr(out, "data") <- data
  if (!is.null(cost)) attr(out, "cost") <- cost
  out
}

# Pick a value from a parsed JSON list, or NA when it is missing. APIs often
# omit empty fields, so every parser in this module reads fields this way.
.field <- function(x, ..., default = NA) {
  for (nm in c(...)) {
    if (!is.list(x)) return(default)
    x <- x[[nm]]
  }
  if (is.null(x) || length(x) == 0) return(default)
  if (is.list(x)) x <- unlist(x)
  if (length(x) > 1) x <- paste(x, collapse = "; ")
  x
}

# A single value, or a fallback when it is NULL or NA (for printing).
`%|na|%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# ---- Domains ---------------------------------------------------------------

# The host of a URL without "www.", in lower case: "https://www.nih.gov/x" ->
# "nih.gov". Also accepts bare domains ("apa.org").
.domain_of <- function(url) {
  host <- sub("^[a-zA-Z][a-zA-Z0-9+.-]*://", "", url)
  host <- sub("[/?#].*$", "", host)
  host <- sub("^[^@]*@", "", host)          # drop user:password@
  host <- sub(":[0-9]+$", "", host)         # drop :port
  sub("^www\\.", "", tolower(host))
}

# Does each URL belong to one of the domains? A domain also covers its
# subdomains: "nih.gov" matches "pubmed.ncbi.nlm.nih.gov" but not "notnih.gov".
.in_domains <- function(urls, domains) {
  hosts <- .domain_of(urls)
  domains <- .domain_of(domains)
  vapply(hosts, function(h) any(h == domains | endsWith(h, paste0(".", domains))),
         logical(1), USE.NAMES = FALSE)
}

#' Keep or drop search results by domain
#'
#' Applies an allow list (`include_domains`) and a block list
#' (`exclude_domains`) to a data frame of results with a `url` column. A
#' domain also covers its subdomains: `"nih.gov"` keeps
#' `pubmed.ncbi.nlm.nih.gov` but not `notnih.gov`. [web_search()] always
#' applies the lists in R, even when the search engine also filters, so the
#' rule a researcher sets is guaranteed whatever the engine does.
#'
#' @param results A data frame with a `url` column.
#' @param include_domains Optional character vector: keep only these domains.
#' @param exclude_domains Optional character vector: drop these domains.
#' @return The filtered data frame, with the number of rows removed in the
#'   attribute `"n_filtered"`.
#' @examples
#' res <- data.frame(url = c("https://www.nih.gov/news", "https://example.com/a",
#'                           "https://pubmed.ncbi.nlm.nih.gov/123", "https://notnih.gov"))
#' filter_domains(res, include_domains = "nih.gov")
#' filter_domains(res, exclude_domains = c("example.com"))
#' @export
filter_domains <- function(results, include_domains = NULL, exclude_domains = NULL) {
  keep <- rep(TRUE, nrow(results))
  if (length(include_domains)) keep <- keep & .in_domains(results$url, include_domains)
  if (length(exclude_domains)) keep <- keep & !.in_domains(results$url, exclude_domains)
  out <- results[keep, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "n_filtered") <- sum(!keep)
  out
}

# ---- Web search --------------------------------------------------------------

# Tavily list price (pay-as-you-go), used to put a dollar figure in the ledger.
# Free plan: 1,000 credits per month; a basic search uses 1 credit, an
# advanced search 2. https://docs.tavily.com/documentation/api-credits
.tavily_price_per_credit <- 0.008

.empty_search <- function() {
  data.frame(title = character(0), url = character(0), content = character(0),
             score = numeric(0), engine = character(0), stringsAsFactors = FALSE)
}

#' Search the web
#'
#' Sends one query to a web search engine and returns the results as a data
#' frame. Three engines are supported:
#'
#' * `"tavily"` (default): a search API built for agents.
#'   `POST https://api.tavily.com/search` with the key from `TAVILY_API_KEY`.
#'   The free plan gives 1,000 credits per month; a basic search uses 1
#'   credit and an advanced search 2 (<https://docs.tavily.com/documentation/api-credits>).
#'   The dollar cost at the pay-as-you-go list price (US$0.008 per credit) is
#'   stored in `attr(result, "cost")` so the episode ledger shows what the
#'   search would cost beyond the free plan. Rate limit: 100 requests per
#'   minute for development keys (<https://docs.tavily.com/documentation/rate-limits>).
#' * `"searxng"`: a SearXNG metasearch instance, free when you run it
#'   yourself (for example with Docker). Set its address with
#'   `options(researchAgentR.searxng_url = "http://localhost:8080")`. The
#'   JSON format must be enabled under `search: formats:` in the instance's
#'   `settings.yml`, otherwise it answers 403
#'   (<https://docs.searxng.org/dev/search_api.html>). Many public instances
#'   disable JSON; do not point an agent at someone else's instance.
#' * `"brave"`: the Brave Search API (`X-Subscription-Token` header, key in
#'   the environment variable `BRAVE_API_KEY`). Its terms forbid storing or
#'   caching results beyond transient use and using them to benchmark or
#'   improve AI models (Section 3(b) of
#'   <https://api-dashboard.search.brave.com/terms-of-service>), so Brave
#'   requests always bypass [use_cache()] and Brave results must not be used
#'   in an evaluation benchmark.
#'
#' Domain lists are always applied in R as well (see [filter_domains()]),
#' because SearXNG and Brave have no domain filter parameter and an allow
#' list is a research decision that should not depend on the engine.
#'
#' @param query The search query.
#' @param n Maximum number of results (Tavily allows up to 20, Brave 20).
#' @param include_domains Optional character vector of domains to keep, for
#'   example `c("nih.gov", "apa.org")` (Tavily accepts up to 300).
#' @param exclude_domains Optional character vector of domains to drop
#'   (Tavily accepts up to 150).
#' @param engine `"tavily"`, `"searxng"` or `"brave"`.
#' @param depth Tavily search depth: `"basic"` (1 credit) or `"advanced"`
#'   (2 credits). Ignored by the other engines.
#' @return A data frame with `title`, `url`, `content` (the snippet),
#'   `score` (the engine's relevance score, or `NA`) and `engine`, with
#'   attributes `query`, `engine`, `n_filtered` (results removed by the
#'   domain lists), `cost` (US dollars, Tavily only; 0 when cached),
#'   `credits` (Tavily) and `retrieved_at`.
#' @examples
#' \dontrun{
#' web_search("exploratory graph analysis tutorial", n = 5)
#' web_search("PROMIS anxiety short form reliability",
#'            include_domains = c("healthmeasures.net", "nih.gov"))
#' options(researchAgentR.searxng_url = "http://localhost:8080")
#' web_search("network psychometrics", engine = "searxng")
#' }
#' @export
web_search <- function(query, n = 5, include_domains = NULL, exclude_domains = NULL,
                       engine = c("tavily", "searxng", "brave"),
                       depth = c("basic", "advanced")) {
  engine <- match.arg(engine)
  depth <- match.arg(depth)
  if (!is.character(query) || length(query) != 1 || !nzchar(trimws(query))) {
    .stop("`query` must be one non-empty string.")
  }
  n <- max(1L, as.integer(n))
  res <- switch(engine,
    tavily = .search_tavily(query, n, include_domains, exclude_domains, depth),
    searxng = .search_searxng(query),
    brave = .search_brave(query, n)
  )
  out <- filter_domains(res, include_domains, exclude_domains)
  n_filtered <- attr(out, "n_filtered")
  out <- utils::head(out, n)
  rownames(out) <- NULL
  attr(out, "query") <- query
  attr(out, "engine") <- engine
  attr(out, "n_filtered") <- n_filtered
  attr(out, "cost") <- attr(res, "cost")
  attr(out, "credits") <- attr(res, "credits")
  attr(out, "retrieved_at") <- .now()
  out
}

# Tavily: https://docs.tavily.com/documentation/api-reference/endpoint/search
.search_tavily <- function(query, n, include_domains, exclude_domains, depth) {
  key <- .get_key("tavily")
  # needed only for a live search: a recorded search replays without the key
  need <- .key_needed("tavily", key, how = paste("the free plan is at https://app.tavily.com,",
                                                 "or use engine = \"searxng\""))
  if (length(include_domains) > 300) .stop("Tavily accepts at most 300 include_domains.")
  if (length(exclude_domains) > 150) .stop("Tavily accepts at most 150 exclude_domains.")
  body <- list(query = query, max_results = min(n, 20L), search_depth = depth)
  # as.list() keeps a single domain as a JSON array, as the API expects.
  if (length(include_domains)) body$include_domains <- as.list(include_domains)
  if (length(exclude_domains)) body$exclude_domains <- as.list(exclude_domains)
  req <- .request("https://api.tavily.com/search", body = body, key = key,
                  realm = "tavily", rpm = 100)
  r <- .perform(req, need = need)
  hits <- r$body$results %||% list()
  out <- if (length(hits)) {
    data.frame(
      title = vapply(hits, .field, character(1), "title", default = NA_character_),
      url = vapply(hits, .field, character(1), "url", default = NA_character_),
      content = vapply(hits, .field, character(1), "content", default = NA_character_),
      score = vapply(hits, function(h) as.numeric(.field(h, "score")), numeric(1)),
      engine = "tavily", stringsAsFactors = FALSE)
  } else .empty_search()
  # The response reports the credits used (usage$credits); when it does not,
  # fall back to the documented price of the requested depth.
  credits <- as.numeric(.field(r$body, "usage", "credits",
                               default = if (depth == "advanced") 2 else 1))
  attr(out, "credits") <- credits
  attr(out, "cost") <- if (isTRUE(r$cached)) 0 else credits * .tavily_price_per_credit
  out
}

# SearXNG: https://docs.searxng.org/dev/search_api.html
.search_searxng <- function(query) {
  base <- .opt("searxng_url")
  if (is.null(base) || !nzchar(base)) {
    .stop("Set the address of your SearXNG instance first, for example\n",
          "  options(researchAgentR.searxng_url = \"http://localhost:8080\")")
  }
  req <- .request(paste0(sub("/+$", "", base), "/search"), auth = "none",
                  query = list(q = query, format = "json", pageno = 1),
                  realm = "searxng", rpm = 30)
  r <- .perform(req)
  hits <- if (is.list(r$body)) r$body$results %||% list() else list()
  if (!length(hits)) return(.empty_search())
  data.frame(
    title = vapply(hits, .field, character(1), "title", default = NA_character_),
    url = vapply(hits, .field, character(1), "url", default = NA_character_),
    content = vapply(hits, .field, character(1), "content", default = NA_character_),
    score = vapply(hits, function(h) as.numeric(.field(h, "score")), numeric(1)),
    engine = "searxng", stringsAsFactors = FALSE)
}

# Brave: https://api-dashboard.search.brave.com/app/documentation/web-search/get-started
.search_brave <- function(query, n) {
  key <- Sys.getenv("BRAVE_API_KEY")
  if (!nzchar(key)) .stop("No Brave key. Add BRAVE_API_KEY=your-key to ~/.Renviron and restart R.")
  if (.cache_mode() != "off") {
    .warn("Brave Search terms forbid storing or caching results, so this request ",
          "bypasses the cache and cannot be replayed offline. Do not use Brave results ",
          "to benchmark agents.")
  }
  req <- .request("https://api.search.brave.com/res/v1/web/search", auth = "none",
                  headers = list(`X-Subscription-Token` = key, Accept = "application/json"),
                  query = list(q = query, count = min(n, 20L)),
                  realm = "brave", rpm = 60)
  r <- .perform(req, cache = FALSE)
  hits <- r$body$web$results %||% list()
  if (!length(hits)) return(.empty_search())
  data.frame(
    title = vapply(hits, .field, character(1), "title", default = NA_character_),
    url = vapply(hits, .field, character(1), "url", default = NA_character_),
    content = vapply(hits, .field, character(1), "description", default = NA_character_),
    score = NA_real_, engine = "brave", stringsAsFactors = FALSE)
}

# ---- Reading a page ------------------------------------------------------------

# Characters that HTML writes as entities.
.decode_entities <- function(x) {
  named <- c("&nbsp;" = " ", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"",
             "&#39;" = "'", "&apos;" = "'", "&ndash;" = "-", "&mdash;" = "-",
             "&hellip;" = "...", "&rsquo;" = "'", "&lsquo;" = "'",
             "&rdquo;" = "\"", "&ldquo;" = "\"")
  for (e in names(named)) x <- gsub(e, named[[e]], x, fixed = TRUE)
  # Other named entities (accented letters, Greek letters, math symbols)
  x <- .replace_matches(x, "&[A-Za-z][A-Za-z0-9]{1,7};", function(m) {
    cp <- .entity_codepoints[substr(m, 2, nchar(m) - 1)]
    if (is.na(cp)) m else intToUtf8(cp)
  })
  # Numeric entities such as &#233; or &#xE9;
  x <- .replace_matches(x, "&#[0-9]+;", function(m) intToUtf8(as.integer(gsub("[&#;]", "", m))))
  x <- .replace_matches(x, "&#[xX][0-9a-fA-F]+;", function(m) {
    intToUtf8(strtoi(gsub("[&#xX;]", "", m), 16L))
  })
  gsub("&amp;", "&", x, fixed = TRUE)   # last, so "&amp;lt;" stays "&lt;"
}

# Code points of the named HTML entities a scholarly page is likely to use:
# Latin-1 (160 to 255, in order), Greek letters and common math symbols.
.entity_codepoints <- local({
  latin1 <- c("nbsp", "iexcl", "cent", "pound", "curren", "yen", "brvbar", "sect", "uml", "copy",
              "ordf", "laquo", "not", "shy", "reg", "macr", "deg", "plusmn", "sup2", "sup3",
              "acute", "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
              "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde", "Auml", "Aring",
              "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc", "Euml", "Igrave", "Iacute", "Icirc",
              "Iuml", "ETH", "Ntilde", "Ograve", "Oacute", "Ocirc", "Otilde", "Ouml", "times",
              "Oslash", "Ugrave", "Uacute", "Ucirc", "Uuml", "Yacute", "THORN", "szlig", "agrave",
              "aacute", "acirc", "atilde", "auml", "aring", "aelig", "ccedil", "egrave", "eacute",
              "ecirc", "euml", "igrave", "iacute", "icirc", "iuml", "eth", "ntilde", "ograve",
              "oacute", "ocirc", "otilde", "ouml", "divide", "oslash", "ugrave", "uacute", "ucirc",
              "uuml", "yacute", "thorn", "yuml")
  greek <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
             "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
             "upsilon", "phi", "chi", "psi", "omega")
  Greek <- paste0(toupper(substr(greek, 1, 1)), substring(greek, 2))
  out <- c(stats::setNames(160:255, latin1), stats::setNames(945:969, greek),
           stats::setNames(913:937, Greek),
           c(minus = 8722L, le = 8804L, ge = 8805L, ne = 8800L, asymp = 8776L, bull = 8226L,
             trade = 8482L, euro = 8364L, prime = 8242L, Prime = 8243L, infin = 8734L))
  out[names(out) != "Sigmaf"]   # there is no capital final sigma (930 is unassigned)
})

.replace_matches <- function(x, pattern, f) {
  m <- gregexpr(pattern, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) vapply(v, f, character(1)))
  x
}

.squish <- function(x) trimws(gsub("[ \t\r\n\u00a0]+", " ", x))

#' Convert HTML to readable text
#'
#' Keeps what a reader needs (the title, headings, paragraphs, list items,
#' quotations and code blocks) and drops what only a browser needs
#' (scripts, styles, navigation, footers, forms). Headings become lines that
#' start with `#`, list items lines that start with `-`. When the page has a
#' `<main>` or `<article>` element, only that part is kept.
#'
#' Uses the xml2 package when it is installed; otherwise a simpler
#' regular-expression version runs, which is good enough for most pages.
#'
#' @param html HTML as one string.
#' @param engine `"auto"` (xml2 when installed), `"xml2"` or `"regex"`.
#' @return The text as one string, with the page title in the attribute
#'   `"title"`.
#' @examples
#' html <- "<html><head><title>Demo</title><script>var x = 1;</script></head>
#'   <body><nav>Home | About</nav><h1>Results</h1>
#'   <p>Reliability was &alpha; = .91 &amp; stable.</p>
#'   <ul><li>Item 1</li><li>Item 2</li></ul><footer>Copyright</footer></body></html>"
#' cat(html_to_text(html))
#' cat(html_to_text(html, engine = "regex"))
#' @export
html_to_text <- function(html, engine = c("auto", "xml2", "regex")) {
  engine <- match.arg(engine)
  html <- paste(html, collapse = "\n")
  # Plain text (for example from a reader service) is returned as it is:
  # xml2 would take a string without tags for a file name.
  if (!grepl("<[A-Za-z!/]", html)) return(structure(trimws(html), title = NA_character_))
  if (engine == "auto") engine <- if (requireNamespace("xml2", quietly = TRUE)) "xml2" else "regex"
  if (engine == "xml2" && !requireNamespace("xml2", quietly = TRUE)) {
    .stop("engine = \"xml2\" needs the xml2 package: install.packages(\"xml2\")")
  }
  if (engine == "xml2") .html_text_xml2(html) else .html_text_regex(html)
}

.drop_tags <- c("script", "style", "noscript", "template", "svg", "nav", "footer",
                "aside", "form", "iframe", "button")
.block_tags <- c(paste0("h", 1:6), "p", "li", "pre", "blockquote", "dt", "dd", "td",
                 "th", "figcaption", "caption")

.html_text_xml2 <- function(html) {
  doc <- xml2::read_html(html)
  title <- .squish(xml2::xml_text(xml2::xml_find_first(doc, "//title")))
  drop <- paste0("//", .drop_tags, collapse = " | ")
  xml2::xml_remove(xml2::xml_find_all(doc, drop))
  # Prefer the main content when the page marks it.
  root <- xml2::xml_find_first(doc, "//main | //article")
  if (inherits(root, "xml_missing")) root <- xml2::xml_find_first(doc, "//body")
  if (inherits(root, "xml_missing")) root <- doc
  # Block elements that are not inside another block element, so a <p>
  # inside an <li> is not printed twice.
  is_block <- paste0("self::", .block_tags, collapse = " or ")
  xpath <- paste0(".//*[", is_block, "][not(ancestor::*[", is_block, "])]")
  nodes <- xml2::xml_find_all(root, xpath)
  if (!length(nodes)) {
    text <- .squish(xml2::xml_text(root))
  } else {
    tag <- xml2::xml_name(nodes)
    txt <- .squish(xml2::xml_text(nodes))
    prefix <- ifelse(tag == "li", "- ", "")
    heading <- grepl("^h[1-6]$", tag)
    prefix[heading] <- paste0(strrep("#", as.integer(substr(tag[heading], 2, 2))), " ")
    keep <- nzchar(txt)
    text <- paste0(prefix[keep], txt[keep], collapse = "\n\n")
  }
  if (is.na(title) || !nzchar(title)) {
    h1 <- xml2::xml_find_first(root, ".//h1")
    title <- if (inherits(h1, "xml_missing")) NA_character_ else .squish(xml2::xml_text(h1))
  }
  structure(text, title = title)
}

.html_text_regex <- function(html) {
  title <- regmatches(html, regexpr("(?is)<title[^>]*>.*?</title>", html, perl = TRUE))
  title <- if (length(title)) .squish(.decode_entities(gsub("<[^>]+>", "", title))) else NA_character_
  x <- gsub("(?s)<!--.*?-->", " ", html, perl = TRUE)
  for (tag in c("head", "title", .drop_tags)) {
    x <- gsub(paste0("(?is)<", tag, "\\b[^>]*>.*?</", tag, ">"), " ", x, perl = TRUE)
  }
  # Mark headings with a placeholder that survives tag removal ("@@H2@@").
  x <- gsub("(?i)<h([1-6])[^>]*>", "\n\n@@H\\1@@", x, perl = TRUE)
  x <- gsub("(?i)<li[^>]*>", "\n\n- ", x, perl = TRUE)
  x <- gsub("(?i)<(br|/p|/div|/h[1-6]|/li|/tr|/blockquote|/pre)[^>]*>", "\n\n", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x)
  x <- .decode_entities(x)
  lines <- .squish(strsplit(x, "\n\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & lines != "-"]
  # @@H2@@ markers become "## ".
  lines <- .replace_matches(lines, "^@@H[1-6]@@ ?", function(m) {
    paste0(strrep("#", as.integer(gsub("\\D", "", m))), " ")
  })
  structure(paste(lines, collapse = "\n\n"), title = title)
}

# Agents choose which URLs to open, so a page reader must not become a way to
# reach the researcher's own machine or private network (for example a local
# model server, a router page, or a cloud metadata address).
.check_public_url <- function(url) {
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    .stop("Only http:// and https:// addresses can be read. Got: ", url)
  }
  if (isTRUE(.opt("allow_private_urls", FALSE))) return(invisible(TRUE))
  host <- .domain_of(url)
  private <- host %in% c("localhost", "0.0.0.0", "[::1]", "::1") ||
    grepl("^(127\\.|10\\.|192\\.168\\.|169\\.254\\.|172\\.(1[6-9]|2[0-9]|3[01])\\.)", host) ||
    grepl("\\.(local|internal)$", host)
  if (private) {
    .stop("Refusing to read a local or private address (", host, "). Set ",
          "options(researchAgentR.allow_private_urls = TRUE) if you really mean it.")
  }
  invisible(TRUE)
}

#' Read a web page as text
#'
#' Downloads a page and converts it to readable text with [html_to_text()].
#' With `via = "jina"`, the page is fetched through the Jina Reader
#' (`https://r.jina.ai/<url>`), which also renders pages that need
#' JavaScript and returns Markdown; it allows 20 requests per minute without
#' a key and 500 with a free key (key from `JINA_API_KEY`; see
#' <https://jina.ai/reader/>). The page then passes through Jina's servers.
#'
#' Be a polite reader: many sites forbid automated access in their terms of
#' service or `robots.txt`, and publishers often forbid bulk downloading of
#' articles. This function reads one page at a time, waits between requests
#' to the same site, and does not check `robots.txt` for you: checking the
#' site's rules is the researcher's responsibility. Local and private
#' addresses (for example `localhost`) are refused, so an agent cannot use
#' the tool to reach services on your own machine.
#'
#' @param url The page address (http or https).
#' @param max_chars Maximum number of characters of text to keep. Longer text
#'   is cut, and the cut is marked in the text.
#' @param via `"direct"` (download the HTML) or `"jina"` (Jina Reader).
#' @return A one-row data frame with `title`, `url`, `text`, `chars` (length
#'   of the full text before cutting) and `truncated`, with attributes
#'   `via` and `retrieved_at`.
#' @examples
#' \dontrun{
#' p <- read_page("https://cran.r-project.org/web/packages/EGAnet/index.html")
#' p$title
#' cat(substr(p$text, 1, 500))
#' read_page("https://example.com", via = "jina")
#' }
#' @export
read_page <- function(url, max_chars = 20000, via = c("direct", "jina")) {
  via <- match.arg(via)
  if (!is.character(url) || length(url) != 1) .stop("`url` must be one string.")
  url <- trimws(url)
  .check_public_url(url)
  if (via == "direct") {
    req <- .request(url, auth = "none", realm = paste0("read_page:", .domain_of(url)), rpm = 30,
                    headers = list(Accept = "text/html,application/xhtml+xml,text/plain;q=0.9"),
                    timeout = 30)
    r <- tryCatch(.perform(req), error = function(e) {
      if (grepl("nul|embedded|invalid", conditionMessage(e), ignore.case = TRUE)) {
        .stop("The address did not return a web page (it may be a PDF or an image). ",
              "Download the file and read it with read_documents().")
      }
      stop(e)
    })
    body <- r$body
    if (!is.list(body) && (!is.character(body) || length(body) != 1 || is.na(body))) {
      .stop("The address returned no readable text (it may be a PDF or another binary file). ",
            "Download the file and read it with read_documents().")
    }
    if (is.list(body)) {           # a JSON response: show it as text
      text <- as.character(.to_json(body, pretty = TRUE))
      title <- NA_character_
    } else if (startsWith(body, "%PDF")) {
      .stop("The address returned a PDF. Download it and read it with read_documents().")
    } else {
      text <- html_to_text(body)
      title <- attr(text, "title")
    }
  } else {
    key <- .get_key("jina")
    req <- .request(paste0("https://r.jina.ai/", url), key = key, realm = "jina_reader",
                    rpm = if (nzchar(key)) 200 else 20, timeout = 60,
                    headers = list(Accept = "text/plain"))
    text <- .as_text(.perform(req)$body)
    # The Reader's text output starts with "Title: ..." and "URL Source: ..."
    # lines; read the title from there when present.
    m <- regmatches(text, regexpr("(?m)^Title: .*$", text, perl = TRUE))
    title <- if (length(m)) sub("^Title: ", "", m) else NA_character_
  }
  text <- as.character(text)
  chars <- nchar(text)
  truncated <- chars > max_chars
  if (truncated) {
    text <- paste0(substr(text, 1, max_chars),
                   "\n[... ", chars - max_chars, " more characters not shown ...]")
  }
  out <- data.frame(title = title, url = url, text = text, chars = chars,
                    truncated = truncated, stringsAsFactors = FALSE)
  attr(out, "via") <- via
  attr(out, "retrieved_at") <- .now()
  out
}

# ---- Tools ---------------------------------------------------------------------

#' Web tools for agents
#'
#' `tool_web_search()` lets an agent search the web with [web_search()];
#' `tool_read_page()` lets it read one page with [read_page()]. The model
#' sees a short numbered list (title, address, snippet) or the page text;
#' the full data frame is kept in `attr(value, "data")` of the tool result.
#' Both outputs are marked as untrusted data: web pages can contain text
#' written to manipulate agents (prompt injection).
#'
#' The researcher's domain lists are always enforced. The model may narrow
#' a search further with its own `domains` argument, but cannot widen it.
#'
#' @inheritParams web_search
#' @param n Number of results the model sees per search.
#' @param include_domains Optional allow list set by the researcher, for
#'   example academic and government sites.
#' @param exclude_domains Optional block list set by the researcher.
#' @return An [define_tool()] object.
#' @examples
#' tool_web_search()
#' tool_web_search(include_domains = c("nih.gov", "apa.org", "osf.io"))
#' tool_read_page()
#' @name web_tools
NULL

#' @rdname web_tools
#' @export
tool_web_search <- function(engine = c("tavily", "searxng", "brave"), n = 5,
                            include_domains = NULL, exclude_domains = NULL) {
  engine <- match.arg(engine)
  fn <- function(query, domains = NULL) {
    include <- include_domains
    if (length(domains)) {
      if (length(include_domains)) {
        inside <- .in_domains(domains, include_domains)
        if (!any(inside)) {
          .stop("these domains are outside the allowed list (",
                paste(include_domains, collapse = ", "), ")")
        }
        domains <- domains[inside]
      }
      include <- domains
    }
    res <- web_search(query, n = n, include_domains = include,
                      exclude_domains = exclude_domains, engine = engine)
    .tool_text(.format_search(res), data = res, cost = attr(res, "cost"))
  }
  allowed <- if (length(include_domains)) {
    paste0(" Only these sites are searched: ", paste(include_domains, collapse = ", "), ".")
  } else ""
  define_tool(fn, name = "web_search",
    description = paste0(
      "Search the web. Returns up to ", n, " results, each with a title, address and ",
      "short snippet. Snippets are not enough to support a claim: open a promising ",
      "result with read_page before citing it. Use short, specific queries.", allowed),
    args = list(
      query = arg_string("Search terms, for example 'PROMIS anxiety short form validation'"),
      domains = arg_array("Optional: only search these sites, for example ['nih.gov', 'osf.io']",
                          items = arg_string("A domain"), required = FALSE)),
    max_output_tokens = 1500, untrusted = TRUE)
}

.format_search <- function(res) {
  if (!nrow(res)) return(paste0("No results for '", attr(res, "query"), "'."))
  lines <- vapply(seq_len(nrow(res)), function(i) {
    sprintf("%d. %s\n   %s\n   %s", i, res$title[i] %|na|% "(no title)", res$url[i],
            .shorten(res$content[i] %|na|% "", 300))
  }, character(1))
  c(paste0(nrow(res), " results for '", attr(res, "query"), "':"), lines)
}

#' @rdname web_tools
#' @param max_chars For `tool_read_page()`, characters of page text the model
#'   sees per call. Keep it small on free tiers: 8,000 characters are about
#'   2,000 tokens.
#' @param via For `tool_read_page()`, `"direct"` or `"jina"` (see [read_page()]).
#' @export
tool_read_page <- function(max_chars = 8000, via = c("direct", "jina")) {
  via <- match.arg(via)
  fn <- function(url) {
    p <- read_page(url, max_chars = max_chars, via = via)
    head <- paste0("Title: ", p$title %|na|% "(none)", "\nURL: ", p$url,
                   if (p$truncated) paste0("\n(Showing the first ", max_chars, " of ",
                                           p$chars, " characters.)") else "")
    .tool_text(c(head, "", p$text), data = p)
  }
  define_tool(fn, name = "read_page",
    description = paste(
      "Read one web page and return its text (title, headings, paragraphs, lists).",
      "Use it to check what a page actually says before relying on it. Works for HTML",
      "pages, not PDFs."),
    args = list(url = arg_string("The full address, starting with https://")),
    max_output_tokens = ceiling(max_chars / 4) + 200, untrusted = TRUE)
}
