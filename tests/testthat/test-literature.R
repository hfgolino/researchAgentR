# Offline tests cover parsing, normalization, deduplication and counting.
# Response fragments below follow the shapes documented by each provider (the
# URL is given next to each one); they test parsers, not API behavior.

test_that("normalize_doi() handles common DOI forms", {
  expect_equal(
    normalize_doi(c("https://doi.org/10.1037/MET0000255", "doi: 10.1007/s11336-021-09820-y.",
                    "http://dx.doi.org/10.1000%2Fabc", "not a doi", NA, "")),
    c("10.1037/met0000255", "10.1007/s11336-021-09820-y", "10.1000/abc", NA, NA, NA))
})

test_that("abstracts are rebuilt from an OpenAlex inverted index", {
  # Shape from https://docs.openalex.org/api-entities/works/work-object
  # ("Despite": [0], "growing": [1], "interest": [2], ...), as parsed by
  # jsonlite with simplifyVector = FALSE.
  inv <- list(Despite = list(0L), growing = list(1L), interest = list(2L, 5L),
              `in` = list(3L), the = list(4L))
  expect_equal(.lit_reconstruct_abstract(inv), "Despite growing interest in the interest")
  expect_true(is.na(.lit_reconstruct_abstract(NULL)))
  expect_true(is.na(.lit_reconstruct_abstract(list())))
})

test_that("an OpenAlex work becomes one row with the common columns", {
  # Field names from the Work schema in
  # https://developers.openalex.org/api-reference/openapi.json
  w <- list(
    id = "https://openalex.org/W2741809807", doi = "https://doi.org/10.7717/peerj.4375",
    display_name = "The state of OA", publication_year = 2018L, type = "article",
    cited_by_count = 10L, is_retracted = FALSE,
    authorships = list(list(author = list(display_name = "Heather Piwowar")),
                       list(author = list(display_name = "Jason Priem"))),
    primary_location = list(source = list(display_name = "PeerJ")),
    best_oa_location = list(pdf_url = "https://peerj.com/articles/4375.pdf"),
    open_access = list(is_oa = TRUE, oa_url = NULL),
    abstract_inverted_index = list(Open = list(0L), access = list(1L)))
  r <- .lit_parse_openalex(w)
  expect_true(all(.lit_columns %in% names(r)))
  expect_equal(r$id, "W2741809807")
  expect_equal(r$doi, "10.7717/peerj.4375")
  expect_equal(r$authors, "Piwowar, H.; Priem, J.")
  expect_equal(r$source, "PeerJ")
  expect_equal(r$abstract, "Open access")
  expect_equal(r$oa_url, "https://peerj.com/articles/4375.pdf")
  expect_equal(r$year, 2018L)
  expect_false(r$is_retracted)
})

test_that("a Crossref item becomes one row with the common columns", {
  # Field names from https://github.com/CrossRef/rest-api-doc/blob/master/api_format.md
  it <- list(DOI = "10.1037/MET0000255", title = list("Exploratory graph analysis"),
             author = list(list(family = "Golino", given = "Hudson F."),
                           list(family = "Epskamp", given = "Sacha")),
             issued = list(`date-parts` = list(list(2017L, 6L, 8L))),
             `container-title` = list("Psychological Methods"),
             abstract = "<jats:p>An <jats:italic>abstract</jats:italic>.</jats:p>",
             type = "journal-article")
  r <- .lit_parse_crossref(it)
  expect_true(all(.lit_columns %in% names(r)))
  expect_equal(r$doi, "10.1037/met0000255")
  expect_equal(r$authors, "Golino, H. F.; Epskamp, S.")
  expect_equal(r$year, 2017L)
  expect_equal(r$abstract, "An abstract.")
  expect_equal(r$database, "Crossref")
})

test_that("a Semantic Scholar paper becomes one row", {
  # Field names from https://api.semanticscholar.org/graph/v1/swagger.json
  p <- list(paperId = "abc123", title = "A paper", year = 2021L,
            externalIds = list(DOI = "10.1000/XYZ"), authors = list(list(name = "Ann B. Smith")),
            venue = "Journal", abstract = NULL, openAccessPdf = list(url = ""),
            citationCount = 3L, publicationTypes = list("JournalArticle"))
  r <- .lit_parse_s2(p)
  expect_equal(r$doi, "10.1000/xyz")
  expect_equal(r$authors, "Smith, A. B.")
  expect_true(is.na(r$abstract))
  expect_true(is.na(r$oa_url))
  expect_equal(r$database, "Semantic Scholar")
})

# An Atom fragment with the elements described in
# https://info.arxiv.org/help/api/user-manual.html
arxiv_feed <- '<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom" xmlns:opensearch="http://a9.com/-/spec/opensearch/1.1/" xmlns:arxiv="http://arxiv.org/schemas/atom">
<opensearch:totalResults>1</opensearch:totalResults>
<entry>
<id>http://arxiv.org/abs/2101.00001v2</id>
<published>2021-01-01T00:00:00Z</published>
<title>Agents &amp; tools:
  a study</title>
<summary>  An abstract
 over two lines. </summary>
<author><name>Ann Smith</name></author>
<author><name>Bo Li</name></author>
<arxiv:doi>10.1000/ABC</arxiv:doi>
<link href="http://arxiv.org/abs/2101.00001v2" rel="alternate" type="text/html"/>
<link title="pdf" href="http://arxiv.org/pdf/2101.00001v2" rel="related" type="application/pdf"/>
</entry>
</feed>'

test_that("arXiv Atom feeds are parsed (xml2 and the fallback agree)", {
  r1 <- .arxiv_regex(arxiv_feed)
  expect_equal(r1$title, "Agents & tools: a study")
  expect_equal(r1$abstract, "An abstract over two lines.")
  expect_equal(r1$authors, "Smith, A.; Li, B.")
  expect_equal(r1$doi, "10.1000/abc")
  expect_equal(r1$oa_url, "http://arxiv.org/pdf/2101.00001v2")
  expect_equal(r1$id, "2101.00001v2")
  expect_equal(r1$year, 2021L)
  expect_equal(attr(r1, "total_found"), 1L)
  skip_if_not_installed("xml2")
  r2 <- .arxiv_xml2(arxiv_feed)
  expect_equal(as.data.frame(r2), as.data.frame(r1), ignore_attr = TRUE)
})

test_that("arXiv queries get field prefixes only when needed", {
  expect_equal(.arxiv_query("graph analysis"), "all:graph AND all:analysis")
  expect_equal(.arxiv_query("ti:psychometrics AND cat:stat.ME"), "ti:psychometrics AND cat:stat.ME")
})

test_that("author names are written as 'Last, F.'", {
  expect_equal(.lit_author_name("Hudson F. Golino"), "Golino, H. F.")
  expect_equal(.lit_author_name("Han L. J. van der Maas"), "van der Maas, H. L. J.")
  expect_equal(.lit_author_name("Golino, H."), "Golino, H.")
  expect_equal(.lit_author_name("Plato"), "Plato")
})

test_that("dedupe_records() merges by DOI, then by title and year, keeping provenance", {
  a <- data.frame(title = c("A toy study", "Another study", "Unique"),
                  year = c(2017, 2020, 2021), doi = c("10.1234/toy.1", NA, NA),
                  abstract = c(NA, "An abstract.", NA), database = "OpenAlex")
  b <- data.frame(title = c("A different title", "Another Study!"),
                  year = c(2017, 2020), doi = c("https://doi.org/10.1234/TOY.1", NA),
                  abstract = c("Toy abstract.", NA), database = "Crossref")
  d <- dedupe_records(rbind(a, b))
  expect_equal(nrow(d), 3)
  expect_equal(d$database, c("OpenAlex; Crossref", "OpenAlex; Crossref", "OpenAlex"))
  expect_equal(d$abstract[1], "Toy abstract.")   # filled from the duplicate
  expect_equal(d$n_found, c(2L, 2L, 1L))
  expect_equal(d$n_databases, c(2L, 2L, 1L))
  # Two versions of one work from the SAME database: two records, one database.
  twice <- dedupe_records(rbind(a, a[1, ], b))
  expect_equal(twice$n_found[1], 3L); expect_equal(twice$n_databases[1], 2L)
  again <- dedupe_records(rbind(d[, names(a)], b))                       # a table that was deduplicated before
  expect_equal(again$n_databases, c(2L, 2L, 1L)); expect_equal(again$database[1], "OpenAlex; Crossref")
  expect_true(all(c("n_databases", "n_found") %in% names(dedupe_records(a[0, ]))))
  counts <- attr(d, "dedup")
  expect_equal(counts$n, c(5L, 1L, 1L, 3L))
  # Different years are not duplicates.
  e <- dedupe_records(data.frame(title = c("Same", "Same"), year = c(2019, 2020),
                                 doi = NA, database = "x"))
  expect_equal(nrow(e), 2)
  expect_error(dedupe_records(data.frame(title = "x")), "needs the columns")
})

test_that("prisma_counts() computes every stage from the data", {
  oa <- data.frame(title = c("A", "B", "C"), year = 2020, doi = c("10.1/a", "10.1/b", NA),
                   database = "OpenAlex")
  cr <- data.frame(title = c("A", "D"), year = 2020, doi = c("10.1/a", "10.1/d"),
                   database = "Crossref")
  d <- dedupe_records(rbind(oa, cr))
  p <- prisma_counts(list(OpenAlex = oa, Crossref = cr), d,
                     screening = c(TRUE, FALSE, NA, TRUE), included = 1)
  n <- stats::setNames(p$n, paste(p$stage, p$database))
  expect_equal(unname(n["Records identified OpenAlex"]), 3L)
  expect_equal(unname(n["Records identified (total) "]), 5L)
  expect_equal(unname(n["Duplicates removed "]), 1L)
  expect_equal(unname(n["Records excluded at screening "]), 1L)
  expect_equal(unname(n["Records awaiting a human decision "]), 1L)
  expect_equal(unname(n["Studies included "]), 1L)
  # A data frame with a database column, and named counts, also work.
  expect_equal(prisma_counts(rbind(oa, cr))$n[1:2], c(2L, 3L))
  expect_equal(prisma_counts(c(PubMed = 10, OpenAlex = 5))$n[3], 15L)
  expect_error(prisma_counts(c(10, 5)), "Name each database")
})

test_that("the model sees a compact summary while R keeps the data frame", {
  df <- .lit_records(title = "A title", year = 2020L, doi = "10.1/a", authors = "A, B.; C, D.; E, F.; G, H.",
                     source = "J", abstract = paste(rep("word", 100), collapse = " "),
                     oa_url = NA, id = "W1", database = "OpenAlex")
  x <- .as_lit_records(df)
  expect_s3_class(x, "data.frame")
  txt <- researchAgentR:::.as_text(x)
  expect_match(txt, "A, B.; C, D.; E, F. et al.", fixed = TRUE)
  expect_match(txt, "DOI: 10.1/a", fixed = TRUE)
  expect_equal(lengths(regmatches(txt, gregexpr("word", txt))), 60)
})

test_that("every research tool has a valid schema", {
  tools <- list(tool_openalex(), tool_crossref(), tool_semantic_scholar(), tool_europepmc(),
                tool_arxiv(), tool_unpaywall(), tool_snowball())
  names <- c("search_openalex", "search_crossref", "search_semantic_scholar", "search_europepmc",
             "search_arxiv", "find_open_access", "snowball")
  for (i in seq_along(tools)) {
    expect_s3_class(tools[[i]], "agent_tool")
    s <- jsonlite::fromJSON(tool_schema(tools[[i]]), simplifyVector = FALSE)
    expect_equal(s$name, names[i])
    expect_equal(s$parameters$type, "object")
    expect_true(nchar(s$description) > 40)
  }
  expect_equal(unlist(jsonlite::fromJSON(tool_schema(tool_openalex()))$parameters$required), "query")
  expect_equal(jsonlite::fromJSON(tool_schema(tool_snowball()))$parameters$properties$direction$enum,
               c("references", "citations"))
  # Invalid arguments are rejected before any request is made.
  out <- call_tool("snowball", list(tool_snowball()),
                   arguments = list(work = "10.1/a", direction = "sideways"))
  expect_true(out$is_error)
})

# ---- Live tests (run on a machine with internet access) ------------------------

test_that("OpenAlex search, single lookup and snowballing work live", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  skip_if_not(nzchar(Sys.getenv("OPENALEX_API_KEY")), "OPENALEX_API_KEY not set")
  res <- search_openalex("exploratory graph analysis", n = 5, year_from = 2017)
  expect_true(all(.lit_columns %in% names(res)))
  expect_lte(nrow(res), 5)
  expect_true(all(res$year >= 2017, na.rm = TRUE))
  w <- get_work("10.1037/met0000255")
  expect_equal(w$doi, "10.1037/met0000255")
  expect_true(length(attr(w, "referenced_works")) > 0)
  refs <- get_references("10.1037/met0000255", n = 5)
  expect_true(nrow(refs) > 0)
  cites <- get_citations(w$id, n = 5)
  expect_true(nrow(cites) > 0)
})

test_that("Crossref, Semantic Scholar, Europe PMC and arXiv searches work live", {
  skip_on_cran()
  skip_if_offline("api.crossref.org")
  cr <- search_crossref("exploratory graph analysis", n = 3)
  expect_true(all(.lit_columns %in% names(cr)))
  expect_true(any(!is.na(cr$doi)))
  s2 <- tryCatch(search_semantic_scholar("exploratory graph analysis", n = 3),
                 error = function(e) skip(paste("Semantic Scholar:", conditionMessage(e))))
  expect_true(all(.lit_columns %in% names(s2)))
  ep <- search_europepmc("loneliness older adults", n = 3)
  expect_true(all(.lit_columns %in% names(ep)))
  expect_true(any(!is.na(ep$abstract)))
  ax <- search_arxiv("language model agents", n = 3)
  expect_equal(nrow(ax), 3)
  expect_true(all(grepl("pdf", ax$oa_url)))
})

test_that("find_open_access() works live", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  oa <- find_open_access(c("10.7717/peerj.4375", "not a doi"))
  expect_equal(nrow(oa), 2)
  expect_true(isTRUE(oa$is_oa[1]))
  expect_equal(oa$detail[2], "not a valid DOI")
})

test_that("tool_get_work is a tool with one DOI argument", {
  t <- tool_get_work()
  expect_s3_class(t, "agent_tool")
  expect_equal(t$name, "get_work")
  expect_match(tool_schema(t), '"doi"')
})

test_that("the researcher's n is the limit of a search tool call", {
  expect_equal(.lit_n(NULL, 5), 5L)       # the model did not say: the researcher's number
  expect_equal(.lit_n(3, 5), 3L)          # fewer is allowed
  expect_equal(.lit_n(50, 5), 5L)         # more is not
  expect_equal(.lit_n(0, 5), 1L)
  expect_equal(.lit_n("many", 5), 5L)
  expect_match(tool_schema(tool_openalex(n = 5)), "at most 5")
  expect_match(tool_schema(tool_snowball(n = 7)), "at most 7")
})
