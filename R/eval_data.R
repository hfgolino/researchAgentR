# The research_tasks benchmark and the live grader its NIH tasks use.

#' A small benchmark of research tasks
#'
#' A [task_set()] for practising evaluation. Every fixed answer is a fact
#' checked against the published articles (DOIs read from the PDFs of the
#' articles); tasks without a fixed answer are graded by code that checks
#' the agent's answer against the live NIH RePORTER API at grading time.
#' Nothing in the answer key was produced by a model.
#'
#' * `dev` tasks are for building and tuning agents; their answers are in
#'   plain text.
#' * `heldout` tasks are for the final evaluation only; their answers are
#'   stored as hashes ([hash_answer()]), so an agent browsing the package
#'   source cannot read them.
#'
#' Tasks:
#' * `doi_dynega` (dev), `doi_entropy` (dev), `doi_aigenie` (heldout),
#'   `doi_ega_sim` (heldout): return the DOI of an article given its title,
#'   journal and year; graded by [grader_doi()].
#' * `journal_entropy` (dev): name the journal of an article; graded by
#'   [grader_exact()].
#' * `nih_ema_2025` (dev), `nih_irt_2025` (heldout): find an NIH project of
#'   fiscal year 2025 on a topic and return its project number; graded live
#'   (the number must exist in NIH RePORTER, belong to fiscal year 2025, and
#'   mention the topic in its title or abstract). These graders need the
#'   network and the package's `search_nih_projects()`; when either is
#'   unavailable the trial is reported as ungraded (`pass = NA`), never as a
#'   failure of the agent.
#'
#' @format A `task_set` data frame with 7 rows and columns `id`,
#'   `instruction`, `split`, `answer` (list), `initial_state` (list),
#'   `grader` (list of functions), `type` and `source`.
#' @source Development articles: Psychometrika (2022),
#'   \doi{10.1007/s11336-021-09820-y}; Multivariate Behavioral Research
#'   (2020), \doi{10.1080/00273171.2020.1779642}. The held-out articles
#'   are named in their task instructions; their DOIs are not written in
#'   plain text anywhere in the package. NIH RePORTER API:
#'   <https://api.reporter.nih.gov/>. Built by `data-raw/research_tasks.R`.
#' @examples
#' data(research_tasks)
#' research_tasks
#' dev <- research_tasks[research_tasks$split == "dev", ]
#' # Grade a hand-written answer (no model involved):
#' i <- which(dev$id == "doi_entropy")
#' dev$grader[[i]]("https://doi.org/10.1080/00273171.2020.1779642",
#'                 list(id = dev$id[i], answer = dev$answer[[i]]))
"research_tasks"

# A live code grader for "find an NIH project of fiscal year <fy> on
# <topic>": every project number in the answer is looked up with
# search_nih_projects(project_nums = ) and must exist, belong to the fiscal
# year, and mention one of `terms` in its title or abstract. The answer passes when at least one
# number it gives meets all three conditions and no number it gives is
# unverifiable.
.grader_nih_fy <- function(fiscal_year, terms) {
  force(fiscal_year)
  force(terms)
  grader_code(function(answer, task) {
    nums <- .nih_project_numbers(.answer_text(answer))
    if (!length(nums)) return(list(pass = FALSE, score = 0, reason = "no NIH project number in the answer"))
    ok <- vapply(nums, .nih_project_matches, logical(1), fiscal_year = fiscal_year, terms = terms)
    list(pass = all(ok), score = mean(ok),
         reason = paste0(sum(ok), " of ", length(ok), " project number(s) verified in NIH RePORTER ",
                         "(fiscal year ", fiscal_year, ", topic '", paste(terms, collapse = "' or '"),
                         "'): ", paste(nums, ifelse(ok, "ok", "not verified"), collapse = "; ")))
  }, label = paste0("live NIH RePORTER check (FY", fiscal_year, ")"))
}

# NIH project numbers look like 5R01MH123456-03 (type, activity, institute,
# serial, support year); the core number is the middle part, R01MH123456.
.nih_project_numbers <- function(text) {
  hits <- regmatches(toupper(text), gregexpr("[1-9]?[A-Z][A-Z0-9]{2}[A-Z]{2}[0-9]{6}(-[0-9A-Z]+)?",
                                             toupper(text)))[[1]]
  core <- sub("-.*$", "", hits)
  core <- sub("^[1-9]", "", core)
  unique(core)
}

.nih_project_matches <- function(core, fiscal_year, terms) {
  # RePORTER writes a project number as type digit + core number + support
  # year (5R01MH123456-03) and has one record per fiscal year. "*" stands for
  # any characters at the END of a number (https://api.reporter.nih.gov/), so
  # the core number is asked for with each of the nine type digits in front.
  # (A "*" in front as well returned nothing in a live test, November 23, 2026.)
  res <- search_nih_projects(project_nums = paste0(1:9, core, "*"), fiscal_years = fiscal_year, n = 50)
  if (!nrow(res)) return(FALSE)
  text <- paste(res$title, res$abstract)
  hit <- grepl(core, toupper(res$project_num), fixed = TRUE) &
    res$fiscal_year %in% fiscal_year &
    Reduce(`|`, lapply(terms, function(t) grepl(t, text, ignore.case = TRUE)))
  any(hit, na.rm = TRUE)
}
