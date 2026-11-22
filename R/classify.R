# A bridge to Jev, TypeSafe AI's classification model. Jev answers typed
# questions about a text with probabilities instead of generated text:
#   Choice: which ONE of several options applies (2 to 255 options)
#   Noul:   the probability that a yes/no statement is true
# Fast, cheap typed decisions are useful inside agents: screening abstracts
# against an inclusion criterion, or routing a task to the right agent.
#
# This is a minimal port of the pure-R Jev client in llmClassificR (same
# author, GPL-3), written with the package's .request()/.perform() so that it
# gets retries, rate limits and the response cache. llmClassificR is not
# needed. API: POST https://api.typesafe.ai/v1/systemone, one text per request.

.jev <- list(
  url = "https://api.typesafe.ai/v1/systemone",
  model = "jev-1.13.0",        # pinned version used in the course (September 2026)
  price_per_million = 0.042,   # US dollars per million input tokens; output is free (September 2026)
  rpm = 600                    # 10 per second; the API allows 1,200 requests per minute
)

# Options as a named character vector: names are the options, values say
# when each applies. An unnamed vector uses each value as its own name.
.as_options <- function(options) {
  options <- unlist(options)
  if (is.null(names(options))) names(options) <- options
  empty <- is.na(names(options)) | names(options) == ""
  names(options)[empty] <- options[empty]
  if (anyDuplicated(names(options))) .stop("Option names must be unique.")
  options
}

# The question object Jev expects (same structure as llmClassificR's
# jev_choice() and jev_noul()).
.jev_question <- function(question, options, type) {
  if (!is.character(question) || length(question) != 1 || !nzchar(question)) {
    .stop("`question` must be one non-empty string.")
  }
  if (type == "choice") {
    if (is.null(options)) .stop("A choice question needs `options`.")
    options <- .as_options(options)
    if (length(options) < 2 || length(options) > 255) {
      .stop("A choice question needs between 2 and 255 options.")
    }
    return(list(type = "choice", instructions = question, criteria = as.list(options)))
  }
  q <- list(type = "noul", instructions = question)
  if (!is.null(options)) {
    # Optional descriptions of what counts as true and as false.
    if (!all(c("true", "false") %in% names(options))) {
      .stop("For a noul question, `options` is NULL or c(true = \"...\", false = \"...\").")
    }
    q$criteria <- list(true = unname(options[["true"]]), false = unname(options[["false"]]))
  }
  q
}

# One row of results from one Jev answer (or NA when the request failed).
.jev_row <- function(answer, type, labels) {
  if (type == "choice") {
    probs <- vapply(labels, function(o) as.numeric(.field(answer, "probabilities", o, default = NA_real_)),
                    numeric(1))
    pred <- as.character(.field(answer, "choice", default = NA_character_))
    if (!is.na(pred) && !pred %in% labels) pred <- NA_character_   # never guess a label
    conf <- as.numeric(.field(answer, "confidence", default = NA_real_))
  } else {
    p <- as.numeric(.field(answer, "noul", default = NA_real_))
    probs <- c(true = p, false = 1 - p)
    pred <- if (is.na(p)) NA_character_ else if (p >= 0.5) "true" else "false"
    conf <- if (is.na(p)) NA_real_ else max(p, 1 - p)
  }
  data.frame(prediction = pred, confidence = conf, t(probs), check.names = FALSE,
             stringsAsFactors = FALSE)
}

#' Classify texts with Jev (TypeSafe AI)
#'
#' Asks Jev one typed question about every text and returns the decision
#' with its probabilities. A `"choice"` question picks one of several
#' options (for example which agent should handle a task); a `"noul"`
#' question gives the probability that a statement is true of the text (for
#' example "The study reports original data from human participants", for
#' screening abstracts). Several Noul questions, one per inclusion
#' criterion, make a transparent screener.
#'
#' Jev is a paid service: US$0.042 per million input tokens, output free
#' (price used in the course, September 2026; check
#' <https://typesafe.ai> before relying on it). The key comes from
#' `TYPESAFE_API_KEY` (see [set_typesafe_key()]). Requests are sent one at a
#' time, at most 10 per second, through the response cache.
#'
#' **Research notes.** Texts are sent to TypeSafe's servers: check your IRB
#' protocol and data agreements before sending participant data. Jev is
#' proprietary and its training data are unknown; pin the model version
#' (the default) so results can be reproduced, and validate against a sample
#' of human decisions before trusting it.
#'
#' Failed requests and invalid answers become `NA` rows. They are counted in
#' the attribute `"n_failed"` and their messages kept in `"errors"`; they are
#' never silently replaced.
#'
#' @param texts Character vector of texts.
#' @param question For `"choice"`, the question asked about each text; for
#'   `"noul"`, the statement whose truth is assessed.
#' @param options For `"choice"`: a named character vector, names are the
#'   options and values describe when each applies (an unnamed vector uses
#'   the values as names). For `"noul"`: `NULL`, or
#'   `c(true = "...", false = "...")` describing what counts as true and false.
#' @param type `"choice"` or `"noul"`.
#' @param model Jev model version. Pin a version for research;
#'   `"jev-latest"` changes with new releases.
#' @return A data frame with `text`, `prediction`, `confidence` and one
#'   probability column per option (`true` and `false` for Noul, where
#'   `prediction` is `"true"` when the probability is at least 0.5). The
#'   attributes `usage` (texts, failures, input tokens, cost), `cost` (US
#'   dollars, 0 for cached answers), `n_failed`, `errors`, `question` and
#'   `model` document the run.
#' @examples
#' \dontrun{
#' set_typesafe_key()
#' abstracts <- c("We surveyed 1,204 adults about loneliness ...",
#'                "This commentary argues that measurement theory ...")
#' classify_texts(abstracts,
#'                "The abstract reports original data collected from human participants.",
#'                type = "noul")
#' classify_texts("Find NIH grants on aging and loneliness",
#'                "Which agent should handle this request?",
#'                options = c(literature = "Finding or summarizing published papers",
#'                            funding = "Finding grants, funders or funding calls",
#'                            design = "Planning a study, sample size or analysis"))
#' }
#' @export
classify_texts <- function(texts, question, options = NULL, type = c("choice", "noul"),
                           model = "jev-1.13.0") {
  type <- match.arg(type)
  key <- .get_key("typesafe")
  # needed only for live calls: recorded decisions replay without the key
  need <- .key_needed("typesafe", key, how = "keys: https://console.typesafe.ai/settings/keys")
  q <- .jev_question(question, options, type)
  labels <- if (type == "choice") names(q$criteria) else c("true", "false")
  texts <- as.character(texts)
  rows <- vector("list", length(texts))
  errors <- rep(NA_character_, length(texts))
  tokens <- numeric(length(texts))
  billed <- numeric(length(texts))
  for (i in seq_along(texts)) {
    req <- .request(.jev$url, key = key, realm = "typesafe", rpm = .jev$rpm,
                    body = list(model = model, state = texts[i], questions = list(q = q)))
    r <- tryCatch(.perform(req, need = need), error = function(e) e)
    if (inherits(r, "error")) {
      if (identical(conditionMessage(r), need)) .stop(need)   # no key: stop at once
      errors[i] <- conditionMessage(r)
      rows[[i]] <- .jev_row(NULL, type, labels)
      next
    }
    answer <- r$body$answers$q
    rows[[i]] <- .jev_row(answer, type, labels)
    if (is.na(rows[[i]]$prediction)) errors[i] <- "no valid answer in the response"
    tokens[i] <- as.numeric(.field(r$body, "usage", "input_tokens", default = 0))
    if (!isTRUE(r$cached)) billed[i] <- tokens[i]   # cached answers cost nothing
  }
  out <- data.frame(text = texts, do.call(rbind, rows), check.names = FALSE,
                    stringsAsFactors = FALSE)
  n_failed <- sum(!is.na(errors))
  cost <- sum(billed) / 1e6 * .jev$price_per_million
  if (n_failed) .warn(n_failed, " of ", length(texts), " texts got no answer (NA). First error: ",
                      errors[!is.na(errors)][1])
  attr(out, "usage") <- data.frame(n_texts = length(texts), n_failed = n_failed,
                                   input_tokens = sum(tokens), cost_usd = cost)
  attr(out, "cost") <- cost
  attr(out, "n_failed") <- n_failed
  attr(out, "errors") <- errors
  attr(out, "question") <- q
  attr(out, "model") <- model
  out
}

#' A classification tool for agents (Jev)
#'
#' Wraps [classify_texts()] with a question fixed by the researcher, so the
#' agent only supplies texts. The question, the options and the model version
#' are a research decision, written down once, not something the model
#' improvises. Typical uses: screening abstracts against one inclusion
#' criterion (`type = "noul"`), or routing a request (`type = "choice"`).
#' The cost of each call (Jev is paid) is recorded in the episode ledger.
#'
#' @inheritParams classify_texts
#' @param name Tool name the model sees.
#' @param description What the tool decides, written for the model. The
#'   default is built from the question.
#' @param approval If `TRUE`, a person approves every call (the service is
#'   paid).
#' @return An [define_tool()] object. The R value of each call keeps the
#'   full [classify_texts()] data frame in `attr(value, "data")`.
#' @examples
#' screen <- tool_classify(
#'   "The abstract reports original data collected from human participants.",
#'   type = "noul", name = "screen_empirical")
#' screen
#' @export
tool_classify <- function(question, options = NULL, type = c("choice", "noul"),
                          model = "jev-1.13.0", name = "classify", description = NULL,
                          approval = FALSE) {
  type <- match.arg(type)
  q <- .jev_question(question, options, type)    # check the question now, not mid-episode
  description <- description %||% if (type == "choice") {
    paste0("Classify texts with a fast classifier. Question: '", question, "' Options: ",
           paste(names(q$criteria), collapse = ", "),
           ". Returns the chosen option and its probability for each text.")
  } else {
    paste0("Estimate, for each text, the probability that this statement is true: '",
           question, "'. Returns true or false with the probability.")
  }
  fn <- function(texts) {
    res <- classify_texts(texts, question, options = options, type = type, model = model)
    lines <- sprintf("%d. %s (p = %s): %s", seq_len(nrow(res)),
                     ifelse(is.na(res$prediction), "NO ANSWER", res$prediction),
                     ifelse(is.na(res$confidence), "NA", sprintf("%.2f", res$confidence)),
                     vapply(res$text, .shorten, character(1), n = 80, USE.NAMES = FALSE))
    if (attr(res, "n_failed")) {
      lines <- c(lines, paste0(attr(res, "n_failed"), " text(s) got no answer."))
    }
    .tool_text(lines, data = res, cost = attr(res, "cost"))
  }
  define_tool(fn, name = name, description = description,
    args = list(texts = arg_array("The texts to classify, one string each",
                                  items = arg_string("A text"))),
    approval = approval, timeout = 300, max_output_tokens = 2000, untrusted = TRUE)
}
