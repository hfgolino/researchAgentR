# Running R code written by a model. The code runs in a SEPARATE R process
# (callr), never in the researcher's session, so a crash, an infinite loop or
# rm(list = ls()) cannot damage the session. The child process gets a time
# limit, its own temporary folder, no API keys, and (by default) proxy
# settings that make ordinary web requests fail.
#
# This is a soft sandbox, NOT a security boundary: the code runs with the
# researcher's user account and can read and write any file that account
# can, start other programs with system(), and open raw sockets that ignore
# proxy settings. Read code before running it when the task allows, and never
# give an agent that runs code access to confidential data. For real
# isolation, run the agent inside a container or a virtual machine.

# An address where nothing listens (port 9 is the "discard" service, closed
# on ordinary machines): requests sent through this "proxy" fail at once.
.blackhole_proxy <- "http://127.0.0.1:9"

# Environment variables for the child process.
.sandbox_env <- function(allow_network) {
  env <- callr::rcmd_safe_env()
  # Hide secrets: every variable whose name suggests a key, token or password,
  # plus the package's own key variables, is set to "" in the child.
  secret <- grep("KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL", names(Sys.getenv()),
                 ignore.case = TRUE, value = TRUE)
  env[unique(c(secret, unname(.key_vars)))] <- ""
  # Do not read ~/.Renviron in the child (it is where the keys live).
  empty <- tempfile("Renviron_")
  file.create(empty)
  env["R_ENVIRON_USER"] <- empty
  if (!allow_network) {
    proxy <- c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY", "all_proxy")
    env[proxy] <- .blackhole_proxy
    env[c("no_proxy", "NO_PROXY")] <- ""     # no exceptions to the proxy
  }
  env
}

# The function that runs inside the child process. callr sends it to a new R
# session without its environment, so it uses only base R and `pkg::` calls.
.run_in_child <- function(code, packages, dir) {
  setwd(dir)
  # files written and read by the code default to UTF-8 on every system
  options(encoding = "UTF-8")
  warnings <- character(0)
  messages <- character(0)
  error <- NA_character_
  value <- NULL
  output <- utils::capture.output({
    error <- tryCatch(withCallingHandlers({
      for (p in packages) library(p, character.only = TRUE)
      exprs <- parse(text = code, keep.source = FALSE)
      env <- new.env(parent = globalenv())
      # Evaluate one expression at a time and print visible values, as the
      # console does, so `summary(fit)` shows its output.
      for (e in exprs) {
        v <- withVisible(eval(e, env))
        if (v$visible) print(v$value)
        value <- v$value
      }
      NA_character_
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }),
    error = function(e) conditionMessage(e))
  })
  grDevices::graphics.off()   # close any plot device so its file is written
  summary <- if (is.null(value)) NA_character_ else {
    paste(utils::capture.output(utils::str(value, max.level = 1, list.len = 10, vec.len = 3)),
          collapse = "\n")
  }
  list(output = paste(output, collapse = "\n"), warnings = warnings, messages = messages,
       error = error, value = summary, files = list.files(dir, recursive = TRUE))
}

#' Run R code in a separate R process
#'
#' Runs code (usually written by a model) in a fresh R session started with
#' the callr package, with a time limit and a temporary working folder, and
#' returns everything it printed, its warnings, messages and error, and a
#' summary of its last value. The researcher's session is never touched.
#'
#' Safety measures, and their limits:
#'
#' * **Separate process with a time limit.** Crashes and infinite loops end
#'   with the child process.
#' * **No keys.** Environment variables whose names contain `KEY`, `TOKEN`,
#'   `SECRET`, `PASSWORD` or `CREDENTIAL` are emptied, and `~/.Renviron` is
#'   not read, so the code cannot see your API keys through `Sys.getenv()`.
#' * **No network (soft).** Unless `allow_network = TRUE`, the proxy
#'   variables (`http_proxy`, `https_proxy`, `ALL_PROXY` and upper-case
#'   versions) point to an address where nothing listens, so
#'   `download.file()`, `url()`, `readLines("https://...")`, httr2 and curl
#'   requests fail.
#'
#' This is **not a security boundary**: the code still runs as your user,
#' can read your files (including `~/.Renviron` itself), can run programs
#' with `system()`, and can open sockets that ignore proxies. Use it for
#' code you would be willing to run yourself, and use a container or virtual
#' machine for untrusted code.
#'
#' @param code R code as a string.
#' @param timeout Seconds before the child process is stopped.
#' @param allow_network If `FALSE` (default), web requests are blocked
#'   (softly, see above).
#' @param packages Character vector of packages to attach before the code
#'   runs, for example `c("pwr")`.
#' @param dir Working folder. The default creates a new temporary folder;
#'   files the code writes (plots, data) stay there after the run.
#' @return A list of class `r_code_result` with `code`, `output` (printed
#'   text), `warnings`, `messages`, `error` (`NA` when the code ran), `value`
#'   (a `str()` summary of the last value), `files` (paths of the files in
#'   `dir`), `dir`, `seconds` and `timed_out`.
#' @examples
#' \dontrun{
#' # Needs the callr package (and starts a new R process)
#' r <- run_r_code("x <- rnorm(100, mean = 10); mean(x) > 9")
#' r
#' run_r_code("pwr::pwr.t.test(d = 0.4, power = 0.8)$n")
#' run_r_code("Sys.sleep(5)", timeout = 1)$timed_out
#' run_r_code("readLines('https://cran.r-project.org')")$error
#' }
#' @export
run_r_code <- function(code, timeout = 30, allow_network = FALSE, packages = NULL,
                       dir = NULL) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    .stop("run_r_code() needs the callr package: install.packages(\"callr\")")
  }
  code <- paste(code, collapse = "\n")
  dir <- dir %||% tempfile("r_code_")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir <- normalizePath(dir)
  t0 <- Sys.time()
  res <- tryCatch(
    callr::r(.run_in_child, args = list(code = code, packages = packages, dir = dir),
             env = .sandbox_env(allow_network), timeout = timeout,
             system_profile = FALSE, user_profile = FALSE),
    callr_timeout_error = function(e) "timeout",
    error = function(e) e)
  seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  timed_out <- identical(res, "timeout")
  if (timed_out || inherits(res, "error")) {
    msg <- if (timed_out) paste0("the code did not finish within ", timeout, " seconds")
           else paste("the R process failed:", conditionMessage(res))
    res <- list(output = "", warnings = character(0), messages = character(0),
                error = msg, value = NA_character_, files = list.files(dir, recursive = TRUE))
  }
  files <- if (length(res$files)) file.path(dir, res$files) else character(0)
  structure(list(code = code, output = res$output, warnings = res$warnings,
                 messages = res$messages, error = res$error, value = res$value,
                 files = files, dir = dir, seconds = seconds, timed_out = timed_out),
            class = "r_code_result")
}

# The text a model (or a person) reads about a run.
.format_code_result <- function(x) {
  part <- function(label, text) if (length(text) && any(nzchar(text))) paste0(label, ":\n", paste(text, collapse = "\n"))
  out <- c(part("Output", x$output),
           part("Messages", x$messages),
           part("Warnings", x$warnings),
           if (!is.na(x$error)) paste0("Error: ", x$error),
           if (!length(x$output) || !nzchar(x$output)) part("Last value", if (!is.na(x$value)) x$value),
           if (length(x$files)) paste0("Files in the working folder: ",
                                       paste(basename(x$files), collapse = ", ")))
  if (!length(out)) out <- "The code ran and printed nothing."
  paste(out, collapse = "\n\n")
}

#' @export
print.r_code_result <- function(x, ...) {
  status <- if (x$timed_out) "timed out" else if (is.na(x$error)) "ok" else "error"
  cat(sprintf("<r_code_result> %s | %.2f s\n", status, x$seconds))
  cat(.format_code_result(x), "\n")
  invisible(x)
}

#' A tool that lets an agent run R code
#'
#' Wraps [run_r_code()] so a model can compute instead of guessing: power
#' analyses, simulations, summaries of a data file. Each call starts a fresh
#' R process, but all calls of one tool share a working folder, so the code
#' can save objects with `saveRDS()` in one call and read them in the next.
#' Read the safety notes in [run_r_code()]: this is a soft sandbox, not a
#' security boundary. Set `approval = TRUE` to read and approve each piece
#' of code before it runs.
#'
#' When the code fails, the tool reports an error (counted in the episode
#' ledger) with the message and any output printed before the failure, so
#' the model can fix the code and try again. The tool output is not marked
#' as untrusted, because it is the result of the agent's own code; any text
#' the code downloads (with `allow_network = TRUE`) is not wrapped either, so
#' keep the network off unless the task needs it.
#'
#' @inheritParams run_r_code
#' @param approval If `TRUE`, every call waits for a person to approve the
#'   code (see [define_tool()]).
#' @param dir The working folder shared by all calls. Defaults to a new
#'   temporary folder.
#' @return An [define_tool()] object. The R value of each call keeps the
#'   full [run_r_code()] result in `attr(value, "data")`.
#' @examples
#' tool_run_r()
#' tool_run_r(packages = c("pwr"), timeout = 60, approval = TRUE)
#' @export
tool_run_r <- function(timeout = 30, allow_network = FALSE, packages = NULL,
                       approval = FALSE, dir = tempfile("agent_r_")) {
  fn <- function(code) {
    res <- run_r_code(code, timeout = timeout, allow_network = allow_network,
                      packages = packages, dir = dir)
    text <- .format_code_result(res)
    if (!is.na(res$error)) .stop("the code failed.\n", text)
    .tool_text(text, data = res)
  }
  # Tell the model what it can count on: code written for a package that is
  # not installed on this computer costs a failed step.
  attached <- if (length(packages)) {
    paste0(" These packages are already attached: ", paste(packages, collapse = ", "),
           ". Otherwise use base R: other packages may not be installed.")
  } else " Use base R (stats, utils): other packages may not be installed."
  define_tool(fn, name = "run_r",
    description = paste0(
      "Run R code in a fresh R session and return what it prints. Use it to compute ",
      "(power analyses, simulations, summaries) instead of estimating in your head. ",
      "Print every result you need to see. Objects do not persist between calls, but ",
      "files do: save with saveRDS('x.rds') and load with readRDS('x.rds'). ",
      "The time limit is ", timeout, " seconds",
      if (allow_network) "." else " and there is no internet access.", attached),
    args = list(code = arg_string("R code, for example 'pwr::pwr.t.test(d = 0.5, power = 0.8)'")),
    approval = approval, timeout = timeout + 30, max_output_tokens = 2000, untrusted = FALSE)
}
