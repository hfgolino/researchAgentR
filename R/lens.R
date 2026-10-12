# Lens: make agent runs visible. view_episode() writes one self-contained
# HTML page per episode (no CDN, no fonts, no libraries), view_episodes()
# compares several runs, plot_episode() draws per-step figures for slides,
# and reliability_curve() shows why long chains of steps fail.
#
# Two rules shape this file:
# 1. Every text that came from a model or a tool is escaped before it is
#    written into HTML. A tool result can contain "<script>" (prompt-injection
#    content found on the web); it must be shown, never executed.
# 2. Episodes read back from JSON (read_episode()) differ from fresh ones:
#    NA becomes NULL and character vectors become lists. Every function here
#    normalizes the episode first, so the same code serves both.

# ---- Normalizing episodes ----------------------------------------------------

.num1 <- function(x, default = NA_real_) {
  x <- unlist(x)
  if (length(x) == 0) return(default)
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) default else v
}

.chr1 <- function(x, default = "") {
  x <- unlist(x)
  if (length(x) == 0 || is.na(x[1])) return(default)
  as.character(x[1])
}

.chrv <- function(x) {
  x <- unlist(x)
  if (length(x) == 0) character(0) else as.character(x)
}

.lgl1 <- function(x) isTRUE(as.logical(unlist(x)[1]))

# One step with every field present and of a fixed type.
.lens_step <- function(s, i) {
  type <- .chr1(s$type, "model")
  out <- list(step = .num1(s$step, i), type = type, time = .chr1(s$time),
              latency = .num1(s$latency), cost = .num1(s$cost))
  if (type == "model") {
    c(out, list(
      model = .chr1(s$model), input = .num1(s$input, 0), cache_read = .num1(s$cache_read, 0),
      cache_write = .num1(s$cache_write, 0), output = .num1(s$output, 0),
      reasoning_tokens = .num1(s$reasoning_tokens, 0), cached = .lgl1(s$cached),
      context_tokens = .num1(s$context_tokens, 0), content = .chr1(s$content),
      reasoning = .chr1(s$reasoning), tool_calls = .chrv(s$tool_calls),
      executed_tools = .chrv(s$executed_tools), stop_reason = .chr1(s$stop_reason),
      notes = .chrv(s$notes), is_error = FALSE))
  } else {
    c(out, list(
      tool = .chr1(s$tool, "(unknown tool)"), arguments = .chr1(s$arguments),
      result = .chr1(s$result), result_chars = .num1(s$result_chars),
      is_error = .lgl1(s$is_error), truncated = .lgl1(s$truncated),
      child_episode = .chr1(s$child_episode, NA_character_)))
  }
}

.lens_episode <- function(ep) {
  if (is.character(ep) && length(ep) == 1 && file.exists(ep)) ep <- read_episode(ep)
  if (!is.list(ep) || !("steps" %in% names(ep))) {
    .stop("Pass an episode from run() or read_episode() (a list with `steps`).")
  }
  steps <- ep$steps %||% list()
  ep$steps <- lapply(seq_along(steps), function(i) .lens_step(steps[[i]], i))
  for (f in c("id", "agent", "model", "task", "started", "finished", "instructions", "context")) {
    ep[[f]] <- .chr1(ep[[f]], if (f %in% c("id", "agent", "model")) "(not recorded)" else "")
  }
  ep$stop_reason <- .chr1(ep$stop_reason, NA_character_)
  ep$seconds <- .num1(ep$seconds)
  ep$tools <- .chrv(ep$tools)
  class(ep) <- "episode"
  ep
}

# A list of normalized episodes from one episode, a list, files or a folder.
.lens_episodes <- function(episodes) {
  if (is.character(episodes)) {
    files <- if (length(episodes) == 1 && dir.exists(episodes)) {
      list.files(episodes, "\\.json$", full.names = TRUE)
    } else episodes
    if (!length(files)) .stop("No episode files (.json) found in ", episodes, ".")
    episodes <- lapply(files, read_episode)
  }
  if (inherits(episodes, "episode") || (is.list(episodes) && "steps" %in% names(episodes))) {
    episodes <- list(episodes)
  }
  lapply(episodes, .lens_episode)
}

# One row per step, numeric columns ready for plotting.
.lens_step_data <- function(ep) {
  rows <- lapply(ep$steps, function(s) {
    model <- s$type == "model"
    data.frame(
      step = s$step, type = s$type, name = if (model) s$model else s$tool,
      input = if (model) s$input + s$cache_write else NA_real_,
      cache_read = if (model) s$cache_read else NA_real_,
      output = if (model) s$output else NA_real_,
      context_tokens = if (model) s$context_tokens else NA_real_,
      cost = s$cost, latency = s$latency, is_error = s$is_error,
      stringsAsFactors = FALSE)
  })
  if (!length(rows)) {
    return(data.frame(step = numeric(0), type = character(0), name = character(0),
                      input = numeric(0), cache_read = numeric(0), output = numeric(0),
                      context_tokens = numeric(0), cost = numeric(0), latency = numeric(0),
                      is_error = logical(0)))
  }
  do.call(rbind, rows)
}

# ---- Formatting and escaping -------------------------------------------------

.html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  x <- enc2utf8(x)
  # Ampersand first, so the entities added below are not escaped twice.
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

.fmt_int <- function(x) {
  ifelse(is.na(x), "NA", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

.fmt_usd <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (v == 0) return("$0")
    if (abs(v) < 1) sprintf("$%.5f", v) else sprintf("$%.2f", v)
  }, character(1))
}

# "2026-12-02T10:15:00.123-0500" becomes "2026-12-02 10:15:00 -0500".
.fmt_time <- function(x) sub("^(\\d{4}-\\d{2}-\\d{2})T(\\d{2}:\\d{2}:\\d{2})(\\.\\d+)?", "\\1 \\2 ", x)

.fmt_sec <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f s", x))

.anchor <- function(id) paste0("ep-", gsub("[^A-Za-z0-9_-]", "_", id))

# ---- Inline SVG charts (drawn in R, no JavaScript) ---------------------------

# The two charts share one x scale (step 1..n), so a bar in the cost chart
# sits above the matching point in the context chart.
.svg_frame <- function(n, width = 760, height = 170, top = 16) {
  m <- c(l = 70, r = 16, t = top, b = 30)
  slot <- (width - m[["l"]] - m[["r"]]) / n
  list(width = width, height = height, m = m, slot = slot,
       ph = height - m[["t"]] - m[["b"]],
       x = function(i) m[["l"]] + (i - 0.5) * slot)
}

.svg_axes <- function(fr, n, ymax, fmt) {
  base <- fr$m[["t"]] + fr$ph
  ticks <- unique(round(c(1, pretty(c(1, n), n = 8), n)))
  ticks <- ticks[ticks >= 1 & ticks <= n]
  grid_y <- c(fr$m[["t"]], fr$m[["t"]] + fr$ph / 2)
  paste0(
    paste(sprintf('<line class="grid" x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f"/>',
                  fr$m[["l"]], fr$width - fr$m[["r"]], grid_y, grid_y), collapse = ""),
    sprintf('<line class="axis" x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f"/>',
            fr$m[["l"]], fr$width - fr$m[["r"]], base, base),
    paste(sprintf('<text class="tick" x="%.1f" y="%.1f" text-anchor="end">%s</text>',
                  fr$m[["l"]] - 6, c(base, grid_y[2], grid_y[1]) + 4,
                  .html_escape(c(fmt(0), fmt(ymax / 2), fmt(ymax)))), collapse = ""),
    paste(sprintf('<text class="tick" x="%.1f" y="%.1f" text-anchor="middle">%d</text>',
                  fr$x(ticks), base + 18, as.integer(ticks)), collapse = ""))
}

.svg_open <- function(fr, id, title) {
  sprintf(paste0('<svg class="chart" viewBox="0 0 %d %d" role="img" aria-labelledby="%s">',
                 '<title id="%s">%s</title>'),
          fr$width, fr$height, id, id, .html_escape(title))
}

.svg_cost <- function(ep, id = "cost") {
  steps <- ep$steps
  n <- length(steps)
  if (n == 0) return('<p class="muted">No steps were recorded.</p>')
  fr <- .svg_frame(n)
  cost <- vapply(steps, `[[`, numeric(1), "cost")
  type <- vapply(steps, `[[`, character(1), "type")
  name <- vapply(steps, function(s) if (s$type == "model") s$model else s$tool, character(1))
  ymax <- suppressWarnings(max(cost, na.rm = TRUE))
  all_zero <- !is.finite(ymax) || ymax <= 0
  if (all_zero) ymax <- 1
  h <- ifelse(is.na(cost), 0, cost / ymax * fr$ph)
  # A $0 step (free tool, cached call) gets a 2-pixel stub so it stays visible.
  h <- ifelse(!is.na(cost) & cost == 0, 2, h)
  bw <- max(1.5, min(fr$slot * 0.72, 40))
  base <- fr$m[["t"]] + fr$ph
  bars <- sprintf(
    '<rect class="bar-%s" x="%.1f" y="%.1f" width="%.1f" height="%.1f"><title>Step %d, %s %s: %s</title></rect>',
    type, fr$x(seq_len(n)) - bw / 2, base - h, bw, pmax(h, 0), seq_len(n), type,
    .html_escape(name), .fmt_usd(cost))
  # A missing price (cost NA) is marked, not drawn as zero.
  na <- which(is.na(cost))
  na_marks <- if (length(na)) sprintf('<text class="na-mark" x="%.1f" y="%.1f" text-anchor="middle">NA</text>',
                                      fr$x(na), base - 4) else ""
  fmt <- if (all_zero) function(v) if (v == 0) "$0" else "" else .fmt_usd
  paste0(.svg_open(fr, paste0(id, "-t"), "Cost per step at list price, in US dollars"),
         .svg_axes(fr, n, ymax, fmt), paste(bars, collapse = ""),
         paste(na_marks, collapse = ""), "</svg>",
         if (all_zero) '<p class="muted small">Every step cost $0 (free, local or replayed from the cache).</p>' else "")
}

.svg_context <- function(ep, id = "ctx") {
  steps <- ep$steps
  n <- length(steps)
  idx <- which(vapply(steps, function(s) s$type == "model", logical(1)))
  if (!length(idx)) return('<p class="muted">No model calls were recorded.</p>')
  fr <- .svg_frame(n, height = 170, top = 34)
  ctx <- vapply(steps[idx], `[[`, numeric(1), "context_tokens")
  ymax <- max(ctx, 1)
  x <- fr$x(idx)
  y <- fr$m[["t"]] + fr$ph - ctx / ymax * fr$ph
  peak <- which.max(ctx)
  # Direct label on the peak instead of a legend.
  # Placed above the peak point (the plot has extra top margin for it), so
  # it never sits on the line.
  anchor <- if (x[peak] > fr$width * 0.75) "end" else if (x[peak] < fr$width * 0.25) "start" else "middle"
  label <- sprintf('<text class="direct" x="%.1f" y="%.1f" text-anchor="%s">peak ~%s tokens</text>',
                   x[peak] + switch(anchor, end = 6, start = -6, middle = 0), y[peak] - 10,
                   anchor, .fmt_int(ctx[peak]))
  paste0(.svg_open(fr, paste0(id, "-t"), "Context sent to the model at each model call, in tokens"),
         .svg_axes(fr, n, ymax, .fmt_int),
         sprintf('<polyline class="ctx-line" points="%s"/>', paste(sprintf("%.1f,%.1f", x, y), collapse = " ")),
         paste(sprintf('<circle class="ctx-pt" cx="%.1f" cy="%.1f" r="3.5"><title>Step %d: ~%s tokens of context</title></circle>',
                       x, y, idx, .fmt_int(ctx)), collapse = ""),
         label, "</svg>")
}

# ---- HTML building blocks ----------------------------------------------------

.tile <- function(label, value, note = NULL) {
  sprintf('<div class="tile"><div class="tile-v">%s</div><div class="tile-l">%s</div>%s</div>',
          .html_escape(value), .html_escape(label),
          if (is.null(note)) "" else sprintf('<div class="tile-n">%s</div>', .html_escape(note)))
}

.pre <- function(x, cls = "mono") sprintf('<pre class="%s">%s</pre>', cls, .html_escape(x))

.chip <- function(x, cls = "chip") sprintf('<span class="%s">%s</span>', cls, .html_escape(x))

.html_model_step <- function(s) {
  tokens_in <- s$input + s$cache_read + s$cache_write
  chips <- c(
    .chip(sprintf("%s in%s", .fmt_int(tokens_in),
                  if (s$cache_read > 0) sprintf(" (%s cached)", .fmt_int(s$cache_read)) else "")),
    .chip(sprintf("%s out", .fmt_int(s$output))),
    if (s$reasoning_tokens > 0) .chip(sprintf("%s reasoning", .fmt_int(s$reasoning_tokens))),
    .chip(.fmt_usd(s$cost)), .chip(.fmt_sec(s$latency)),
    .chip(sprintf("context ~%s", .fmt_int(s$context_tokens))),
    if (s$cached) .chip("replayed from cache", "chip chip-info"))
  calls <- if (length(s$tool_calls)) {
    paste0('<div class="label">Tool calls requested</div><ul class="calls">',
           paste0("<li><code>", .html_escape(s$tool_calls), "</code></li>", collapse = ""), "</ul>")
  } else ""
  builtins <- if (length(s$executed_tools)) {
    paste0('<div class="label">Provider built-in tools run on the server</div><p>',
           paste(.html_escape(s$executed_tools), collapse = ", "), "</p>")
  } else ""
  text <- if (nzchar(s$content)) paste0('<div class="label">Assistant text</div>', .pre(s$content, "prose")) else ""
  reasoning <- if (nzchar(s$reasoning)) {
    sprintf('<details><summary>Reasoning (%s characters, as logged)</summary>%s</details>',
            .fmt_int(nchar(s$reasoning)), .pre(s$reasoning, "prose"))
  } else ""
  notes <- if (length(s$notes)) paste0('<p class="muted small">Notes: ', paste(.html_escape(s$notes), collapse = "; "), "</p>") else ""
  paste0('<div class="step-head"><span class="kind kind-model">model</span><span class="name">',
         .html_escape(s$model), '</span></div><div class="chips">', paste(chips, collapse = ""),
         "</div>", calls, builtins, text, reasoning, notes)
}

.html_tool_step <- function(s, known_ids = character(0)) {
  chips <- c(.chip(.fmt_sec(s$latency)), .chip(.fmt_usd(s$cost)),
             if (!is.na(s$result_chars)) .chip(sprintf("%s characters", .fmt_int(s$result_chars))),
             if (s$is_error) .chip("error", "chip chip-err"),
             if (s$truncated) .chip("output truncated for the model", "chip chip-info"))
  child <- if (!is.na(s$child_episode)) {
    id <- .html_escape(s$child_episode)
    target <- if (s$child_episode %in% known_ids) sprintf('<a href="#%s">%s</a>', .anchor(s$child_episode), id) else
      paste0("<code>", id, "</code>")
    paste0('<p>Sub-agent episode: ', target, "</p>")
  } else ""
  args <- if (nzchar(s$arguments)) paste0('<div class="label">Arguments</div>', .pre(s$arguments)) else ""
  result <- sprintf('<details%s><summary>%s (%s characters logged)</summary>%s</details>',
                    if (s$is_error) " open" else "", if (s$is_error) "Error returned to the model" else "Result",
                    .fmt_int(nchar(s$result)), .pre(s$result))
  paste0('<div class="step-head"><span class="kind kind-tool">tool</span><span class="name">',
         .html_escape(s$tool), '</span></div><div class="chips">', paste(chips, collapse = ""),
         "</div>", args, result, child)
}

.html_answer <- function(ep) {
  a <- ep$answer
  if (is.null(a) || length(a) == 0) {
    return(sprintf('<p class="muted">No final answer (the episode stopped with <code>%s</code>).</p>',
                   .html_escape(ep$stop_reason)))
  }
  if (is.character(a) && length(a) == 1) return(.pre(a, "prose answer"))
  .pre(as.character(.to_json(a, pretty = TRUE)), "mono answer")
}

# The body of one episode: header, tiles, charts, timeline, answer.
.html_episode_body <- function(ep, known_ids = character(0), level = 1) {
  t <- .totals(ep)
  h <- function(k) paste0("h", min(level + k, 6))
  key <- .anchor(ep$id)
  steps <- vapply(seq_along(ep$steps), function(i) {
    s <- ep$steps[[i]]
    cls <- paste0("step step-", s$type, if (s$is_error) " step-err" else "")
    body <- if (s$type == "model") .html_model_step(s) else .html_tool_step(s, known_ids)
    sprintf('<li class="%s" data-type="%s" data-error="%s"><div class="num">%d</div><div class="body">%s</div></li>',
            cls, s$type, tolower(s$is_error), as.integer(s$step), body)
  }, character(1))
  budget <- ep$budget
  budget_txt <- if (length(budget)) {
    b <- unlist(budget)
    paste0('<p class="muted small">Budget: ', .html_escape(paste(names(b), b, sep = " = ", collapse = "; ")), "</p>")
  } else ""
  stop_cls <- if (identical(ep$stop_reason, "final_answer")) "chip chip-ok" else "chip chip-err"
  paste0(
    sprintf('<header class="ep-head"><%s>%s</%s>', h(0), .html_escape(ep$agent), h(0)),
    '<dl class="meta">',
    sprintf("<div><dt>Model</dt><dd><code>%s</code></dd></div>", .html_escape(ep$model)),
    sprintf('<div><dt>Stop reason</dt><dd><span class="%s">%s</span></dd></div>', stop_cls, .html_escape(ep$stop_reason)),
    sprintf("<div><dt>Started</dt><dd>%s</dd></div>", .html_escape(.fmt_time(ep$started))),
    sprintf("<div><dt>Episode</dt><dd><code>%s</code></dd></div>", .html_escape(ep$id)),
    if (nzchar(ep$context)) sprintf("<div><dt>Context strategy</dt><dd>%s</dd></div>", .html_escape(ep$context)) else "",
    if (length(ep$tools)) sprintf("<div><dt>Tools offered</dt><dd>%s</dd></div>", .html_escape(paste(ep$tools, collapse = ", "))) else "",
    "</dl>",
    sprintf('<div class="label">Task</div>%s', .pre(ep$task, "prose task")),
    if (nzchar(ep$instructions)) sprintf("<details><summary>Instructions (system prompt)</summary>%s</details>", .pre(ep$instructions, "prose")) else "",
    budget_txt, "</header>",
    '<section class="tiles" aria-label="Episode totals">',
    .tile("model calls", .fmt_int(t$model_calls)),
    .tile("tool calls", .fmt_int(t$tool_calls)),
    .tile("tool errors", .fmt_int(t$tool_errors)),
    .tile("billed tokens", .fmt_int(t$tokens), sprintf("%s in, %s out", .fmt_int(t$input + t$cache_read + t$cache_write), .fmt_int(t$output))),
    .tile("cached tokens", .fmt_int(t$cache_read)),
    .tile("peak context", .fmt_int(t$peak_context), "tokens, estimated"),
    .tile("cost at list price", .fmt_usd(t$cost), if (is.na(t$cost)) "a model has no price in the registry" else "US dollars"),
    .tile("seconds", if (is.na(t$seconds)) "NA" else sprintf("%.1f", t$seconds)),
    "</section>",
    sprintf("<section><%s>Cost and context per step</%s>", h(1), h(1)),
    '<p class="legend"><span class="sw sw-model"></span>model call <span class="sw sw-tool"></span>tool call ',
    '<span class="sw sw-ctx"></span>context tokens (model calls only)</p>',
    '<div class="chartwrap">', .svg_cost(ep, paste0(key, "-cost")), .svg_context(ep, paste0(key, "-ctx")),
    "</div></section>",
    sprintf("<section><%s>Timeline</%s>", h(1), h(1)),
    '<ol class="timeline">', paste(steps, collapse = ""), "</ol></section>",
    sprintf("<section><%s>Final answer</%s>", h(1), h(1)), .html_answer(ep), "</section>")
}

.html_page <- function(title, body) {
  paste0(
    '<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>", .html_escape(title), "</title><style>", .lens_css, "</style></head><body>",
    '<div class="bar-top"><span class="brand">researchAgentR Lens</span>',
    '<span class="controls"><label>Show <select id="filter"><option value="all">all steps</option>',
    '<option value="model">model calls</option><option value="tool">tool calls</option>',
    '<option value="error">errors only</option></select></label>',
    '<button type="button" data-act="expand">Expand all</button>',
    '<button type="button" data-act="collapse">Collapse all</button></span></div>',
    '<main>', body,
    '<p class="notice">Model and tool text on this page is shown as plain text: it was escaped when the ',
    "page was written, so nothing in it can run. Treat it as untrusted data. Token counts are reported by ",
    "the provider; context sizes are estimates (about 4 characters per token); costs use list prices from ",
    "<code>model_registry()</code>.</p></main><script>", .lens_js, "</script></body></html>\n")
}

.write_html <- function(html, file, open) {
  write_text(html, file, encoding = "UTF-8")
  if (isTRUE(open)) utils::browseURL(normalizePath(file))
  invisible(file)
}

# ---- Exported viewers --------------------------------------------------------

#' View an episode as a self-contained HTML page
#'
#' Writes one HTML file that shows everything an agent did in one episode:
#' the task, the stop reason, totals (model calls, tool calls, tool errors,
#' billed and cached tokens, peak context, cost at list price, seconds), a
#' chart of cost per step and of the context sent at each model call, a
#' timeline with every model call (tokens, cost, latency, tool calls
#' requested, assistant text, reasoning) and every tool call (arguments,
#' result, errors, sub-agent episode), and the final answer.
#'
#' The page needs no internet connection: styles, the two charts (drawn as
#' SVG by R) and a few lines of JavaScript are inside the file. All model and
#' tool text is escaped, so a web page that tried a prompt injection with
#' `<script>` shows up as text and never runs.
#'
#' @param episode An episode from [run()], an episode read with
#'   [read_episode()], or the path of a saved episode (.json).
#' @param file Path of the HTML file. Defaults to a file named after the
#'   episode in [tempdir()].
#' @param open Open the page in the browser with [utils::browseURL()]?
#' @return The path of the HTML file, invisibly.
#' @seealso [view_episodes()] to compare several episodes, [plot_episode()]
#'   for figures on slides.
#' @examples
#' \dontrun{
#' ep <- run(scout, "Find the DOI of the EGA paper by Golino and Epskamp (2017).")
#' view_episode(ep)
#' view_episode("episodes/20261202-101500-a1b2.json", file = "scout.html")
#' }
#' @export
view_episode <- function(episode, file = NULL, open = interactive()) {
  ep <- .lens_episode(episode)
  if (is.null(file)) file <- file.path(tempdir(), paste0("episode-", gsub("[^A-Za-z0-9_-]", "_", ep$id), ".html"))
  html <- .html_page(paste("Episode", ep$id, "|", ep$agent),
                     sprintf('<article id="%s">%s</article>', .anchor(ep$id), .html_episode_body(ep)))
  .write_html(html, file, open)
}

#' Compare several episodes on one HTML page
#'
#' Writes one self-contained HTML file with a comparison table (one row per
#' episode, from [episodes_df()]) linked to a full section for each episode,
#' as in [view_episode()]. Sub-agent episodes included in the list are linked
#' from the tool call that started them.
#'
#' @param episodes A list of episodes, a vector of saved episode files, or a
#'   folder of saved episodes.
#' @param file Path of the HTML file. Defaults to `episodes.html` in
#'   [tempdir()].
#' @param open Open the page in the browser?
#' @return The path of the HTML file, invisibly.
#' @examples
#' \dontrun{
#' view_episodes(list(ep_cheap, ep_strong), file = "compare.html")
#' view_episodes("episodes")
#' }
#' @export
view_episodes <- function(episodes, file = NULL, open = interactive()) {
  eps <- .lens_episodes(episodes)
  if (is.null(file)) file <- file.path(tempdir(), "episodes.html")
  df <- episodes_df(eps)
  ids <- vapply(eps, `[[`, character(1), "id")
  max_cost <- suppressWarnings(max(df$cost, na.rm = TRUE))
  rows <- vapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    w <- if (is.finite(max_cost) && max_cost > 0 && !is.na(r$cost)) 100 * r$cost / max_cost else 0
    sprintf(paste0('<tr><td><a href="#%s">%s</a></td><td>%s</td><td><code>%s</code></td><td>%s</td>',
                   '<td>%s</td><td class="r">%s</td><td class="r">%s</td><td class="r">%s</td>',
                   '<td class="r">%s</td><td class="r">%s</td><td class="r costcell">%s',
                   '<span class="cbar" style="width:%.0f%%"></span></td><td class="r">%s</td></tr>'),
            .anchor(ids[i]), .html_escape(r$id), .html_escape(r$agent), .html_escape(r$model),
            .html_escape(r$task), .html_escape(r$stop_reason), .fmt_int(r$model_calls),
            .fmt_int(r$tool_calls), .fmt_int(r$tool_errors), .fmt_int(r$tokens),
            .fmt_int(r$peak_context), .fmt_usd(r$cost), w, .fmt_sec(r$seconds))
  }, character(1))
  total_cost <- if (anyNA(df$cost)) NA_real_ else sum(df$cost)
  table <- paste0(
    "<h1>Episodes compared</h1>",
    sprintf('<p>%d episodes; %s billed tokens; total cost at list price %s.</p>',
            nrow(df), .fmt_int(sum(df$tokens)), .fmt_usd(total_cost)),
    '<div class="tablewrap"><table><thead><tr><th>Episode</th><th>Agent</th><th>Model</th><th>Task</th>',
    '<th>Stop reason</th><th class="r">Model calls</th><th class="r">Tool calls</th><th class="r">Tool errors</th>',
    '<th class="r">Tokens</th><th class="r">Peak context</th><th class="r">Cost</th><th class="r">Time</th></tr></thead><tbody>',
    paste(rows, collapse = ""), "</tbody></table></div>")
  sections <- vapply(eps, function(ep) {
    sprintf('<article id="%s" class="ep">%s<p><a href="#top">Back to the table</a></p></article>',
            .anchor(ep$id), .html_episode_body(ep, known_ids = ids, level = 2))
  }, character(1))
  html <- .html_page("Episodes compared", paste0('<div id="top"></div>', table, paste(sections, collapse = "")))
  .write_html(html, file, open)
}

# ---- Figures for slides ------------------------------------------------------

# Okabe-Ito colors: distinguishable with the common forms of color blindness.
.lens_palette <- c(model = "#0072B2", tool = "#E69F00", cached = "#56B4E9",
                   output = "#009E73", context = "#CC79A7", error = "#D55E00")

# Spread label positions so that no two are closer than `gap`.
.spread <- function(y, gap) {
  o <- order(y)
  z <- y[o]
  for (i in seq_along(z)[-1]) z[i] <- max(z[i], z[i - 1] + gap)
  y[o] <- z
  y
}

#' Plot an episode step by step
#'
#' Figures for slides: billed tokens per model call (input, cached input and
#' output), cost per step (model calls and tool calls), or the context sent
#' at each model call. Series are labeled directly on the plot, in
#' colorblind-safe Okabe-Ito colors. Uses ggplot2 when it is installed and
#' base graphics otherwise.
#'
#' @param episode An episode from [run()] or [read_episode()].
#' @param what `"tokens"`, `"cost"` or `"context"`.
#' @return A ggplot object, or (with base graphics) the plotted data,
#'   invisibly.
#' @examples
#' \dontrun{
#' plot_episode(ep, "tokens")
#' plot_episode(ep, "context")
#' }
#' @export
plot_episode <- function(episode, what = c("tokens", "cost", "context")) {
  what <- match.arg(what)
  ep <- .lens_episode(episode)
  d <- .lens_step_data(ep)
  if (!nrow(d)) .stop("The episode has no steps to plot.")
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    switch(what, tokens = .gg_tokens(d, ep), cost = .gg_cost(d, ep), context = .gg_context(d, ep))
  } else {
    .base_plot_episode(d, ep, what)
  }
}

.gg_theme <- function() {
  ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
}

.token_long <- function(d) {
  m <- d[d$type == "model", ]
  long <- data.frame(
    step = rep(m$step, 3),
    part = factor(rep(c("Input", "Cached input", "Output"), each = nrow(m)),
                  levels = c("Output", "Cached input", "Input")),
    tokens = c(m$input, m$cache_read, m$output))
  long
}

.gg_tokens <- function(d, ep) {
  long <- .token_long(d)
  if (!nrow(long)) .stop("The episode has no model calls.")
  cols <- c(Input = .lens_palette[["model"]], `Cached input` = .lens_palette[["cached"]],
            Output = .lens_palette[["output"]])
  # Direct labels: a column of colored labels at the right of the bars, in
  # the same top-to-bottom order as the stack, each with its total. Every
  # series is labeled, even one that is absent from the last bar.
  last <- max(long$step)
  ymax <- max(tapply(long$tokens, long$step, sum))
  totals <- tapply(long$tokens, long$part, sum)
  lab <- data.frame(part = factor(levels(long$part), levels = levels(long$part)),
                    y = ymax * c(0.95, 0.80, 0.65))
  lab$text <- sprintf("%s: %s", lab$part, .fmt_int(totals[as.character(lab$part)]))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$step, y = .data$tokens, fill = .data$part)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = last + 0.6, y = .data$y, label = .data$text,
                                                colour = .data$part),
                       hjust = 0, size = 4.5, fontface = "bold") +
    ggplot2::scale_fill_manual(values = cols) + ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_x_continuous(breaks = unique(long$step), expand = ggplot2::expansion(mult = c(0.04, 0.6))) +
    ggplot2::scale_y_continuous(labels = .fmt_int) +
    ggplot2::labs(x = "Step (model calls only)", y = "Tokens billed",
                  title = sprintf("Tokens per model call: %s", ep$agent),
                  subtitle = sprintf("%s | %s billed tokens", ep$model, .fmt_int(sum(long$tokens)))) +
    .gg_theme()
}

.int_breaks <- function(lim) {
  b <- pretty(lim)
  unique(round(b[b >= 1]))
}

.gg_cost <- function(d, ep) {
  d$cost_plot <- ifelse(is.na(d$cost), 0, d$cost)
  d$label <- .fmt_usd(d$cost)
  cols <- c(model = .lens_palette[["model"]], tool = .lens_palette[["tool"]])
  # Direct labels: name each type once, above its most expensive step.
  lab <- do.call(rbind, lapply(split(d, d$type), function(x) x[which.max(x$cost_plot), ]))
  lab$text <- ifelse(lab$type == "model", "model calls", "tool calls")
  ymax <- max(d$cost_plot)
  if (ymax == 0) ymax <- 1e-6
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$step, y = .data$cost_plot, fill = .data$type)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(data = lab, ggplot2::aes(y = .data$cost_plot + ymax * 0.17, label = .data$text,
                                                colour = .data$type),
                       size = 4.5, fontface = "bold") +
    ggplot2::scale_fill_manual(values = cols) + ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_x_continuous(breaks = .int_breaks) +
    ggplot2::scale_y_continuous(labels = .fmt_usd, expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::labs(x = "Step", y = "Cost (US dollars, list price)",
                  title = sprintf("Cost per step: %s", ep$agent),
                  subtitle = sprintf("%s | episode cost %s", ep$model, .fmt_usd(.totals(ep)$cost))) +
    .gg_theme()
  if (nrow(d) <= 20) {
    g <- g + ggplot2::geom_text(ggplot2::aes(y = .data$cost_plot, label = .data$label),
                                vjust = -0.4, size = 3.4, colour = "grey30")
  }
  g
}

.gg_context <- function(d, ep) {
  m <- d[d$type == "model", ]
  if (!nrow(m)) .stop("The episode has no model calls.")
  peak <- m[which.max(m$context_tokens), ]
  col <- .lens_palette[["context"]]
  ggplot2::ggplot(m, ggplot2::aes(x = .data$step, y = .data$context_tokens)) +
    ggplot2::geom_line(colour = col, linewidth = 1.2) +
    ggplot2::geom_point(colour = col, size = 3) +
    ggplot2::annotate("text", x = peak$step, y = peak$context_tokens,
                      label = sprintf("peak ~%s tokens", .fmt_int(peak$context_tokens)),
                      vjust = -1, colour = col, fontface = "bold", size = 4.5) +
    ggplot2::scale_x_continuous(breaks = .int_breaks) +
    ggplot2::scale_y_continuous(labels = .fmt_int, limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(x = "Step", y = "Context sent (tokens, estimated)",
                  title = sprintf("Context at each model call: %s", ep$agent),
                  subtitle = "Each call resends the conversation, so context grows with every step") +
    .gg_theme()
}

# Base-graphics versions, used only when ggplot2 is not installed.
.base_plot_episode <- function(d, ep, what) {
  if (what == "tokens") {
    m <- d[d$type == "model", ]
    mat <- rbind(Input = m$input, `Cached input` = m$cache_read, Output = m$output)
    cols <- .lens_palette[c("model", "cached", "output")]
    mids <- graphics::barplot(mat, names.arg = m$step, col = cols, border = NA, xlab = "Step",
                              ylab = "Tokens billed", main = paste("Tokens per model call:", ep$agent),
                              xlim = c(0, ncol(mat) * 1.2 * 1.45))
    last <- mat[, ncol(mat)]
    y <- .spread(cumsum(last) - last / 2, max(colSums(mat)) * 0.07)
    graphics::text(max(mids) + 0.7, y, rownames(mat), col = cols, pos = 4, font = 2, xpd = TRUE)
    return(invisible(m))
  }
  if (what == "cost") {
    cost <- ifelse(is.na(d$cost), 0, d$cost)
    cols <- ifelse(d$type == "model", .lens_palette[["model"]], .lens_palette[["tool"]])
    mids <- graphics::barplot(cost, names.arg = d$step, col = cols, border = NA, xlab = "Step",
                              ylab = "Cost (US dollars)", main = paste("Cost per step:", ep$agent),
                              ylim = c(0, max(cost, 1e-6) * 1.25))
    for (ty in unique(d$type)) {
      i <- which(d$type == ty)[which.max(cost[d$type == ty])]
      graphics::text(mids[i], cost[i], paste(ty, "calls"), pos = 3, font = 2,
                     col = .lens_palette[[ty]])
    }
    return(invisible(d))
  }
  m <- d[d$type == "model", ]
  graphics::plot(m$step, m$context_tokens, type = "b", pch = 19, lwd = 2,
                 col = .lens_palette[["context"]], ylim = c(0, max(m$context_tokens, 1) * 1.15),
                 xlab = "Step", ylab = "Context sent (tokens, estimated)",
                 main = paste("Context at each model call:", ep$agent))
  i <- which.max(m$context_tokens)
  graphics::text(m$step[i], m$context_tokens[i], paste("peak ~", .fmt_int(m$context_tokens[i])),
                 pos = 3, col = .lens_palette[["context"]], font = 2)
  invisible(m)
}

#' How per-step reliability compounds over many steps
#'
#' If each step of an agent succeeds independently with probability $p$, a
#' task that needs $n$ steps succeeds with probability $p^n$. At 95% per step,
#' 10 steps succeed about 60% of the time and 50 steps about 8%. The numbers
#' are computed here, not typed in. The example follows the lecture on agent
#' reliability in Harvard CS2680, Modern AI Systems (Fall 2026,
#' <https://cs2680.seas.harvard.edu>).
#'
#' Independence is a simplification: real failures cluster (one wrong
#' search result misleads every later step), which is one reason to record
#' every step and to measure pass^k over repeated trials.
#'
#' @param p Per-step success probabilities, each between 0 and 1.
#' @param steps Numbers of steps (positive integers).
#' @param plot Draw the curves (ggplot2 when installed, base graphics
#'   otherwise)?
#' @return A data frame with `p`, `steps`, `success` ($p^n$) and `label`.
#'   With `plot = TRUE`, the ggplot object is in the attribute `"plot"` and
#'   the data frame is returned invisibly.
#' @examples
#' r <- reliability_curve()
#' r[r$steps %in% c(10, 50) & r$p == 0.95, ]
#' \donttest{
#' reliability_curve(plot = TRUE)
#' }
#' @export
reliability_curve <- function(p = c(0.99, 0.95, 0.90), steps = 1:50, plot = FALSE) {
  if (!is.numeric(p) || any(is.na(p)) || any(p < 0 | p > 1)) .stop("`p` must be probabilities between 0 and 1.")
  if (!is.numeric(steps) || any(is.na(steps)) || any(steps < 1)) .stop("`steps` must be positive integers.")
  d <- expand.grid(steps = as.integer(steps), p = p)
  d$success <- d$p^d$steps
  d$label <- sprintf("%s%% per step", format(100 * d$p, trim = TRUE))
  d <- d[, c("p", "steps", "success", "label")]
  if (!plot) return(d)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    g <- .gg_reliability(d)
    print(g)
    attr(d, "plot") <- g
  } else {
    .base_reliability(d)
  }
  invisible(d)
}

.reliability_colors <- function(n) {
  rep(unname(.lens_palette[c("model", "tool", "output", "context", "error", "cached")]), length.out = n)
}

.gg_reliability <- function(d) {
  last <- max(d$steps)
  ends <- d[d$steps == last, ]
  ends$text <- sprintf("%s: %.1f%%", ends$label, 100 * ends$success)
  ends$y <- .spread(ends$success, 0.07)
  cols <- stats::setNames(.reliability_colors(length(unique(d$label))), unique(d$label))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$steps, y = .data$success, colour = .data$label)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::geom_point(data = ends, size = 3) +
    ggplot2::geom_text(data = ends, ggplot2::aes(x = last + max(1, last * 0.02), y = .data$y, label = .data$text),
                       hjust = 0, size = 4.5, fontface = "bold") +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%"), limits = c(0, 1)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.5))) +
    ggplot2::labs(x = "Number of steps (n)", y = "Probability that every step succeeds",
                  title = "Small per-step errors compound over long tasks",
                  subtitle = sprintf("Task success = p^n for n independent steps; labels give the value at n = %d", last)) +
    .gg_theme()
}

.base_reliability <- function(d) {
  labs <- unique(d$label)
  cols <- .reliability_colors(length(labs))
  last <- max(d$steps)
  graphics::plot(range(d$steps), c(0, 1), type = "n", xlim = c(min(d$steps), last * 1.5),
                 xlab = "Number of steps (n)", ylab = "Probability that every step succeeds",
                 main = "Small per-step errors compound over long tasks")
  for (i in seq_along(labs)) {
    x <- d[d$label == labs[i], ]
    graphics::lines(x$steps, x$success, col = cols[i], lwd = 2)
  }
  ends <- d[d$steps == last, ]
  graphics::text(last, .spread(ends$success, 0.07),
                 sprintf("%s: %.1f%%", ends$label, 100 * ends$success),
                 pos = 4, col = cols, font = 2)
}

# ---- Page styles and script --------------------------------------------------

# Colors are CSS variables, redefined for dark mode; the SVG charts use the
# same variables, so they follow the theme too. Palette: Okabe-Ito.
.lens_css <- "
:root{--bg:#ffffff;--fg:#1b1f24;--muted:#57606a;--line:#d0d7de;--panel:#f6f8fa;--code:#eef1f4;
--c-model:#0072B2;--c-tool:#B86E00;--c-ctx:#AA4A86;--c-err:#C0430B;--c-ok:#0B7A55;--c-info:#005A8C;--link:#0059b3}
@media (prefers-color-scheme: dark){:root{--bg:#0f1419;--fg:#e6e8eb;--muted:#a0a8b3;--line:#30363d;
--panel:#161b22;--code:#1f252d;--c-model:#56B4E9;--c-tool:#E69F00;--c-ctx:#E58FC4;--c-err:#FF8A5C;
--c-ok:#3FD49B;--c-info:#7CC4F0;--link:#79b8ff}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.5 system-ui,-apple-system,'Segoe UI',Roboto,sans-serif}
main{max-width:1080px;margin:0 auto;padding:16px 20px 48px}
a{color:var(--link)}
code,pre{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:.88em}
h1{font-size:1.6rem;margin:.4em 0 .3em}h2{font-size:1.25rem;margin:1.4em 0 .5em}h3{font-size:1.1rem;margin:1.2em 0 .5em}
.bar-top{position:sticky;top:0;z-index:2;display:flex;flex-wrap:wrap;gap:8px;justify-content:space-between;align-items:center;
padding:8px 20px;background:var(--panel);border-bottom:1px solid var(--line)}
.brand{font-weight:700}
.controls{display:flex;gap:8px;align-items:center;flex-wrap:wrap}
button,select{font:inherit;font-size:.9rem;color:var(--fg);background:var(--bg);border:1px solid var(--line);border-radius:6px;padding:3px 10px;cursor:pointer}
button:focus-visible,select:focus-visible,summary:focus-visible,a:focus-visible{outline:3px solid var(--c-info);outline-offset:2px}
.meta{display:flex;flex-wrap:wrap;gap:6px 28px;margin:0 0 10px}
.meta div{min-width:0}.meta dt{font-size:.78rem;text-transform:uppercase;letter-spacing:.04em;color:var(--muted)}
.meta dd{margin:0;overflow-wrap:anywhere}
.label{font-size:.78rem;text-transform:uppercase;letter-spacing:.04em;color:var(--muted);margin:10px 0 2px}
pre{margin:0;padding:8px 10px;background:var(--code);border-radius:6px;white-space:pre-wrap;overflow-wrap:anywhere;max-height:420px;overflow:auto}
pre.prose{font-family:inherit;font-size:.95rem}
pre.task{font-size:1rem}
pre.answer{max-height:none;border-left:4px solid var(--c-ok)}
.tiles{display:grid;grid-template-columns:repeat(2,1fr);gap:10px;margin:18px 0}
@media (min-width:700px){.tiles{grid-template-columns:repeat(4,1fr)}}
.tile{background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:10px 12px}
.tile-v{font-size:1.45rem;font-weight:700;font-variant-numeric:tabular-nums}
.tile-l{font-size:.85rem;color:var(--muted)}.tile-n{font-size:.75rem;color:var(--muted)}
.legend{font-size:.88rem;color:var(--muted);margin:0 0 4px}
.sw{display:inline-block;width:12px;height:12px;border-radius:2px;margin:0 4px 0 12px;vertical-align:-1px}
.sw:first-child{margin-left:0}
.sw-model{background:var(--c-model)}.sw-tool{background:var(--c-tool)}.sw-ctx{background:var(--c-ctx)}
.chartwrap{overflow-x:auto}
svg.chart{width:100%;min-width:560px;height:auto;display:block;margin:4px 0 10px}
svg .bar-model{fill:var(--c-model)}svg .bar-tool{fill:var(--c-tool)}
svg .grid{stroke:var(--line);stroke-dasharray:3 3}svg .axis{stroke:var(--muted)}
svg .tick{fill:var(--muted);font-size:12px}svg .na-mark{fill:var(--c-err);font-size:11px;font-weight:700}
svg .ctx-line{fill:none;stroke:var(--c-ctx);stroke-width:2.5}svg .ctx-pt{fill:var(--c-ctx)}
svg .direct{fill:var(--c-ctx);font-size:13px;font-weight:700}
.timeline{list-style:none;padding:0;margin:0}
.step{display:flex;gap:12px;padding:10px 0;border-top:1px solid var(--line)}
.step .num{flex:0 0 2.2em;height:2.2em;border-radius:50%;display:flex;align-items:center;justify-content:center;
font-weight:700;font-size:.9rem;background:var(--panel);border:2px solid var(--c-model)}
.step-tool .num{border-color:var(--c-tool)}.step-err .num{border-color:var(--c-err);color:var(--c-err)}
.step .body{flex:1;min-width:0}
.step-err .body{border-left:4px solid var(--c-err);padding-left:10px}
.step-head{display:flex;gap:8px;align-items:baseline;flex-wrap:wrap}
.name{font-weight:600;overflow-wrap:anywhere}
.kind{font-size:.72rem;text-transform:uppercase;letter-spacing:.05em;font-weight:700;padding:1px 6px;border-radius:4px;color:var(--bg)}
.kind-model{background:var(--c-model)}.kind-tool{background:var(--c-tool)}
.chips{display:flex;flex-wrap:wrap;gap:6px;margin:6px 0}
.chip{font-size:.8rem;padding:1px 8px;border:1px solid var(--line);border-radius:999px;font-variant-numeric:tabular-nums}
.chip-err{border-color:var(--c-err);color:var(--c-err);font-weight:700}
.chip-ok{border-color:var(--c-ok);color:var(--c-ok);font-weight:700}
.chip-info{border-color:var(--c-info);color:var(--c-info)}
ul.calls{margin:2px 0;padding-left:20px}ul.calls code{overflow-wrap:anywhere}
details{margin:6px 0}summary{cursor:pointer;color:var(--muted);font-size:.9rem}
.muted{color:var(--muted)}.small{font-size:.85rem}
.notice{margin-top:32px;padding:10px 12px;border:1px solid var(--line);border-radius:8px;font-size:.85rem;color:var(--muted)}
.tablewrap{overflow-x:auto}
table{border-collapse:collapse;width:100%;font-size:.9rem;font-variant-numeric:tabular-nums}
th,td{padding:6px 8px;border-bottom:1px solid var(--line);text-align:left;vertical-align:top}
th{font-size:.78rem;text-transform:uppercase;letter-spacing:.03em;color:var(--muted)}
.r{text-align:right}td.r{white-space:nowrap}
.costcell{min-width:96px}
.cbar{display:block;height:6px;margin-top:4px;margin-left:auto;background:var(--c-model);border-radius:3px}
article.ep{margin-top:40px;padding-top:10px;border-top:3px solid var(--line)}
[hidden]{display:none!important}
"

# A few lines of plain JavaScript: expand or collapse every section, and
# filter the timeline. The page works without it.
.lens_js <- "
document.querySelectorAll('[data-act]').forEach(function(b){
  b.addEventListener('click', function(){
    var open = b.getAttribute('data-act') === 'expand';
    document.querySelectorAll('details').forEach(function(d){ d.open = open; });
  });
});
document.getElementById('filter').addEventListener('change', function(e){
  var v = e.target.value;
  document.querySelectorAll('li.step').forEach(function(li){
    var show = v === 'all' || (v === 'error' ? li.getAttribute('data-error') === 'true' : li.getAttribute('data-type') === v);
    li.hidden = !show;
  });
});
"
