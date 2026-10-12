# A minimal Model Context Protocol (MCP) client. MCP servers offer tools
# (and resources and prompts) to any client through JSON-RPC 2.0 messages.
# This file implements the parts an agent needs: connect, list tools, call a
# tool, and turn the server's tools into agent tools.
#
# Two protocol eras are supported (verified 2026-10-10 against
# https://modelcontextprotocol.io/specification/latest/changelog and
# https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http.md):
# * Modern, 2026-07-28: stateless. No initialize handshake and no sessions;
#   every request carries its protocol version, client identity and
#   capabilities in params$`_meta`, and HTTP requests repeat the version,
#   method and tool name in headers.
# * Legacy, 2025-11-25 (and 2025-06-18, 2025-03-26): an `initialize` request,
#   then a `notifications/initialized` notification, then requests that carry
#   the session id the server returned (Mcp-Session-Id header).
# mcp_connect() tries modern first and falls back to legacy, as the
# specification recommends for clients that support both.

.mcp_modern <- "2026-07-28"
.mcp_legacy <- "2025-11-25"
# Legacy versions whose tools/list and tools/call messages this client handles.
.mcp_legacy_ok <- c("2025-11-25", "2025-06-18", "2025-03-26")
# JSON-RPC error codes that only a modern server sends: HeaderMismatch,
# MissingRequiredClientCapability, UnsupportedProtocolVersion.
.mcp_modern_errors <- c(-32020, -32021, -32022)

.mcp_is_modern <- function(version) !is.null(version) && version >= .mcp_modern

.mcp_client_info <- function() {
  list(name = "researchAgentR", version = as.character(utils::packageVersion("researchAgentR")))
}

# ---- Messages and headers ------------------------------------------------------

# One JSON-RPC message. A request has an id; a notification has none. Modern
# requests carry the required `_meta` fields in params.
.mcp_message <- function(method, params = NULL, id = NULL, version = .mcp_modern) {
  if (.mcp_is_modern(version)) {
    params <- params %||% list()
    params[["_meta"]] <- c(params[["_meta"]], list(
      `io.modelcontextprotocol/protocolVersion` = version,
      `io.modelcontextprotocol/clientInfo` = .mcp_client_info(),
      `io.modelcontextprotocol/clientCapabilities` = .empty_object()))
  }
  msg <- list(jsonrpc = "2.0")
  if (!is.null(id)) msg$id <- id
  msg$method <- method
  if (length(params)) msg$params <- params
  msg
}

# HTTP headers for one POST. Modern servers check that the headers match the
# body (a mismatch is HTTP 400 with JSON-RPC error -32020), because proxies
# route on headers without reading the body.
.mcp_headers <- function(msg, version, session_id = NULL, schema = NULL, initialize = FALSE) {
  h <- list(Accept = "application/json, text/event-stream")
  # Legacy servers learn the version from initialize; the header comes after it.
  if (!initialize) h[["MCP-Protocol-Version"]] <- version
  if (.mcp_is_modern(version)) {
    h[["Mcp-Method"]] <- msg$method
    if (msg$method %in% c("tools/call", "resources/read", "prompts/get")) {
      h[["Mcp-Name"]] <- .mcp_header_value(msg$params$name %||% msg$params$uri)
    }
    if (identical(msg$method, "tools/call") && !is.null(schema)) {
      h <- c(h, .mcp_param_headers(schema, msg$params$arguments))
    }
  } else if (!is.null(session_id)) {
    h[["Mcp-Session-Id"]] <- session_id
  }
  h
}

# A header value: plain when it is safe visible ASCII, otherwise the Base64
# sentinel form =?base64?...?= that the specification defines.
.mcp_header_value <- function(x) {
  x <- if (is.logical(x)) tolower(as.character(x)) else
    if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
  x <- enc2utf8(x)
  plain <- grepl("^[\\x20-\\x7E]*$", x, perl = TRUE) && !grepl("^\\s|\\s$", x) &&
    !grepl("^=\\?base64\\?.*\\?=$", x)
  if (plain) x else paste0("=?base64?", jsonlite::base64_enc(charToRaw(x)), "?=")
}

# Servers may ask for tool arguments to be copied into headers by marking a
# property with "x-mcp-header". Collect every annotation with its property
# path and whether that path uses only `properties` keys ("statically
# reachable"); annotations anywhere else make the tool invalid.
.mcp_header_annotations <- function(schema, path = character(0), reachable = TRUE) {
  if (!is.list(schema)) return(list())
  out <- list()
  if (!is.null(schema[["x-mcp-header"]])) {
    out <- list(list(path = path, header = schema[["x-mcp-header"]],
                     type = unlist(schema$type), reachable = reachable && length(path) > 0))
  }
  for (key in names(schema)) {
    child <- schema[[key]]
    if (!is.list(child) || key == "x-mcp-header") next
    if (key == "properties") {
      for (p in names(child)) out <- c(out, .mcp_header_annotations(child[[p]], c(path, p), reachable))
    } else if (is.null(names(child))) {
      for (el in child) out <- c(out, .mcp_header_annotations(el, path, FALSE))
    } else {
      out <- c(out, .mcp_header_annotations(child, path, FALSE))
    }
  }
  out
}

# Problems with a tool's x-mcp-header annotations (empty when valid).
.mcp_header_problems <- function(schema) {
  ann <- .mcp_header_annotations(schema)
  if (!length(ann)) return(character(0))
  headers <- vapply(ann, function(a) as.character(a$header)[1], character(1))
  problems <- character(0)
  for (a in ann) {
    h <- as.character(a$header)[1]
    if (!isTRUE(a$reachable)) problems <- c(problems, paste0("'", h, "' is not reachable through properties only"))
    if (is.na(h) || !grepl("^[!#$%&'*+.^_`|~0-9A-Za-z-]+$", h)) problems <- c(problems, paste0("'", h, "' is not a valid header name"))
    if (length(a$type) != 1 || !a$type %in% c("string", "integer", "boolean")) {
      problems <- c(problems, paste0("'", h, "' is on a property that is not a string, integer or boolean"))
    }
  }
  if (anyDuplicated(tolower(headers))) problems <- c(problems, "x-mcp-header names are not unique")
  problems
}

.mcp_param_headers <- function(schema, arguments) {
  out <- list()
  for (a in .mcp_header_annotations(schema)) {
    value <- tryCatch(Reduce(function(x, k) x[[k]], a$path, arguments), error = function(e) NULL)
    if (is.null(value) || !isTRUE(a$reachable)) next
    out[[paste0("Mcp-Param-", a$header)]] <- .mcp_header_value(value)
  }
  out
}

# ---- Responses -----------------------------------------------------------------------

# Server-Sent Events: events are separated by blank lines; each `data:` line
# carries part of a JSON-RPC message. Comment lines (starting with ":") and
# the event/id/retry fields carry nothing this client needs.
.mcp_parse_sse <- function(text) {
  lines <- strsplit(text, "\r\n|\n|\r")[[1]]
  events <- list()
  data <- character(0)
  for (line in c(lines, "")) {
    if (!nzchar(line)) {
      if (length(data)) events <- c(events, list(paste(data, collapse = "\n")))
      data <- character(0)
    } else if (startsWith(line, "data:")) {
      data <- c(data, sub("^data: ?", "", line))
    }
  }
  msgs <- lapply(events, function(d) {
    if (!nzchar(trimws(d))) return(NULL)   # priming events have empty data
    tryCatch(.from_json(d), error = function(e) NULL)
  })
  Filter(Negate(is.null), msgs)
}

# From an HTTP response, find the JSON-RPC reply to request `id`; anything
# else on an SSE stream (progress, log messages) is kept as notifications.
.mcp_parse_response <- function(status, type, text, id) {
  if (identical(as.integer(status), 202L) || !nzchar(trimws(text %||% ""))) {
    return(list(message = NULL, notifications = list()))
  }
  type <- if (is.null(type) || is.na(type)) "" else type
  msgs <- if (grepl("event-stream", type)) .mcp_parse_sse(text) else
    Filter(Negate(is.null), list(tryCatch(.from_json(text), error = function(e) NULL)))
  msgs <- Filter(is.list, msgs)
  is_reply <- vapply(msgs, function(m) !is.null(m$result) || !is.null(m$error), logical(1))
  replies <- msgs[is_reply]
  same_id <- Filter(function(m) !is.null(id) && identical(as.character(m$id %||% ""), as.character(id)), replies)
  # A transport-level rejection may be a JSON-RPC error with no id.
  reply <- if (length(same_id)) same_id[[1]] else if (length(replies)) replies[[1]] else NULL
  list(message = reply, notifications = msgs[!is_reply])
}

.mcp_error <- function(message, code = NA, data = NULL, status = NA) {
  structure(class = c("mcp_error", "error", "condition"),
            list(message = message, call = NULL, code = code, data = data, status = status))
}

# ---- Client and transports ------------------------------------------------------------

# The client keeps mutable state (request counter, negotiated version, session
# id, tool schemas) in an environment, so functions can update it in place.
.mcp_new_client <- function(transport, url, headers = list(), timeout = 60) {
  state <- new.env(parent = emptyenv())
  state$next_id <- 0L
  structure(list(url = url, transport = transport, headers = headers, timeout = timeout,
                 state = state), class = "mcp_client")
}

.mcp_next_id <- function(client) {
  client$state$next_id <- client$state$next_id + 1L
  client$state$next_id
}

# One HTTP POST. MCP needs the raw response (status, the Mcp-Session-Id
# header, SSE text, 202 with an empty body), so the response is captured
# through httr2's is_error hook while .perform() still provides retries,
# backoff and the timeout.
.mcp_http_post <- function(client, msg, headers) {
  seen <- new.env(parent = emptyenv())
  req <- .request(client$url, body = msg, auth = "none",
                  headers = c(headers, client$headers), timeout = client$timeout)
  req <- httr2::req_error(req, is_error = function(resp) {
    seen$resp <- resp
    FALSE
  })
  res <- tryCatch(.perform(req, cache = FALSE), error = function(e) e)
  resp <- seen$resp
  if (is.null(resp)) {
    .stop("Could not reach the MCP server at ", client$url, ": ",
          if (inherits(res, "error")) conditionMessage(res) else "no response")
  }
  list(status = httr2::resp_status(resp), type = httr2::resp_content_type(resp),
       session_id = httr2::resp_header(resp, "mcp-session-id"),
       text = if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else "")
}

# One stdio exchange: write a line, read lines until the reply with the request's id.
.mcp_stdio_exchange <- function(client, msg, timeout) {
  proc <- client$state$process
  if (!proc$is_alive()) .stop("The MCP server process has exited.", .mcp_stderr_tail(client))
  # jsonlite escapes newlines inside strings, so one message is one line.
  proc$write_input(paste0(as.character(.to_json(msg)), "\n"))
  if (is.null(msg$id)) return(list(message = NULL, notifications = list()))
  deadline <- Sys.time() + timeout
  notes <- list()
  repeat {
    left <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (left <= 0) stop(.mcp_error(paste0("No reply to ", msg$method, " within ", timeout, " seconds.")))
    proc$poll_io(as.integer(min(left, 1) * 1000))
    lines <- proc$read_output_lines()
    for (l in lines) {
      m <- tryCatch(.from_json(l), error = function(e) NULL)
      if (!is.list(m)) next
      if ((!is.null(m$result) || !is.null(m$error)) &&
          identical(as.character(m$id %||% ""), as.character(msg$id))) {
        return(list(message = m, notifications = notes))
      }
      notes <- c(notes, list(m))
    }
    if (!length(lines) && !proc$is_alive()) {
      stop(.mcp_error(paste0("The MCP server process exited before replying.", .mcp_stderr_tail(client))))
    }
  }
}

.mcp_stderr_tail <- function(client) {
  f <- client$state$stderr_file
  if (is.null(f) || !file.exists(f)) return("")
  x <- utils::tail(read_text(f, encoding = "UTF-8"), 5)
  if (length(x)) paste0("\nServer messages (stderr):\n", paste(x, collapse = "\n")) else ""
}

# Send one message and return the result, or signal an `mcp_error`.
.mcp_request <- function(client, method, params = NULL, notification = FALSE,
                         initialize = FALSE, timeout = client$timeout) {
  version <- client$state$version
  id <- if (notification) NULL else .mcp_next_id(client)
  msg <- .mcp_message(method, params, id, version)
  if (client$transport == "stdio") {
    out <- .mcp_stdio_exchange(client, msg, timeout)
    status <- NA
  } else {
    schema <- if (identical(method, "tools/call")) client$state$tools[[params$name]] else NULL
    http <- .mcp_http_post(client, msg, .mcp_headers(msg, version, client$state$session_id,
                                                      schema, initialize))
    out <- .mcp_parse_response(http$status, http$type, http$text, id)
    out$session_id <- http$session_id
    status <- http$status
    if (is.null(out$message) && status >= 400) {
      stop(.mcp_error(paste0("HTTP ", status, " from the MCP server",
                             if (nzchar(http$text)) paste0(": ", .shorten(http$text, 200)) else ""),
                      status = status))
    }
  }
  if (notification) return(out)
  m <- out$message
  if (is.null(m)) stop(.mcp_error(paste("The MCP server sent no reply to", method), status = status))
  if (!is.null(m$error)) {
    stop(.mcp_error(paste0("MCP error ", m$error$code %||% "", ": ", m$error$message %||% ""),
                    code = m$error$code %||% NA, data = m$error$data, status = status))
  }
  # Results from servers before 2026-07-28 have no resultType: they are complete.
  if (identical(m$result$resultType, "input_required")) {
    stop(.mcp_error(paste(
      "The server needs extra input (resultType 'input_required': elicitation, sampling",
      "or roots), which this minimal client does not support."), status = status))
  }
  out$result <- m$result
  out
}

.mcp_rpc <- function(client, method, params = NULL, ...) .mcp_request(client, method, params, ...)$result

# ---- Connecting -----------------------------------------------------------------------------

#' Connect to an MCP server
#'
#' The Model Context Protocol (MCP) lets any client use the tools of any
#' server. `mcp_connect()` connects over Streamable HTTP; [mcp_stdio()] starts
#' a local server process. With `protocol = "auto"`, the client first sends a
#' modern (2026-07-28, stateless) `server/discover` request; if the server
#' does not answer as a modern server, it falls back to the 2025-11-25
#' `initialize` handshake and keeps the session id the server returns.
#'
#' Public servers to try: DeepWiki, `https://mcp.deepwiki.com/mcp` (no
#' authentication; tools `read_wiki_structure`, `read_wiki_contents`,
#' `ask_question` about public GitHub repositories; see
#' <https://docs.devin.ai/work-with-devin/deepwiki-mcp.md>).
#'
#' Security: an MCP server's tool descriptions go into your model's context
#' and its outputs are untrusted data. Use servers you trust, read their tool
#' descriptions with [mcp_list_tools()] before giving them to an agent, and
#' keep tokens out of scripts (`headers = list(Authorization =
#' paste("Bearer", Sys.getenv("MY_TOKEN")))`).
#'
#' To serve R tools to other MCP clients, see `mcptools::mcp_server()`; this
#' package does not include a server.
#'
#' @param url The server's MCP endpoint.
#' @param headers Optional named list of extra HTTP headers, for example an
#'   authorization header.
#' @param protocol `"auto"`, or force `"2026-07-28"` or `"2025-11-25"`.
#' @param timeout Seconds to wait for each reply.
#' @return An `mcp_client` object: `url`, `transport`, and in `$state` the
#'   negotiated `version`, `session_id` (legacy servers only),
#'   `server_info`, `capabilities` and `instructions`.
#' @examples
#' \dontrun{
#' dw <- mcp_connect("https://mcp.deepwiki.com/mcp")
#' dw
#' mcp_list_tools(dw)
#' }
#' @export
mcp_connect <- function(url, headers = NULL, protocol = c("auto", "2026-07-28", "2025-11-25"),
                        timeout = 60) {
  protocol <- match.arg(protocol)
  client <- .mcp_new_client("http", url = url, headers = headers %||% list(), timeout = timeout)
  .mcp_handshake(client, protocol)
}

#' Start a local MCP server and connect to it over stdio (optional)
#'
#' Many MCP servers are local programs (often started with `npx` or `uvx`).
#' `mcp_stdio()` starts one as a background process with the processx package
#' and exchanges newline-delimited JSON-RPC messages through its standard
#' input and output. It returns the same kind of client as [mcp_connect()].
#' Only start servers you trust: they run with your user's permissions. Stop
#' the process with [mcp_close()].
#'
#' The client first sends a modern `server/discover` probe; if the server
#' answers with any error that is not a recognized modern error, or does not
#' answer within `min(timeout, 30)` seconds, it falls back to `initialize`.
#'
#' @param command The program to run, for example `"npx"`.
#' @param args Character vector of arguments.
#' @param env Environment variables for the server, as a named character
#'   vector (`NULL` inherits the R session's environment).
#' @inheritParams mcp_connect
#' @return An `mcp_client` object.
#' @examples
#' \dontrun{
#' fs <- mcp_stdio("npx", c("-y", "@modelcontextprotocol/server-filesystem", tempdir()))
#' mcp_list_tools(fs)
#' mcp_close(fs)
#' }
#' @export
mcp_stdio <- function(command, args = character(0), env = NULL,
                      protocol = c("auto", "2026-07-28", "2025-11-25"), timeout = 60) {
  protocol <- match.arg(protocol)
  if (!requireNamespace("processx", quietly = TRUE)) {
    .stop("mcp_stdio() needs the processx package: install.packages(\"processx\").")
  }
  client <- .mcp_new_client("stdio", url = paste(c(command, args), collapse = " "), timeout = timeout)
  # stderr goes to a file: an unread pipe could fill up and block the server.
  client$state$stderr_file <- tempfile("mcp-stderr-", fileext = ".log")
  client$state$process <- processx::process$new(
    command, args, env = env, stdin = "|", stdout = "|",
    stderr = client$state$stderr_file, cleanup = TRUE)
  .mcp_handshake(client, protocol)
}

# Decide the protocol era, following the specification's backward
# compatibility rules: a modern reply or a recognized modern error means a
# modern server; anything else means a legacy server.
.mcp_handshake <- function(client, protocol) {
  if (protocol == .mcp_legacy) return(.mcp_initialize(client))
  client$state$version <- .mcp_modern
  probe_timeout <- if (client$transport == "stdio") min(client$timeout, 30) else client$timeout
  res <- tryCatch(.mcp_rpc(client, "server/discover", timeout = probe_timeout),
                  mcp_error = function(e) e)
  if (!inherits(res, "mcp_error") && !is.null(res$supportedVersions)) {
    supported <- unlist(res$supportedVersions)
    client$state$supported <- supported
    if (.mcp_modern %in% supported) {
      client$state$server_info <- res[["_meta"]][["io.modelcontextprotocol/serverInfo"]]
      client$state$capabilities <- res$capabilities
      client$state$instructions <- res$instructions
      return(client)
    }
    if (protocol == "auto" && any(.mcp_legacy_ok %in% supported)) return(.mcp_initialize(client))
    .stop("The server supports MCP versions ", paste(supported, collapse = ", "),
          "; this client speaks ", .mcp_modern, " and ", .mcp_legacy, ".")
  }
  if (inherits(res, "mcp_error") && isTRUE(res$code %in% .mcp_modern_errors)) {
    supported <- unlist(res$data$supported)
    if (isTRUE(res$code == -32022) && protocol == "auto" && any(.mcp_legacy_ok %in% supported)) {
      return(.mcp_initialize(client))
    }
    .stop("The server speaks modern MCP but rejected the request: ", conditionMessage(res),
          if (length(supported)) paste0(" (supported versions: ", paste(supported, collapse = ", "), ")"))
  }
  why <- if (inherits(res, "mcp_error")) conditionMessage(res) else "no supportedVersions in the reply"
  if (protocol == .mcp_modern) {
    .stop("The server did not answer server/discover as an MCP ", .mcp_modern, " server (", why,
          "). Try protocol = \"auto\".")
  }
  .msg("Not an MCP ", .mcp_modern, " server (", .shorten(why, 120), "); using ", .mcp_legacy,
       " with initialize.")
  .mcp_initialize(client)
}

.mcp_initialize <- function(client) {
  client$state$version <- .mcp_legacy
  client$state$session_id <- NULL
  params <- list(protocolVersion = .mcp_legacy, capabilities = .empty_object(),
                 clientInfo = .mcp_client_info())
  out <- .mcp_request(client, "initialize", params, initialize = TRUE)
  res <- out$result
  if (!isTRUE(res$protocolVersion %in% .mcp_legacy_ok)) {
    .stop("The server answered initialize with MCP version ", res$protocolVersion %||% "(none)",
          ", which this client does not support.")
  }
  client$state$version <- res$protocolVersion
  client$state$session_id <- out$session_id
  client$state$server_info <- res$serverInfo
  client$state$capabilities <- res$capabilities
  client$state$instructions <- res$instructions
  sent <- tryCatch(.mcp_request(client, "notifications/initialized", notification = TRUE),
                   error = function(e) e)
  if (inherits(sent, "error")) .warn("The server did not accept notifications/initialized: ", conditionMessage(sent))
  client
}

#' Close an MCP connection
#'
#' Stops a server started with [mcp_stdio()] (closes its input, waits two
#' seconds, then kills it). For a legacy HTTP server with a session, sends
#' the HTTP DELETE that ends the session. Modern HTTP servers keep no
#' session, so there is nothing to close.
#'
#' @param client An `mcp_client`.
#' @return `NULL`, invisibly.
#' @examples
#' \dontrun{
#' mcp_close(fs)
#' }
#' @export
mcp_close <- function(client) {
  if (client$transport == "stdio") {
    proc <- client$state$process
    if (!is.null(proc) && proc$is_alive()) {
      close(proc$get_input_connection())
      proc$wait(2000)
      if (proc$is_alive()) proc$kill()
    }
  } else if (!is.null(client$state$session_id)) {
    req <- .request(client$url, method = "DELETE", auth = "none",
                    headers = c(list(`Mcp-Session-Id` = client$state$session_id,
                                     `MCP-Protocol-Version` = client$state$version), client$headers))
    # Best effort, once: a server may refuse (405) or have ended the session already.
    req <- httr2::req_retry(httr2::req_error(req, is_error = function(resp) FALSE), max_tries = 1)
    try(.perform(req, cache = FALSE), silent = TRUE)
    client$state$session_id <- NULL
  }
  invisible(NULL)
}

#' @export
print.mcp_client <- function(x, ...) {
  s <- x$state
  info <- s$server_info
  cat("<mcp_client>", x$transport, "|", x$url, "\n")
  cat("Server:", if (length(info)) paste(info$name %||% "?", info$version %||% "") else "(no info)", "\n")
  cat("Protocol:", s$version %||% "(not connected)",
      if (!is.null(s$session_id)) "| session" else "", "\n")
  caps <- names(s$capabilities)
  if (length(caps)) cat("Capabilities:", paste(caps, collapse = ", "), "\n")
  if (length(x$headers)) cat("Extra headers:", paste(names(x$headers), collapse = ", "), "(values hidden)\n")
  if (nzchar(s$instructions %||% "")) cat("Server instructions (untrusted):", .shorten(s$instructions, 200), "\n")
  invisible(x)
}

# ---- Tools -----------------------------------------------------------------------------------

#' List the tools of an MCP server
#'
#' @param client An `mcp_client` from [mcp_connect()] or [mcp_stdio()].
#' @return A data frame with `name`, `title`, `description` and
#'   `input_schema` (the JSON Schema as text). The raw tool definitions are
#'   in `attr(x, "tools")`. On HTTP, tools whose `x-mcp-header` annotations
#'   break the specification's rules are left out with a warning, as the
#'   specification requires.
#' @examples
#' \dontrun{
#' dw <- mcp_connect("https://mcp.deepwiki.com/mcp")
#' tools <- mcp_list_tools(dw)
#' tools[, c("name", "description")]
#' cat(tools$input_schema[1])
#' }
#' @export
mcp_list_tools <- function(client) {
  tools <- list()
  cursor <- NULL
  for (page in 1:100) {   # pagination, with a guard against a server that never stops
    res <- .mcp_rpc(client, "tools/list", if (!is.null(cursor)) list(cursor = cursor))
    tools <- c(tools, res$tools)
    cursor <- res$nextCursor
    if (is.null(cursor) || !nzchar(cursor)) break
  }
  if (client$transport == "http") {
    keep <- vapply(tools, function(t) {
      p <- .mcp_header_problems(t$inputSchema)
      if (length(p)) .warn("MCP tool '", t$name, "' left out: ", paste(p, collapse = "; "))
      !length(p)
    }, logical(1))
    tools <- tools[keep]
  }
  names(tools) <- vapply(tools, function(t) as.character(t$name), character(1))
  client$state$tools <- lapply(tools, `[[`, "inputSchema")
  chr <- function(field) vapply(tools, function(t) as.character(t[[field]] %||% NA_character_)[1], character(1))
  df <- data.frame(name = unname(chr("name")), title = unname(chr("title")),
                   description = unname(chr("description")),
                   input_schema = unname(vapply(tools, function(t) as.character(.to_json(t$inputSchema %||% .empty_object())), character(1))),
                   stringsAsFactors = FALSE)
  attr(df, "tools") <- unname(tools)
  df
}

#' Call a tool on an MCP server
#'
#' A protocol error (unknown tool, malformed request) stops with an error of
#' class `mcp_error`. An error inside the tool is a normal result with
#' `is_error = TRUE`, whose text explains what went wrong.
#'
#' Arrays: a length-one vector is sent as a single value, not an array. Wrap
#' array arguments in `list()` or `I()`, for example `list(ids = I("W123"))`.
#'
#' @param client An `mcp_client`.
#' @param tool The tool name, as listed by [mcp_list_tools()].
#' @param arguments A named list of arguments that follows the tool's
#'   `input_schema`.
#' @return A list with `text` (all text content joined; other content types
#'   are named in brackets), `content` (the raw content blocks), `is_error`
#'   and `structured` (the `structuredContent`, or `NULL`).
#' @examples
#' \dontrun{
#' dw <- mcp_connect("https://mcp.deepwiki.com/mcp")
#' tools <- mcp_list_tools(dw)
#' cat(tools$input_schema[tools$name == "ask_question"])
#' # Use the argument names shown in the schema above:
#' r <- mcp_call(dw, "ask_question",
#'               list(repoName = "hfgolino/EGAnet",
#'                    question = "Which function estimates the number of dimensions?"))
#' cat(r$text)
#' }
#' @export
mcp_call <- function(client, tool, arguments = list()) {
  # Modern HTTP servers may need some arguments copied into headers
  # (x-mcp-header), which requires the tool's schema: fetch it once.
  if (client$transport == "http" && .mcp_is_modern(client$state$version) &&
      is.null(client$state$tools)) {
    try(mcp_list_tools(client), silent = TRUE)
  }
  params <- list(name = tool, arguments = if (length(arguments)) arguments else .empty_object())
  res <- .mcp_rpc(client, "tools/call", params)
  blocks <- res$content %||% list()
  text <- vapply(blocks, .mcp_block_text, character(1))
  text <- paste(text[nzchar(text)], collapse = "\n")
  if (!nzchar(text) && !is.null(res$structuredContent)) text <- as.character(.to_json(res$structuredContent))
  list(text = text, content = blocks, is_error = isTRUE(res$isError),
       structured = res$structuredContent)
}

.mcp_block_text <- function(b) {
  switch(b$type %||% "",
    text = as.character(b$text %||% ""),
    resource = as.character(b$resource$text %||% paste0("[resource ", b$resource$uri %||% "", "]")),
    resource_link = paste0("[resource link: ", b$name %||% "", " ", b$uri %||% "", "]"),
    image = , audio = paste0("[", b$type, " content (", b$mimeType %||% "unknown type", ") not shown]"),
    paste0("[", b$type %||% "unknown", " content]"))
}

#' Use the tools of an MCP server as agent tools
#'
#' Each server tool becomes an `agent_tool` whose JSON Schema is the server's
#' `inputSchema`, so the model sees exactly what the server documents. The
#' model's arguments get a light check in R (required fields, unknown fields
#' when the schema forbids them, top-level types and enums) and the server
#' validates them fully. Outputs are marked as untrusted data. A tool error
#' reported by the server is returned to the model as an error observation.
#'
#' Names: characters other than letters, digits, `_` and `-` (MCP allows
#' `.`) become `_`; use `prefix` when two servers have tools with the same
#' name.
#'
#' @param client An `mcp_client`.
#' @param tools Optional character vector of tool names to keep (keep the
#'   menu short: every tool description costs tokens on every call).
#' @param prefix Optional prefix for the tool names, for example `"deepwiki_"`.
#' @return A list of `agent_tool` objects for [agent()].
#' @examples
#' \dontrun{
#' dw <- mcp_connect("https://mcp.deepwiki.com/mcp")
#' helper <- agent("repo_helper", "Answer questions about R packages on GitHub.",
#'                 model = "groq:openai/gpt-oss-120b",
#'                 tools = mcp_as_tools(dw, tools = "ask_question"))
#' run(helper, "In hfgolino/EGAnet, what does bootEGA() do?")
#' }
#' @export
mcp_as_tools <- function(client, tools = NULL, prefix = NULL) {
  df <- mcp_list_tools(client)
  defs <- attr(df, "tools")
  if (!is.null(tools)) {
    missing <- setdiff(tools, df$name)
    if (length(missing)) {
      .stop("Not tools of this server: ", paste(missing, collapse = ", "),
            ". Available: ", paste(df$name, collapse = ", "))
    }
    defs <- defs[df$name %in% tools]
  }
  lapply(defs, .mcp_agent_tool, client = client, prefix = prefix %||% "")
}

# Build the agent_tool directly (not with define_tool()), because the schema
# comes from the server and the function takes whatever arguments it lists.
.mcp_agent_tool <- function(def, client, prefix = "") {
  original <- as.character(def$name)
  schema <- .mcp_model_schema(def$inputSchema)
  fn <- function(...) {
    r <- mcp_call(client, original, list(...))
    if (r$is_error) stop("the MCP server reported an error: ", r$text, call. = FALSE)
    r$text
  }
  structure(list(
    name = .safe_tool_name(paste0(prefix, original)),
    description = as.character(def$description %||% def$title %||% original),
    fn = fn, args = .mcp_display_args(schema), schema = schema,
    # Server schemas rarely meet the rules of strict mode (every field required).
    strict = FALSE, approval = FALSE, timeout = client$timeout + 10,
    max_output_tokens = 2000, cost_per_call = 0, untrusted = TRUE,
    validate = function(arguments) .mcp_validate(arguments, schema),
    mcp_server = client$url, mcp_tool = original
  ), class = "agent_tool")
}

# The schema as sent to the model: an object with properties. "$schema" is
# dropped because some providers reject it.
.mcp_model_schema <- function(schema) {
  schema <- schema %||% list()
  schema[["$schema"]] <- NULL
  if (is.null(schema$type)) schema$type <- "object"
  if (is.null(schema$properties)) schema$properties <- .empty_object()
  schema
}

# Argument descriptions for print.agent_tool (display only; validation uses
# the schema itself).
.mcp_display_args <- function(schema) {
  props <- schema$properties %||% list()
  req <- unlist(schema$required)
  stats::setNames(lapply(names(props), function(nm) {
    p <- props[[nm]]
    .arg(paste(unlist(p$type %||% "any"), collapse = "|"), as.character(p$description %||% ""),
         nm %in% req)
  }), names(props))
}

# A light check of the model's arguments against a server's inputSchema.
# Returns list(arguments, problems) as call_tool() expects of `validate`.
.mcp_validate <- function(arguments, schema) {
  arguments <- arguments %||% list()
  if (length(arguments) && is.null(names(arguments))) {
    return(list(arguments = arguments, problems = "the arguments must be an object with named fields"))
  }
  problems <- character(0)
  props <- schema$properties %||% list()
  missing <- setdiff(unlist(schema$required), names(arguments))
  if (length(missing)) problems <- c(problems, paste0("'", missing, "' is required"))
  if (isFALSE(schema$additionalProperties)) {
    extra <- setdiff(names(arguments), names(props))
    if (length(extra)) problems <- c(problems, paste("unknown argument(s):", paste(extra, collapse = ", ")))
  }
  for (nm in intersect(names(arguments), names(props))) {
    p <- props[[nm]]
    v <- arguments[[nm]]
    types <- unlist(p$type)
    if (length(types) && !.mcp_type_ok(v, types)) {
      problems <- c(problems, paste0("'", nm, "' must be ", paste(types, collapse = " or ")))
    } else if (!is.null(p$enum) && !is.null(v) && !(length(v) == 1 && v %in% unlist(p$enum))) {
      problems <- c(problems, paste0("'", nm, "' must be one of: ", paste(unlist(p$enum), collapse = ", ")))
    }
    # Keep arrays as arrays in JSON (a length-one vector would become a scalar).
    if ("array" %in% types && is.atomic(v) && !is.null(v)) arguments[[nm]] <- as.list(v)
  }
  list(arguments = arguments, problems = problems)
}

.mcp_type_ok <- function(v, types) {
  scalar <- length(v) == 1 && !is.list(v)
  any(vapply(types, function(type) switch(type,
    null = is.null(v),
    string = scalar && is.character(v),
    integer = scalar && is.numeric(v) && !is.na(v) && v == round(v),
    number = scalar && is.numeric(v) && !is.na(v),
    boolean = scalar && is.logical(v) && !is.na(v),
    array = !is.null(v) && is.null(names(v)) && (is.list(v) || is.atomic(v)),
    object = is.list(v) && (length(v) == 0 || !is.null(names(v))),
    TRUE), logical(1)))
}
