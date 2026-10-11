# Offline tests of the MCP client: message and header construction, SSE
# parsing, tool conversion and validation. JSON fragments follow the examples
# in the MCP specification (accessed 2026-10-10):
#   https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http.md
#   https://modelcontextprotocol.io/specification/2026-07-28/server/tools.md
#   https://modelcontextprotocol.io/specification/2026-07-28/server/discover.md
#   https://modelcontextprotocol.io/specification/2025-11-25/basic/lifecycle

test_that("modern requests carry the required _meta fields", {
  msg <- .mcp_message("tools/call", list(name = "get_weather", arguments = list(location = "Seattle, WA")),
                      id = 1L, version = "2026-07-28")
  expect_equal(msg$jsonrpc, "2.0")
  expect_equal(msg$id, 1L)
  meta <- msg$params[["_meta"]]
  expect_equal(meta[["io.modelcontextprotocol/protocolVersion"]], "2026-07-28")
  expect_equal(meta[["io.modelcontextprotocol/clientInfo"]]$name, "researchAgentR")
  expect_true("io.modelcontextprotocol/clientCapabilities" %in% names(meta))
  json <- as.character(.to_json(msg))
  # clientCapabilities must be an empty object, not an empty array
  expect_match(json, "\"io.modelcontextprotocol/clientCapabilities\":{}", fixed = TRUE)
  expect_match(json, "\"arguments\":{\"location\":\"Seattle, WA\"}", fixed = TRUE)
})

test_that("legacy messages have no _meta and notifications have no id", {
  init <- .mcp_message("initialize", list(protocolVersion = "2025-11-25",
                                          capabilities = .empty_object(),
                                          clientInfo = .mcp_client_info()),
                       id = 1L, version = "2025-11-25")
  expect_null(init$params[["_meta"]])
  expect_equal(init$params$protocolVersion, "2025-11-25")
  note <- .mcp_message("notifications/initialized", version = "2025-11-25")
  expect_null(note$id)
  expect_null(note$params)
  expect_equal(as.character(.to_json(note)), "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
})

test_that("modern HTTP headers mirror the body", {
  msg <- .mcp_message("tools/call", list(name = "get_weather", arguments = .empty_object()), id = 1L)
  h <- .mcp_headers(msg, "2026-07-28")
  expect_equal(h$Accept, "application/json, text/event-stream")
  expect_equal(h[["MCP-Protocol-Version"]], "2026-07-28")
  expect_equal(h[["MCP-Protocol-Version"]], msg$params[["_meta"]][["io.modelcontextprotocol/protocolVersion"]])
  expect_equal(h[["Mcp-Method"]], "tools/call")
  expect_equal(h[["Mcp-Name"]], "get_weather")
  expect_null(h[["Mcp-Session-Id"]])

  list_msg <- .mcp_message("tools/list", NULL, id = 2L)
  h2 <- .mcp_headers(list_msg, "2026-07-28")
  expect_equal(h2[["Mcp-Method"]], "tools/list")
  expect_null(h2[["Mcp-Name"]])

  read_msg <- .mcp_message("resources/read", list(uri = "file:///projects/myapp/config.json"), id = 3L)
  expect_equal(.mcp_headers(read_msg, "2026-07-28")[["Mcp-Name"]], "file:///projects/myapp/config.json")
})

test_that("legacy HTTP headers carry the session id, and initialize has no version header", {
  init <- .mcp_message("initialize", list(protocolVersion = "2025-11-25"), id = 1L, version = "2025-11-25")
  h0 <- .mcp_headers(init, "2025-11-25", initialize = TRUE)
  expect_null(h0[["MCP-Protocol-Version"]])
  expect_null(h0[["Mcp-Method"]])
  msg <- .mcp_message("tools/list", NULL, id = 2L, version = "2025-11-25")
  h <- .mcp_headers(msg, "2025-11-25", session_id = "1868a90c")
  expect_equal(h[["MCP-Protocol-Version"]], "2025-11-25")
  expect_equal(h[["Mcp-Session-Id"]], "1868a90c")
  expect_null(h[["Mcp-Method"]])
})

test_that("header values use the Base64 sentinel when needed (spec examples)", {
  expect_equal(.mcp_header_value("us-west1"), "us-west1")
  expect_equal(.mcp_header_value("Hello, \u4e16\u754c"), "=?base64?SGVsbG8sIOS4lueVjA==?=")
  expect_equal(.mcp_header_value(" padded "), "=?base64?IHBhZGRlZCA=?=")
  expect_equal(.mcp_header_value("line1\nline2"), "=?base64?bGluZTEKbGluZTI=?=")
  expect_equal(.mcp_header_value("=?base64?literal?="), "=?base64?PT9iYXNlNjQ/bGl0ZXJhbD89?=")
  expect_equal(.mcp_header_value(TRUE), "true")
  expect_equal(.mcp_header_value(42L), "42")
})

test_that("x-mcp-header annotations become Mcp-Param headers and invalid ones are found", {
  schema <- .from_json('{"type":"object","properties":{
    "region":{"type":"string","description":"The region","x-mcp-header":"Region"},
    "query":{"type":"string","description":"The SQL query"}},
    "required":["region","query"]}')
  expect_length(.mcp_header_problems(schema), 0)
  msg <- .mcp_message("tools/call", list(name = "execute_sql",
                                         arguments = list(region = "us-west1", query = "SELECT 1")), id = 1L)
  h <- .mcp_headers(msg, "2026-07-28", schema = schema)
  expect_equal(h[["Mcp-Param-Region"]], "us-west1")
  # absent value: header omitted
  msg2 <- .mcp_message("tools/call", list(name = "execute_sql", arguments = list(query = "SELECT 1")), id = 2L)
  expect_null(.mcp_headers(msg2, "2026-07-28", schema = schema)[["Mcp-Param-Region"]])

  in_items <- .from_json('{"type":"object","properties":{"ids":{"type":"array",
    "items":{"type":"string","x-mcp-header":"Id"}}}}')
  expect_match(.mcp_header_problems(in_items), "not reachable")
  on_number <- .from_json('{"type":"object","properties":{"x":{"type":"number","x-mcp-header":"X"}}}')
  expect_match(.mcp_header_problems(on_number), "not a string, integer or boolean")
  dup <- .from_json('{"type":"object","properties":{"a":{"type":"string","x-mcp-header":"Name"},
    "b":{"type":"string","x-mcp-header":"name"}}}')
  expect_true(any(grepl("not unique", .mcp_header_problems(dup))))
})

test_that("SSE responses are parsed, skipping comments and priming events", {
  sse <- paste(
    ": keep-alive comment",
    "",
    "id: 0",
    "data:",
    "",
    "event: message",
    'data: {"jsonrpc":"2.0","method":"notifications/progress","params":{"progress":1}}',
    "",
    'data: {"jsonrpc":"2.0","id":7,',
    'data: "result":{"resultType":"complete","content":[{"type":"text","text":"hi"}],"isError":false}}',
    "",
    sep = "\r\n")
  msgs <- .mcp_parse_sse(sse)
  expect_length(msgs, 2)
  expect_equal(msgs[[1]]$method, "notifications/progress")
  expect_equal(msgs[[2]]$id, 7L)
  parsed <- .mcp_parse_response(200, "text/event-stream", sse, id = 7L)
  expect_equal(parsed$message$result$content[[1]]$text, "hi")
  expect_length(parsed$notifications, 1)
})

test_that("JSON responses, 202 and errors are told apart", {
  ok <- '{"jsonrpc":"2.0","id":2,"result":{"resultType":"complete","tools":[]}}'
  expect_equal(.mcp_parse_response(200, "application/json", ok, id = 2L)$message$id, 2L)
  expect_null(.mcp_parse_response(202, NA, "", id = NULL)$message)
  err <- '{"jsonrpc":"2.0","id":1,"error":{"code":-32020,"message":"Header mismatch"}}'
  m <- .mcp_parse_response(400, "application/json", err, id = 1L)$message
  expect_equal(m$error$code, -32020L)
  expect_true(m$error$code %in% .mcp_modern_errors)
  expect_null(.mcp_parse_response(400, "text/html", "<html>Bad Request</html>", id = 1L)$message)
})

test_that("content blocks are turned into text", {
  blocks <- .from_json('[{"type":"text","text":"First"},
    {"type":"image","data":"AAAA","mimeType":"image/png"},
    {"type":"resource_link","uri":"file:///project/src/main.rs","name":"main.rs"},
    {"type":"resource","resource":{"uri":"file:///a.txt","mimeType":"text/plain","text":"Embedded"}}]')
  txt <- vapply(blocks, .mcp_block_text, character(1))
  expect_equal(txt[1], "First")
  expect_match(txt[2], "image content \\(image/png\\) not shown")
  expect_match(txt[3], "file:///project/src/main.rs", fixed = TRUE)
  expect_equal(txt[4], "Embedded")
})

test_that("an MCP tool becomes an agent_tool with the server's schema", {
  def <- .from_json('{"name":"get_weather","title":"Weather Information Provider",
    "description":"Get current weather information for a location",
    "inputSchema":{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object",
      "properties":{"location":{"type":"string","description":"City name or zip code"},
                    "units":{"type":"string","enum":["metric","imperial"]},
                    "days":{"type":"integer"},
                    "tags":{"type":"array","items":{"type":"string"}}},
      "required":["location"],"additionalProperties":false}}')
  client <- .mcp_new_client("http", "https://example.invalid/mcp")
  tool <- .mcp_agent_tool(def, client, prefix = "wx.")
  expect_s3_class(tool, "agent_tool")
  expect_equal(tool$name, "wx_get_weather")
  expect_equal(tool$mcp_tool, "get_weather")
  expect_true(tool$untrusted)
  expect_false(tool$strict)
  expect_null(tool$schema[["$schema"]])
  expect_equal(tool$schema$properties$location$type, "string")
  expect_match(tool_schema(tool), "City name or zip code")
  expect_output(print(tool), "location \\(string\\)")

  v <- tool$validate(list(location = "Charlottesville", units = "metric", days = 3, tags = "rain"))
  expect_length(v$problems, 0)
  expect_type(v$arguments$tags, "list")   # stays a JSON array
  expect_match(as.character(.to_json(v$arguments)), "\"tags\":[\"rain\"]", fixed = TRUE)
  expect_match(tool$validate(list())$problems, "'location' is required")
  expect_match(tool$validate(list(location = 1))$problems, "'location' must be string")
  expect_match(tool$validate(list(location = "x", units = "kelvin"))$problems, "must be one of")
  expect_match(tool$validate(list(location = "x", days = 2.5))$problems, "must be integer")
  expect_match(tool$validate(list(location = "x", wind = TRUE))$problems, "unknown argument")

  # Through call_tool(): invalid arguments become an error observation and no
  # request is sent (the server URL is not reachable).
  res <- call_tool("wx_get_weather", list(tool), arguments = list(units = "metric"))
  expect_true(res$is_error)
  expect_match(res$output, "'location' is required")
})

test_that("a tool with no parameters gets an object schema", {
  s <- .mcp_model_schema(.from_json('{"type":"object","additionalProperties":false}'))
  expect_match(as.character(.to_json(s)), "\"properties\":{}", fixed = TRUE)
  expect_length(.mcp_validate(list(), s)$problems, 0)
  expect_match(.mcp_validate(list(x = 1), s)$problems, "unknown argument")
})

# A local stdio server written for this test (base R + jsonlite). It is a test
# fixture that speaks a small subset of MCP, used to test the stdio framing
# and the modern/legacy handshake offline; it is not a real MCP server.
.write_test_server <- function() {
  f <- tempfile(fileext = ".R")
  writeLines(c(
    '`%||%` <- function(x, y) if (is.null(x)) y else x',
    'mode <- commandArgs(TRUE)[1]',
    'con <- file("stdin", open = "r")',
    'send <- function(x) { cat(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), "\\n", sep = ""); flush(stdout()) }',
    'empty <- setNames(list(), character(0))',
    'ready <- mode == "modern"',
    'repeat {',
    '  line <- readLines(con, n = 1)',
    '  if (!length(line)) break',
    '  msg <- jsonlite::fromJSON(line, simplifyVector = FALSE)',
    '  if (identical(msg$method, "notifications/initialized")) { ready <- TRUE; next }',
    '  if (is.null(msg$id)) next',
    '  reply <- function(result) send(list(jsonrpc = "2.0", id = msg$id, result = result))',
    '  err <- function(code, text) send(list(jsonrpc = "2.0", id = msg$id, error = list(code = code, message = text)))',
    '  m <- msg$method',
    '  if (mode == "legacy" && m == "server/discover") { err(-32601, "Method not found"); next }',
    '  if (mode == "legacy" && m == "initialize") { reply(list(protocolVersion = "2025-11-25",',
    '      capabilities = list(tools = empty), serverInfo = list(name = "test-legacy", version = "0.0.1"))); next }',
    '  if (!ready) { err(-32002, "Server not initialized"); next }',
    '  if (m == "server/discover") {',
    '    reply(list(resultType = "complete", supportedVersions = list("2026-07-28"),',
    '      capabilities = list(tools = empty),',
    '      `_meta` = list(`io.modelcontextprotocol/serverInfo` = list(name = "test-modern", version = "0.0.1"))))',
    '  } else if (m == "tools/list") {',
    '    reply(list(tools = list(list(name = "echo", description = "Echo text back.",',
    '      inputSchema = list(type = "object", properties = list(text = list(type = "string")),',
    '                         required = list("text"))))))',
    '  } else if (m == "tools/call" && !identical(msg$params$name, "echo")) {',
    '    err(-32602, paste("Unknown tool:", msg$params$name))',
    '  } else if (m == "tools/call") {',
    '    v <- msg$params[["_meta"]][["io.modelcontextprotocol/protocolVersion"]] %||% "no _meta"',
    '    if (identical(msg$params$arguments$text, "fail")) {',
    '      reply(list(content = list(list(type = "text", text = "the tool failed")), isError = TRUE))',
    '    } else {',
    '      reply(list(content = list(list(type = "text", text = paste0("echo: ", msg$params$arguments$text, " [", v, "]"))),',
    '                 isError = FALSE))',
    '    }',
    '  } else err(-32601, "Method not found")',
    '}'), f)
  f
}

test_that("stdio transport works with a modern and a legacy local server", {
  skip_on_cran()
  skip_if_not_installed("processx")
  server <- .write_test_server()
  rscript <- file.path(R.home("bin"), "Rscript")
  old <- options(researchAgentR.verbose = FALSE)
  on.exit(options(old), add = TRUE)

  modern <- mcp_stdio(rscript, c("--vanilla", server, "modern"), timeout = 30)
  on.exit(mcp_close(modern), add = TRUE)
  expect_equal(modern$state$version, "2026-07-28")
  expect_equal(modern$state$server_info$name, "test-modern")
  expect_equal(mcp_list_tools(modern)$name, "echo")
  r <- mcp_call(modern, "echo", list(text = "hello"))
  expect_equal(r$text, "echo: hello [2026-07-28]")
  expect_false(r$is_error)
  expect_true(mcp_call(modern, "echo", list(text = "fail"))$is_error)
  expect_error(mcp_call(modern, "nope"), class = "mcp_error")
  tools <- mcp_as_tools(modern)
  ok <- call_tool("echo", tools, arguments = list(text = "via agent tool"))
  expect_false(ok$is_error)
  expect_match(ok$output, "trust=\"untrusted\"", fixed = TRUE)
  bad <- call_tool("echo", tools, arguments = list(text = "fail"))
  expect_true(bad$is_error)
  expect_match(bad$output, "the tool failed")

  legacy <- mcp_stdio(rscript, c("--vanilla", server, "legacy"), timeout = 30)
  on.exit(mcp_close(legacy), add = TRUE)
  expect_equal(legacy$state$version, "2025-11-25")
  expect_equal(legacy$state$server_info$name, "test-legacy")
  expect_equal(mcp_call(legacy, "echo", list(text = "old"))$text, "echo: old [no _meta]")
  expect_output(print(legacy), "2025-11-25")
})

test_that("live: DeepWiki public MCP server", {
  skip_on_cran()
  skip_if_offline <- function() {
    ok <- tryCatch({
      httr2::req_perform(httr2::req_timeout(httr2::req_method(
        httr2::request("https://mcp.deepwiki.com/mcp"), "HEAD"), 5))
      TRUE
    }, httr2_http = function(e) TRUE, error = function(e) FALSE)
    if (!ok) skip("mcp.deepwiki.com is not reachable")
  }
  skip_if_offline()
  dw <- mcp_connect("https://mcp.deepwiki.com/mcp")
  tools <- mcp_list_tools(dw)
  expect_true(all(c("read_wiki_structure", "read_wiki_contents", "ask_question") %in% tools$name))
  agent_tools <- mcp_as_tools(dw, tools = "read_wiki_structure")
  expect_length(agent_tools, 1)
})
