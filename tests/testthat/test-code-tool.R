# run_r_code() needs no network or model, so all of it is tested here.

test_that("printed output, warnings, messages and the last value are captured", {
  skip_if_not_installed("callr")
  r <- run_r_code("x <- 1:10\nprint(mean(x))\nwarning('careful')\nmessage('hello')\nsum(x)")
  expect_s3_class(r, "r_code_result")
  expect_true(is.na(r$error))
  expect_false(r$timed_out)
  expect_match(r$output, "[1] 5.5", fixed = TRUE)
  expect_match(r$output, "[1] 55", fixed = TRUE)   # visible values print, as at the console
  expect_equal(r$warnings, "careful")
  expect_equal(r$messages, "hello")
  expect_match(r$value, "int 55")
  expect_gt(r$seconds, 0)
})

test_that("errors and syntax errors are captured, not thrown", {
  skip_if_not_installed("callr")
  r <- run_r_code("print('before'); stop('boom')")
  expect_equal(r$error, "boom")
  expect_match(r$output, "before")
  p <- run_r_code("1 +")
  expect_match(p$error, "unexpected end of input")
  m <- run_r_code("1", packages = "aPackageThatDoesNotExist")
  expect_match(m$error, "aPackageThatDoesNotExist")
})

test_that("code that runs too long is stopped", {
  skip_if_not_installed("callr")
  r <- run_r_code("Sys.sleep(20)", timeout = 2)
  expect_true(r$timed_out)
  expect_match(r$error, "did not finish within 2 seconds")
  expect_lt(r$seconds, 15)
})

test_that("web requests fail and keys are hidden when the network is off", {
  skip_if_not_installed("callr")
  withr::local_envvar(RESEARCHAGENTR_TEST_API_KEY = "s3cret")
  r <- run_r_code(paste(
    "cat(Sys.getenv('https_proxy'), '|', Sys.getenv('RESEARCHAGENTR_TEST_API_KEY'), '\\n')",
    "readLines('https://cran.r-project.org', n = 1)", sep = "\n"))
  expect_match(r$output, .blackhole_proxy, fixed = TRUE)
  expect_false(grepl("s3cret", r$output))
  expect_false(is.na(r$error))      # the request failed
})

test_that("files written by the code stay in the working folder", {
  skip_if_not_installed("callr")
  d <- withr::local_tempdir()
  r <- run_r_code("saveRDS(1:3, 'x.rds'); 'done'", dir = d)
  expect_equal(basename(r$files), "x.rds")
  r2 <- run_r_code("readRDS('x.rds')", dir = d)
  expect_match(r2$output, "[1] 1 2 3", fixed = TRUE)
})

test_that("the tool reports output to the model and failures as errors", {
  skip_if_not_installed("callr")
  t <- tool_run_r(timeout = 20)
  expect_equal(t$name, "run_r")
  expect_false(t$untrusted)
  expect_false(t$approval)
  expect_equal(t$schema$required, list("code"))
  expect_true(tool_run_r(approval = TRUE)$approval)
  ok <- call_tool("run_r", list(t), list(code = "x <- 6; x * 7"))
  expect_false(ok$is_error)
  expect_match(ok$output, "[1] 42", fixed = TRUE)
  expect_s3_class(attr(ok$value, "data"), "r_code_result")
  # Calls of one tool share a working folder.
  call_tool("run_r", list(t), list(code = "saveRDS(99, 'keep.rds')"))
  again <- call_tool("run_r", list(t), list(code = "readRDS('keep.rds')"))
  expect_match(again$output, "99")
  bad <- call_tool("run_r", list(t), list(code = "print('partial'); stop('bad input')"))
  expect_true(bad$is_error)
  expect_match(bad$output, "bad input")
  expect_match(bad$output, "partial")
})
