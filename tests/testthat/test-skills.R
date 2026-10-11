# Skill folders are written by these tests into temporary directories.

make_skill <- function(root, folder, frontmatter, body = c("# Title", "", "Do the task step by step.")) {
  d <- file.path(root, folder)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(c("---", frontmatter, "---", "", body), file.path(d, "SKILL.md"))
  d
}

good_desc <- "description: Summarizes methods sections. Use when the user asks for a methods summary."

rule_ok <- function(chk, rule) chk$ok[chk$rule == rule]

test_that("frontmatter parsing handles quotes, maps, block scalars and lists", {
  fm <- .parse_frontmatter(c(
    "name: my-skill",
    "description: \"Does X: and \\\"Y\\\". Use when Z.\"",
    "license: 'MIT'",
    "metadata:",
    "  author: \"Someone\"",
    "  version: \"1.0\"",
    "compatibility: >",
    "  Requires R 4.1",
    "  and internet",
    "allowed-tools: Read Bash"))
  expect_length(fm$errors, 0)
  f <- fm$fields
  expect_equal(f$name, "my-skill")
  expect_equal(f$description, "Does X: and \"Y\". Use when Z.")
  expect_equal(f$license, "MIT")
  expect_equal(f$metadata, list(author = "Someone", version = "1.0"))
  expect_equal(f$compatibility, "Requires R 4.1 and internet")
  expect_equal(f$`allowed-tools`, "Read Bash")
  lit <- .parse_frontmatter(c("description: |", "  line one", "  line two"))$fields
  expect_equal(lit$description, "line one\nline two")
  bad <- .parse_frontmatter(c("name my-skill"))
  expect_length(bad$errors, 1)
})

test_that("the example skill shipped with the package is valid", {
  d <- system.file("extdata", "skills", "summarizing-methods", package = "researchAgentR")
  chk <- validate_skill(d)
  expect_s3_class(chk, "skill_check")
  expect_true(attr(chk, "valid"))
  expect_true(all(chk$ok[chk$level != "review"]))
  expect_output(print(chk), "Valid skill")
})

test_that("validate_skill rejects each rule of the specification", {
  root <- withr::local_tempdir()
  # Uppercase letters.
  chk <- validate_skill(make_skill(root, "Bad-Name", c("name: Bad-Name", good_desc)))
  expect_false(attr(chk, "valid"))
  expect_false(rule_ok(chk, "name uses only a-z, 0-9 and hyphens"))
  # Consecutive hyphens.
  chk <- validate_skill(make_skill(root, "pdf--processing", c("name: pdf--processing", good_desc)))
  expect_false(rule_ok(chk, "name has no consecutive hyphens"))
  # Leading hyphen and a folder that does not match.
  chk <- validate_skill(make_skill(root, "reading-files", c("name: -reading-files", good_desc)))
  expect_false(rule_ok(chk, "name does not start or end with a hyphen"))
  expect_false(rule_ok(chk, "name matches the folder name"))
  # Reserved word.
  chk <- validate_skill(make_skill(root, "claude-helping", c("name: claude-helping", good_desc)))
  expect_false(rule_ok(chk, "name has no reserved word (anthropic, claude)"))
  # Too long.
  long <- paste(rep("a", 65), collapse = "")
  chk <- validate_skill(make_skill(root, long, c(paste0("name: ", long), good_desc)))
  expect_false(rule_ok(chk, "name at most 64 characters"))
  # Description: XML tags, length, missing.
  chk <- validate_skill(make_skill(root, "tagging-items", c("name: tagging-items",
                                                          "description: Tags <b>items</b>. Use when asked.")))
  expect_false(rule_ok(chk, "description has no XML tags"))
  chk <- validate_skill(make_skill(root, "padding-text", c("name: padding-text",
                                                         paste0("description: ", strrep("word ", 210)))))
  expect_false(rule_ok(chk, "description at most 1,024 characters"))
  chk <- validate_skill(make_skill(root, "missing-description", "name: missing-description"))
  expect_false(rule_ok(chk, "description present and non-empty"))
  # compatibility over 500 characters.
  chk <- validate_skill(make_skill(root, "checking-compat", c("name: checking-compat", good_desc,
                                                            paste0("compatibility: ", strrep("x", 501)))))
  expect_false(rule_ok(chk, "compatibility at most 500 characters"))
  # No frontmatter at all.
  d <- file.path(root, "no-frontmatter")
  dir.create(d)
  writeLines("# Just a body", file.path(d, "SKILL.md"))
  chk <- validate_skill(d)
  expect_false(rule_ok(chk, "frontmatter is valid YAML"))
  expect_false(attr(chk, "valid"))
  # No SKILL.md.
  d <- file.path(root, "empty-folder")
  dir.create(d)
  expect_false(attr(validate_skill(d), "valid"))
})

test_that("recommendations and scripts are reported without failing validity", {
  root <- withr::local_tempdir()
  d <- make_skill(root, "pdf-tool", c("name: pdf-tool", "description: I can help you with PDFs.", "extra: 1"),
                  body = c("See [guide](references/guide.md) and [missing](references/none.md).",
                           "Run scripts\\fix.R"))
  dir.create(file.path(d, "references"))
  dir.create(file.path(d, "scripts"))
  writeLines(c("# Guide", "See [deeper](more.md)", rep("line", 120)), file.path(d, "references", "guide.md"))
  writeLines("cat('hello')", file.path(d, "scripts", "fix.R"))
  chk <- validate_skill(d)
  expect_true(attr(chk, "valid"))
  expect_false(rule_ok(chk, "description in the third person"))
  expect_false(rule_ok(chk, "description says when to use the skill"))
  expect_false(rule_ok(chk, "name is a gerund (for example processing-pdfs)"))
  expect_false(rule_ok(chk, "only fields of the specification"))
  expect_false(rule_ok(chk, "referenced files exist"))
  expect_false(rule_ok(chk, "references one level deep"))
  expect_false(rule_ok(chk, "files over 100 lines have a table of contents"))
  expect_false(rule_ok(chk, "paths use forward slashes"))
  expect_true(is.na(rule_ok(chk, "bundled scripts reviewed by a person")))
  expect_equal(attr(chk, "scripts"), "scripts/fix.R")
})

test_that("skills written by the package round-trip and are never silently overwritten", {
  root <- withr::local_tempdir()
  desc <- "Checks reports: flags \"quoted\" claims. Use when reviewing a report."
  p <- .write_skill(root, "checking-reports", desc, c("# Checking reports", "", "Read the report."),
                    metadata = list(model = "none", date = "2026-10-11"),
                    references = list(`notes.md` = c("# Notes", "Short.")))
  chk <- validate_skill(p)
  expect_true(attr(chk, "valid"))
  expect_equal(attr(chk, "description"), desc)
  expect_error(.write_skill(root, "checking-reports", desc, "x"), "already exists")
  expect_silent(.write_skill(root, "checking-reports", desc, "x", overwrite = TRUE))
  expect_equal(.skill_name("Screening Abstracts (v2)!"), "screening-abstracts-v2")
  ls <- list_skills(root)
  expect_equal(ls$name, "checking-reports")
  expect_true(ls$valid)
})

test_that("use_skills adds the skill list and two safe tools", {
  root <- withr::local_tempdir()
  file.copy(system.file("extdata", "skills", "summarizing-methods", package = "researchAgentR"),
            root, recursive = TRUE)
  a <- agent("helper", "Help with research tasks.", model = "groq:openai/gpt-oss-20b")
  a2 <- use_skills(a, root)
  names <- vapply(a2$tools, `[[`, "", "name")
  expect_setequal(names, c("load_skill", "read_skill_file"))
  expect_match(a2$instructions, "- summarizing-methods: Summarizes the methods", fixed = TRUE)
  expect_match(a2$instructions, "do not try to run them", fixed = TRUE)
  # Applying twice does not duplicate tools or the skill list.
  a3 <- use_skills(a2, root)
  expect_length(a3$tools, 2)
  expect_equal(lengths(regmatches(a3$instructions, gregexpr("Available skills", a3$instructions))), 1)

  body <- call_tool("load_skill", a2$tools, arguments = list(name = "summarizing-methods"))
  expect_false(body$is_error)
  expect_match(body$output, "Summarizing methods")
  expect_match(body$output, "references/checklist.md", fixed = TRUE)
  expect_false(grepl("^---", body$output))          # frontmatter not repeated
  ref <- call_tool("read_skill_file", a2$tools,
                   arguments = list(name = "summarizing-methods", file = "references/checklist.md"))
  expect_false(ref$is_error)
  expect_match(ref$output, "Edge cases")
  for (bad in c("../../etc/passwd", "/etc/passwd", "references/../../x", "nothing.md")) {
    out <- call_tool("read_skill_file", a2$tools, arguments = list(name = "summarizing-methods", file = bad))
    expect_true(out$is_error, info = bad)
  }
  # With a run_r tool, the agent is told to use it for scripts.
  run_r <- define_tool(function(code) "ok", "Run R code in a sandbox.",
                       args = list(code = arg_string("R code")), name = "run_r")
  a4 <- use_skills(agent("helper", "Help.", model = "groq:openai/gpt-oss-20b", tools = list(run_r)), root)
  expect_match(a4$instructions, "pass the code to the run_r tool", fixed = TRUE)
})

test_that("style_to_skill writes a valid skill from a writing style", {
  root <- withr::local_tempdir()
  txt <- c("The model was estimated twice; the estimates were stable across samples.",
           "However, the second sample may differ, which the present study examines.")
  st <- structure(list(
    guide = "## Sentence architecture\n\nLong sentences joined by semicolons, as in \"estimated twice\".",
    profile = style_profile(txt),
    exemplars = data.frame(file = "a.txt", section = "", paragraph = 1:2, text = txt),
    sources = "a.txt", author = "Test Author", model = "none:none", date = "2026-10-11T00:00:00"),
    class = "writing_style")
  chk <- style_to_skill(st, "writing-like-test-author", dir = root)
  expect_true(attr(chk, "valid"))
  expect_true(file.exists(file.path(root, "writing-like-test-author", "references", "exemplars.md")))
  body <- readLines(file.path(root, "writing-like-test-author", "SKILL.md"))
  expect_true(any(grepl("### Sentence architecture", body, fixed = TRUE)))
  expect_error(style_to_skill(st, "writing-like-test-author", dir = root), "already exists")
})

test_that("create_skill and test_skill work live", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")), "GROQ_API_KEY not set")
  root <- withr::local_tempdir()
  chk <- create_skill("Extract the sample size and population from a methods paragraph.",
                      examples = "Two hundred adults completed the survey online.",
                      dir = root, model = "groq:openai/gpt-oss-120b")
  expect_s3_class(chk, "skill_check")
  expect_true(dir.exists(attr(chk, "path")))
  grader <- function(answer, task) list(pass = grepl("200|two hundred", answer, ignore.case = TRUE),
                                        score = NA_real_, reason = "")
  res <- test_skill(attr(chk, "path"), "Methods: Two hundred adults completed the survey online. Sample size?",
                    grader = grader, model = "groq:openai/gpt-oss-20b")
  expect_s3_class(res, "skill_test")
  expect_setequal(res$condition, c("with", "without"))
})
