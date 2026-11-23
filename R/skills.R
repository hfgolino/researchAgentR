# Skills in the open Agent Skills format (https://agentskills.io/specification,
# accessed 2026-10-11): a folder with a SKILL.md file (YAML frontmatter, then
# Markdown instructions) and optional scripts/, references/ and assets/.
#
# Progressive disclosure: an agent always sees each skill's name and
# description (about 100 tokens per skill), loads the SKILL.md body only when
# a task calls for it, and reads bundled files only when the body points to
# them. use_skills() implements exactly this with two tools.

# ---- Reading and writing SKILL.md --------------------------------------------

# A minimal YAML reader for SKILL.md frontmatter, in plain R. It handles what
# the specification uses: `key: value` pairs, quoted values, block scalars
# (`|` keeps line breaks, `>` folds them), one level of nested `key: value`
# pairs (for `metadata`) and `- item` lists. Anything else is reported as an
# error rather than guessed.
.parse_frontmatter <- function(lines) {
  out <- list()
  errors <- character(0)
  unquote <- function(v) {
    v <- trimws(v)
    if (grepl("^\".*\"$", v)) {
      v <- substr(v, 2, nchar(v) - 1)
      v <- gsub("\\\\\"", "\"", v)
      return(gsub("\\\\\\\\", "\\\\", v))
    }
    if (grepl("^'.*'$", v)) return(gsub("''", "'", substr(v, 2, nchar(v) - 1)))
    v
  }
  i <- 1
  n <- length(lines)
  while (i <= n) {
    ln <- lines[i]
    if (!nzchar(trimws(ln)) || grepl("^\\s*#", ln)) { i <- i + 1; next }
    if (grepl("^\\s", ln)) {
      errors <- c(errors, paste0("unexpected indented line ", i, ": ", trimws(ln)))
      i <- i + 1; next
    }
    kv <- regmatches(ln, regexec("^([A-Za-z0-9_-]+):(.*)$", ln))[[1]]
    if (!length(kv)) {
      errors <- c(errors, paste0("line ", i, " is not `key: value`: ", ln))
      i <- i + 1; next
    }
    key <- kv[2]
    value <- trimws(kv[3])
    # Collect the indented lines that belong to this key.
    j <- i + 1
    while (j <= n && (grepl("^\\s", lines[j]) || !nzchar(trimws(lines[j])))) j <- j + 1
    block <- if (j > i + 1) lines[(i + 1):(j - 1)] else character(0)
    while (length(block) && !nzchar(trimws(block[length(block)]))) block <- block[-length(block)]
    if (grepl("^[|>][-+]?$", value)) {
      body <- sub("^\\s+", "", block)
      out[[key]] <- if (substr(value, 1, 1) == "|") paste(body, collapse = "\n") else
        trimws(gsub("\\s+", " ", paste(body, collapse = " ")))
    } else if (!nzchar(value) && length(block)) {
      items <- trimws(block[nzchar(trimws(block))])
      if (all(grepl("^- ", items))) {
        out[[key]] <- vapply(sub("^- ", "", items), unquote, character(1), USE.NAMES = FALSE)
      } else {
        sub_kv <- regmatches(items, regexec("^([^:]+):(.*)$", items))
        if (any(lengths(sub_kv) == 0)) {
          errors <- c(errors, paste0("could not read the entries under `", key, "`"))
        }
        sub_kv <- Filter(length, sub_kv)
        out[[key]] <- stats::setNames(lapply(sub_kv, function(x) unquote(x[3])),
                                      vapply(sub_kv, function(x) trimws(x[2]), character(1)))
      }
    } else {
      # A plain value may continue on indented lines (folded with spaces).
      out[[key]] <- unquote(paste(c(value, trimws(block)), collapse = " "))
    }
    i <- j
  }
  list(fields = out, errors = errors)
}

# Read a SKILL.md file into its frontmatter fields and Markdown body.
.read_skill_md <- function(file) {
  lines <- read_text(file, encoding = "UTF-8")
  if (!length(lines) || trimws(lines[1]) != "---") {
    return(list(fields = list(), body = lines, errors = "SKILL.md does not start with a `---` line"))
  }
  end <- which(trimws(lines[-1]) == "---")[1]
  if (is.na(end)) {
    return(list(fields = list(), body = lines, errors = "the frontmatter has no closing `---` line"))
  }
  fm <- .parse_frontmatter(lines[seq_len(end)[-1]])
  body <- if (end + 2 <= length(lines)) lines[(end + 2):length(lines)] else character(0)
  list(fields = fm$fields, body = body, errors = fm$errors)
}

# Quote a value for YAML: always double quotes, with backslashes and double
# quotes escaped, so colons and other YAML symbols in a description are safe.
.yaml_quote <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  paste0("\"", gsub("\"", "\\\\\"", gsub("\\s+", " ", x)), "\"")
}

# Write a skill folder. Shared by create_skill() and style_to_skill().
.write_skill <- function(dir, name, description, body, metadata = NULL, references = list(),
                         scripts = NULL, overwrite = FALSE) {
  path <- file.path(dir, name)
  if (dir.exists(path)) {
    if (!overwrite) .stop("The folder ", path, " already exists. Use overwrite = TRUE to replace it.")
    unlink(path, recursive = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  fm <- c("---", paste0("name: ", name), paste0("description: ", .yaml_quote(description)))
  if (length(metadata)) {
    fm <- c(fm, "metadata:", paste0("  ", names(metadata), ": ", .yaml_quote(unlist(metadata))))
  }
  fm <- c(fm, "---", "")
  write_text(c(fm, body), file.path(path, "SKILL.md"), encoding = "UTF-8")
  if (length(references)) {
    dir.create(file.path(path, "references"), showWarnings = FALSE)
    for (f in names(references)) {
      write_text(references[[f]], file.path(path, "references", f), encoding = "UTF-8")
    }
  }
  if (length(scripts)) {
    dir.create(file.path(path, "scripts"), showWarnings = FALSE)
    file.copy(scripts, file.path(path, "scripts", basename(scripts)))
  }
  path
}

# Turn any text into a valid skill name: lowercase, hyphens, at most 64.
.skill_name <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("^-|-$", "", substr(x, 1, 64))
  gsub("^-|-$", "", x)
}

# ---- Validation --------------------------------------------------------------

#' Check a skill folder against the Agent Skills rules
#'
#' Deterministic checks in plain R, no model involved. Rules of level
#' `"required"` come from the specification; a skill that fails one is not
#' valid. Rules of level `"recommended"` come from the authoring best
#' practices. The `"review"` row lists bundled scripts: a person should read
#' every script before an agent is allowed to run it.
#'
#' Required (<https://agentskills.io/specification> and
#' <https://platform.claude.com/docs/en/agents-and-tools/agent-skills/overview>,
#' accessed 2026-10-11): a `SKILL.md` with YAML frontmatter; `name` of 1 to
#' 64 characters, only lowercase letters, digits and hyphens, not starting or
#' ending with a hyphen, without `--`, equal to the folder name, without the
#' reserved words "anthropic" or "claude" and without XML tags; `description`
#' non-empty, at most 1,024 characters, without XML tags; `compatibility` at
#' most 500 characters; `metadata` a map of strings.
#'
#' Recommended
#' (<https://platform.claude.com/docs/en/agents-and-tools/agent-skills/best-practices>):
#' a description in the third person that says when to use the skill; a
#' gerund name (for example `"screening-abstracts"`); a body under 500 lines;
#' referenced files that exist; references one level deep (reference files
#' do not point to further files); a table of contents in files longer than
#' 100 lines; forward slashes in paths; only the fields of the specification.
#'
#' @param path The skill folder (or its `SKILL.md`).
#' @return A data frame of class `skill_check` with columns `rule`, `level`,
#'   `ok` (`NA` for rows that need human review) and `detail`; attributes
#'   `path`, `valid` (all required rules pass), `name`, `description` and
#'   `scripts`.
#' @examples
#' d <- file.path(tempdir(), "summarizing-methods")
#' dir.create(d, showWarnings = FALSE)
#' writeLines(c("---", "name: summarizing-methods",
#'              "description: Summarizes the methods section of a paper. Use when asked for a methods summary.",
#'              "---", "", "# Summarizing methods", "", "List design, sample, measures and analysis."),
#'            file.path(d, "SKILL.md"))
#' validate_skill(d)
#' @export
validate_skill <- function(path) {
  if (basename(path) == "SKILL.md") path <- dirname(path)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  rows <- list()
  add <- function(rule, level, ok, detail = "") {
    rows[[length(rows) + 1]] <<- data.frame(rule = rule, level = level, ok = ok, detail = detail,
                                            stringsAsFactors = FALSE)
  }
  finish <- function(name = NA_character_, description = NA_character_, scripts = character(0)) {
    out <- do.call(rbind, rows)
    req <- out$level == "required"
    structure(out, class = c("skill_check", "data.frame"), path = path,
              valid = all(out$ok[req] %in% TRUE), name = name, description = description,
              scripts = scripts)
  }
  md <- file.path(path, "SKILL.md")
  add("SKILL.md exists", "required", file.exists(md), if (!file.exists(md)) paste("not found in", path) else "")
  if (!file.exists(md)) return(finish())

  s <- .read_skill_md(md)
  add("frontmatter is valid YAML", "required", !length(s$errors), paste(s$errors, collapse = "; "))
  f <- s$fields
  xml <- "<[A-Za-z/!?][^>]*>"
  name <- f$name
  name_ok <- is.character(name) && length(name) == 1 && nzchar(name)
  add("name present", "required", name_ok)
  if (name_ok) {
    add("name at most 64 characters", "required", nchar(name) <= 64, paste(nchar(name), "characters"))
    add("name uses only a-z, 0-9 and hyphens", "required", grepl("^[a-z0-9-]+$", name), name)
    add("name does not start or end with a hyphen", "required", !grepl("^-|-$", name))
    add("name has no consecutive hyphens", "required", !grepl("--", name, fixed = TRUE))
    add("name matches the folder name", "required", identical(name, basename(path)),
        paste0("name '", name, "', folder '", basename(path), "'"))
    add("name has no reserved word (anthropic, claude)", "required",
        !grepl("anthropic|claude", name, ignore.case = TRUE))
    add("name has no XML tags", "required", !grepl(xml, name))
    add("name is a gerund (for example processing-pdfs)", "recommended",
        grepl("^[a-z0-9]+ing(-|$)", name), "")
  }
  desc <- f$description
  desc_ok <- is.character(desc) && length(desc) == 1 && nzchar(trimws(desc))
  add("description present and non-empty", "required", desc_ok)
  if (desc_ok) {
    add("description at most 1,024 characters", "required", nchar(desc) <= 1024,
        paste(nchar(desc), "characters"))
    add("description has no XML tags", "required", !grepl(xml, desc))
    first_second <- grepl("\\bI\\b", desc) || grepl("\\b(me|my|you|your)\\b", desc, ignore.case = TRUE)
    add("description in the third person", "recommended", !first_second,
        if (first_second) "avoid 'I', 'you': the description is placed in the system prompt" else "")
    add("description says when to use the skill", "recommended",
        grepl("\\bwhen\\b|\\bif the user\\b|\\bfor tasks\\b", desc, ignore.case = TRUE))
  }
  if (!is.null(f$compatibility)) {
    cmp <- paste(f$compatibility, collapse = " ")
    add("compatibility at most 500 characters", "required", nchar(cmp) >= 1 && nchar(cmp) <= 500,
        paste(nchar(cmp), "characters"))
  }
  if (!is.null(f$metadata)) {
    add("metadata is a map of strings", "required",
        is.list(f$metadata) && length(names(f$metadata)) == length(f$metadata) &&
          all(vapply(f$metadata, function(v) is.character(v) && length(v) == 1, logical(1))))
  }
  if (!is.null(f$`allowed-tools`)) {
    add("allowed-tools is a space-separated string", "recommended",
        is.character(f$`allowed-tools`) && length(f$`allowed-tools`) == 1,
        "experimental field; support varies between agents")
  }
  known <- c("name", "description", "license", "compatibility", "metadata", "allowed-tools")
  extra <- setdiff(names(f), known)
  add("only fields of the specification", "recommended", !length(extra),
      if (length(extra)) paste("unknown:", paste(extra, collapse = ", ")) else "")

  body <- s$body
  add("body is not empty", "recommended", any(nzchar(trimws(body))))
  add("body under 500 lines", "recommended", length(body) < 500, paste(length(body), "lines"))
  add("paths use forward slashes", "recommended",
      !any(grepl("[A-Za-z0-9_.-]+\\\\[A-Za-z0-9_.-]+\\.[A-Za-z0-9]+", body)))

  # Files the body points to must exist, and must not point further.
  refs <- .local_links(body)
  missing <- refs[!file.exists(file.path(path, refs))]
  add("referenced files exist", "recommended", !length(missing),
      if (length(missing)) paste("missing:", paste(missing, collapse = ", ")) else
        paste(length(refs), "file(s) named in the body"))
  files <- list.files(path, recursive = TRUE)
  # An agent loads a bundled file only when the instructions point to it.
  bundled <- files[grepl("^(references|assets)/", files)]
  unnamed <- setdiff(bundled, refs)
  add("bundled reference files are named in the body", "recommended", !length(unnamed),
      if (length(unnamed)) paste("never loaded unless SKILL.md names them:", paste(unnamed, collapse = ", ")) else
        paste(length(bundled), "bundled file(s)"))
  text_files <- setdiff(files[grepl("\\.(md|txt)$", files, ignore.case = TRUE)], "SKILL.md")
  nested <- text_files[vapply(text_files, function(tf) {
    links <- .local_links(read_text(file.path(path, tf), encoding = "UTF-8"))
    any(grepl("\\.(md|txt)$", links, ignore.case = TRUE))
  }, logical(1))]
  add("references one level deep", "recommended", !length(nested),
      if (length(nested)) paste("these files link to further files:", paste(nested, collapse = ", ")) else "")
  no_toc <- text_files[vapply(text_files, function(tf) {
    l <- read_text(file.path(path, tf), encoding = "UTF-8")
    length(l) > 100 && !any(grepl("^#+\\s*(table of )?contents\\b", utils::head(l, 40), ignore.case = TRUE)) &&
      sum(grepl("\\]\\(#", utils::head(l, 40))) < 3
  }, logical(1))]
  add("files over 100 lines have a table of contents", "recommended", !length(no_toc),
      if (length(no_toc)) paste("add one to:", paste(no_toc, collapse = ", ")) else "")

  scripts <- files[grepl("^scripts/", files)]
  add("bundled scripts reviewed by a person", "review", if (length(scripts)) NA else TRUE,
      if (length(scripts)) paste("read before running:", paste(scripts, collapse = ", ")) else "no scripts")
  finish(name %||% NA_character_, desc %||% NA_character_, scripts)
}

# Relative paths a Markdown text points to: link targets, and paths under
# scripts/, references/ or assets/ written in backticks or as plain text
# ("read references/guide.md."). Web links and anchors are excluded.
.local_links <- function(lines) {
  text <- paste(lines, collapse = "\n")
  links <- regmatches(text, gregexpr("\\]\\(([^)#\\s]+)\\)", text, perl = TRUE))[[1]]
  links <- sub("^\\]\\(", "", sub("\\)$", "", links))
  # a path ends with a letter, digit, "_" or "-": the period that ends a sentence is not part of it
  plain <- regmatches(text, gregexpr("(?<![A-Za-z0-9_/.-])(?:scripts|references|assets)/[A-Za-z0-9_./-]*[A-Za-z0-9_-]",
                                     text, perl = TRUE))[[1]]
  out <- unique(c(links, plain))
  out[!grepl("^[a-z]+://|^mailto:", out)]
}

#' @export
print.skill_check <- function(x, ...) {
  cat("<skill_check>", attr(x, "path"), "\n")
  status <- ifelse(is.na(x$ok), "REVIEW", ifelse(x$ok, "pass  ", "FAIL  "))
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  %s [%s] %s%s\n", status[i], substr(x$level[i], 1, 3), x$rule[i],
                if (nzchar(x$detail[i])) paste0(": ", x$detail[i]) else ""))
  }
  rec_fail <- sum(x$level == "recommended" & x$ok %in% FALSE)
  cat(if (isTRUE(attr(x, "valid"))) "Valid skill." else "NOT a valid skill: fix the FAIL [req] rows.",
      if (rec_fail) paste(rec_fail, "recommendation(s) not met.") else "", "\n")
  invisible(x)
}

#' List the skills in a folder
#'
#' @param dir A folder that contains skill folders (or a single skill folder).
#' @return A data frame with `name`, `description`, `path`, `valid`,
#'   `n_files` and `scripts` (number of bundled scripts).
#' @examples
#' list_skills(tempdir())
#' @export
list_skills <- function(dir = ".") {
  dirs <- if (file.exists(file.path(dir, "SKILL.md"))) dir else
    list.dirs(dir, recursive = FALSE)[file.exists(file.path(list.dirs(dir, recursive = FALSE), "SKILL.md"))]
  if (!length(dirs)) {
    return(data.frame(name = character(0), description = character(0), path = character(0),
                      valid = logical(0), n_files = integer(0), scripts = integer(0)))
  }
  do.call(rbind, lapply(dirs, function(d) {
    chk <- validate_skill(d)
    data.frame(name = attr(chk, "name"), description = attr(chk, "description"),
               path = attr(chk, "path"), valid = attr(chk, "valid"),
               n_files = length(list.files(d, recursive = TRUE)),
               scripts = length(attr(chk, "scripts")), stringsAsFactors = FALSE)
  }))
}

# ---- Giving skills to an agent -----------------------------------------------

#' Give an agent a folder of skills
#'
#' The agent's instructions get one line per skill (name and description);
#' two tools let it load a skill's instructions (`load_skill`) and read a file
#' bundled with it (`read_skill_file`) only when a task needs them. Scripts
#' in a skill are never executed by these tools: the agent is told to pass
#' code to a `run_r` tool when it has one (a sandbox, see the code tools).
#'
#' Skill text is treated as trusted instructions (it is not marked as
#' untrusted tool output), so give an agent only skills you have read.
#' [validate_skill()] lists the scripts to review. Invalid skills are skipped
#' with a warning.
#'
#' @param agent An [agent()].
#' @param dir A folder of skill folders, or one skill folder.
#' @return The agent with the skill list added to its instructions and the two
#'   skill tools added to its tools.
#' @examples
#' d <- file.path(tempdir(), "skills-demo", "summarizing-methods")
#' dir.create(d, recursive = TRUE, showWarnings = FALSE)
#' writeLines(c("---", "name: summarizing-methods",
#'              "description: Summarizes a methods section. Use when asked for a methods summary.",
#'              "---", "", "List design, sample, measures and analysis."), file.path(d, "SKILL.md"))
#' a <- agent("helper", "Help with research tasks.", model = "groq:openai/gpt-oss-20b")
#' a <- use_skills(a, dirname(d))
#' a
#' @export
use_skills <- function(agent, dir) {
  if (!inherits(agent, "agent")) .stop("`agent` must come from agent().")
  sk <- list_skills(dir)
  if (any(!sk$valid)) .warn("Skipping invalid skill(s): ", paste(sk$path[!sk$valid], collapse = ", "),
                           ". See validate_skill().")
  sk <- sk[sk$valid, , drop = FALSE]
  if (!nrow(sk)) .stop("No valid skills found in ", dir, ".")
  paths <- stats::setNames(sk$path, sk$name)

  load_skill <- function(name) {
    s <- .read_skill_md(file.path(paths[[name]], "SKILL.md"))
    files <- setdiff(list.files(paths[[name]], recursive = TRUE), "SKILL.md")
    paste0(paste(s$body, collapse = "\n"),
           if (length(files)) paste0("\n\nFiles in this skill: ", paste(files, collapse = ", ")) else "")
  }
  read_skill_file <- function(name, file) {
    # Only files inside the skill folder can be read: no absolute paths, no
    # "..", and the resolved path must stay inside the folder.
    if (grepl("^(/|[A-Za-z]:|~)", file) || grepl("(^|/)\\.\\.(/|$)", file)) {
      stop("Give a path relative to the skill folder, without '..'.")
    }
    root <- normalizePath(paths[[name]], winslash = "/")
    target <- normalizePath(file.path(root, file), winslash = "/", mustWork = FALSE)
    if (!startsWith(target, paste0(root, "/")) || !file.exists(target)) {
      stop("No file '", file, "' in skill '", name, "'.")
    }
    paste(read_text(target, encoding = "UTF-8"), collapse = "\n")
  }
  tools <- list(
    define_tool(load_skill, "Load the instructions of a skill. Call it before a task that matches the skill's description.",
                args = list(name = arg_enum("The skill's name", sk$name)),
                max_output_tokens = 6000, untrusted = FALSE),
    define_tool(read_skill_file, "Read a file bundled with a skill (for example references/guide.md), when the skill's instructions point to it.",
                args = list(name = arg_enum("The skill's name", sk$name),
                            file = arg_string("Path relative to the skill folder, with forward slashes, for example references/guide.md")),
                max_output_tokens = 6000, untrusted = FALSE))

  tool_names <- vapply(agent$tools, `[[`, character(1), "name")
  has_r <- any(grepl("run_r", tool_names))
  base <- agent$base_instructions %||% agent$instructions
  agent$instructions <- paste0(
    base, "\n\n## Skills\n",
    "Skills are folders of instructions for specialized tasks. When a task matches a skill, ",
    "call load_skill with its name first and follow the instructions it returns; call ",
    "read_skill_file for a file the instructions mention. Scripts in a skill are never run ",
    "automatically. ",
    if (has_r) "To run one, read it with read_skill_file and pass the code to the run_r tool.\n" else
      "No code tool is available, so do not try to run them.\n",
    "Available skills:\n", paste0("- ", sk$name, ": ", sk$description, collapse = "\n"))
  agent$base_instructions <- base
  agent$tools <- c(Filter(function(t) !t$name %in% c("load_skill", "read_skill_file"), agent$tools), tools)
  agent$skills <- paths
  agent
}

# ---- Creating and testing skills with a model --------------------------------

#' Draft a skill with a model
#'
#' One [chat()] call with a structured output drafts the skill's name,
#' description, instructions and optional reference files, following the
#' authoring best practices; the function writes the folder and checks it with
#' [validate_skill()]. Read and edit the draft before use: the model writes
#' instructions, not verified facts. Following the best practices, write the
#' test prompts for [test_skill()] first.
#'
#' Scripts are never written by the model: deterministic steps you want in the
#' skill are supplied as files through `scripts`, copied to `scripts/`, and
#' listed for review by [validate_skill()].
#'
#' @param description What the skill should do and when it should be used.
#' @param examples Optional character vector of example tasks (and, ideally,
#'   what a good result looks like).
#' @param name Optional skill name; by default the model proposes one. Either
#'   way it is converted to lowercase letters, digits and hyphens.
#' @param dir Folder in which the skill folder is created.
#' @param model The model that drafts the skill.
#' @param scripts Optional paths of script files to bundle in `scripts/`.
#' @param overwrite Replace an existing folder of the same name?
#' @return The [validate_skill()] result, with attributes `path`, `cost` and
#'   `model`.
#' @examples
#' \dontrun{
#' chk <- create_skill(
#'   "Screen abstracts for a systematic review against inclusion criteria given by the user.",
#'   examples = c("Screen these 20 abstracts: include randomized trials of adults."),
#'   dir = "skills")
#' chk
#' }
#' @export
create_skill <- function(description, examples = NULL, name = NULL, dir = ".",
                         model = default_model("lead"), scripts = NULL, overwrite = FALSE) {
  if (length(scripts) && !all(file.exists(scripts))) .stop("Script file(s) not found: ",
                                                          paste(scripts[!file.exists(scripts)], collapse = ", "))
  if (!is.null(name)) {
    name <- .skill_name(name)
    if (dir.exists(file.path(dir, name)) && !overwrite) {
      .stop("The folder ", file.path(dir, name), " already exists. Use overwrite = TRUE to replace it.")
    }
  }
  schema <- list(type = "object", additionalProperties = FALSE,
    required = list("name", "description", "body", "references"),
    properties = list(
      name = list(type = "string", description = "Gerund name, lowercase with hyphens, for example screening-abstracts"),
      description = list(type = "string", description = "Third person; what the skill does and when to use it; under 1024 characters"),
      body = list(type = "string", description = "The Markdown instructions (no frontmatter)"),
      references = list(type = "array", items = list(
        type = "object", additionalProperties = FALSE, required = list("file", "content"),
        properties = list(file = list(type = "string"), content = list(type = "string")))))
  )
  script_text <- if (length(scripts)) paste0(
    "\n\nBundled scripts (in scripts/; say when to run each one and what it returns):\n",
    paste0("scripts/", basename(scripts), ":\n",
           vapply(scripts, function(s) .shorten(paste(read_text(s, encoding = "UTF-8"), collapse = "\n"), 1500),
                  character(1)), collapse = "\n\n")) else ""
  system <- paste(
    "You write Agent Skills: a name, a description and Markdown instructions for another model.",
    "Rules: the name is lowercase letters, digits and hyphens, at most 64 characters, in gerund",
    "form, and never contains 'anthropic' or 'claude'. The description is in the third person,",
    "says what the skill does and when to use it, has no XML tags and stays under 1024",
    "characters. The body is concise (the reader is a capable model), under 500 lines, gives",
    "step-by-step instructions with an input and output example, and covers edge cases. Put",
    "long detail in reference files (references/<descriptive_name>.md) linked from the body",
    "one level deep; give any reference file over 100 lines a table of contents. Use forward",
    "slashes in paths. Avoid time-sensitive statements. Do not write scripts.")
  user <- paste0("Write a skill for this purpose:\n\n", description,
                 if (length(examples)) paste0("\n\nExample tasks:\n", paste0("- ", examples, collapse = "\n")) else "",
                 if (!is.null(name)) paste0("\n\nUse the name: ", name) else "", script_text)
  r <- chat(user, model = model, system = system, output_schema = schema, max_tokens = 8192)
  draft <- tryCatch(.from_json(r$content), error = function(e) NULL)
  if (is.null(draft) || !is.character(draft$body)) {
    .stop("The model did not return a valid skill draft (stop reason: ", r$stop_reason, ").")
  }
  name <- name %||% .skill_name(draft$name)
  # Reference files are written as references/<safe-name>.md; where the body
  # names a file differently, point it to the written one.
  refs <- list()
  body <- draft$body
  for (ref in draft$references) {
    if (!is.character(ref$file) || !nzchar(ref$file)) next
    stem <- .skill_name(sub("\\.[A-Za-z]+$", "", basename(ref$file)))
    f <- paste0(if (nzchar(stem)) stem else "reference", ".md")
    refs[[f]] <- ref$content
    written <- paste0("references/", f)
    if (ref$file != written && !grepl(written, body, fixed = TRUE)) {
      body <- gsub(ref$file, written, body, fixed = TRUE)
    }
  }
  meta <- list(`generated-by` = "researchAgentR::create_skill", model = r$model,
               date = format(Sys.Date()))
  path <- .write_skill(dir, name, draft$description, strsplit(body, "\n")[[1]], meta,
                       references = refs, scripts = scripts, overwrite = overwrite)
  chk <- validate_skill(path)
  attr(chk, "cost") <- r$cost
  attr(chk, "model") <- r$model
  chk
}

#' Test whether a skill helps
#'
#' Runs each test prompt with and without the skill (the same agent and
#' model, with and without [use_skills()]) and grades every answer, so the
#' skill's effect is measured rather than assumed. Write the prompts before
#' writing or editing the skill.
#'
#' @param path A skill folder.
#' @param prompts A character vector of tasks, or a data frame with a
#'   `prompt` column (and optionally `id` and `criteria`, passed to the
#'   grader as part of the task).
#' @param grader A function `(answer, task)` returning `list(pass, score,
#'   reason)`, as in the evaluation module. When `NULL`, an LLM judge from
#'   `grader_judge()` is used with a rubric built from the skill's
#'   description.
#' @param model The model of the agent under test.
#' @param trials Number of runs per prompt and condition.
#' @param tools Other tools for the agent (for example a `run_r` tool).
#' @param budget Limits for each episode.
#' @return A data frame of class `skill_test`, one row per prompt, condition
#'   (`"without"`, `"with"`) and trial, with `pass`, `score`, `reason`,
#'   `cost` (agent episode plus grader), `tokens`, `stop_reason` and
#'   `episode_id`; the attribute `"summary"` gives pass rate, mean score and
#'   cost per condition.
#' @examples
#' \dontrun{
#' res <- test_skill("skills/screening-abstracts",
#'                   prompts = c("Screen this abstract: ...", "Screen these two: ..."),
#'                   trials = 3)
#' res
#' }
#' @export
test_skill <- function(path, prompts, grader = NULL, model = default_model("lead"), trials = 1,
                       tools = list(), budget = researchAgentR::budget(max_steps = 6, max_cost = 0.05)) {
  chk <- validate_skill(path)
  if (!isTRUE(attr(chk, "valid"))) .stop("The skill is not valid; see validate_skill(\"", path, "\").")
  tasks <- if (is.data.frame(prompts)) prompts else data.frame(prompt = prompts, stringsAsFactors = FALSE)
  if (is.null(tasks$id)) tasks$id <- paste0("p", seq_len(nrow(tasks)))
  if (is.null(grader)) {
    gj <- .lazy_fn("grader_judge")
    if (is.null(gj)) .stop("Give a grader function, or load the evaluation module for grader_judge().")
    grader <- gj(paste("The answer completes the task correctly and follows the conventions of",
                       "this skill:", attr(chk, "description")))
  }
  base <- agent("skill_test", instructions = "Complete the user's task.", model = model, tools = tools)
  agents <- list(without = base, with = use_skills(base, path))
  rows <- list()
  for (i in seq_len(nrow(tasks))) {
    task <- as.list(tasks[i, , drop = FALSE])
    task$instruction <- task$prompt
    for (cond in names(agents)) for (k in seq_len(trials)) {
      ep <- run(agents[[cond]], task$prompt, budget = budget, verbose = FALSE)
      answer <- if (is.null(ep$answer)) "" else .as_text(ep$answer)
      g <- tryCatch(grader(answer, task), error = function(e) list(pass = NA, score = NA_real_,
                                                                    reason = paste("grader error:", conditionMessage(e))))
      tot <- .totals(ep)
      rows[[length(rows) + 1]] <- data.frame(
        id = task$id, condition = cond, trial = k, pass = as.logical(g$pass %||% NA),
        score = as.numeric(g$score %||% NA), reason = as.character(g$reason %||% ""),
        cost = tot$cost + (attr(g, "cost") %||% g$cost %||% 0), tokens = tot$tokens,
        stop_reason = ep$stop_reason, episode_id = ep$id, stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  summ <- do.call(rbind, lapply(split(out, out$condition), function(d) data.frame(
    condition = d$condition[1], runs = nrow(d), pass_rate = mean(d$pass, na.rm = TRUE),
    mean_score = mean(d$score, na.rm = TRUE), grader_failures = sum(is.na(d$pass)),
    cost = sum(d$cost), stringsAsFactors = FALSE)))
  structure(out, class = c("skill_test", "data.frame"), summary = summ[c("without", "with"), ],
            skill = attr(chk, "name"), model = model)
}

#' @export
print.skill_test <- function(x, ...) {
  cat("<skill_test>", attr(x, "skill"), "|", attr(x, "model"), "|", nrow(x), "runs\n")
  print(attr(x, "summary"), row.names = FALSE)
  invisible(x)
}
