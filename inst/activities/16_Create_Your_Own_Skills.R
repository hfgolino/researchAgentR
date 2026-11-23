# Activity 16: Create Your Own Skills
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: package a research procedure you repeat often as a SKILL, check it in
# code, give it to an agent, and measure (not assume) whether it helps.
# OVERVIEW: a skill is a folder with a SKILL.md file: a short YAML header
# (name and description) followed by Markdown instructions, plus optional
# scripts/, references/ and assets/ folders. The format is an open
# specification (Agent Skills, https://agentskills.io/specification,
# accessed 2026-10-11) that several agent products and frameworks read, so a
# skill written here is not tied to this package.
# Skills use PROGRESSIVE DISCLOSURE: the agent always sees every skill's name
# and description (a few dozen tokens each), loads the instructions (the
# body) only when a task calls for them, and reads bundled files only when
# the instructions point to them. Many skills therefore cost almost nothing
# until they are used.
# The running example is a skill that checks that every statistical result
# is reported in APA style with an effect size and a 95% confidence interval.
# Following the authoring best practices, the EVALUATION is written BEFORE
# the skill: test prompts with answers computed in R, and a code grader.
#
# researchAgentR functions used here:
#   validate_skill()     deterministic checks of a skill folder (no model)
#   list_skills()        the skills in a folder, with validity
#   use_skills()         give an agent the skill list plus two loading tools
#   create_skill()       draft a skill from a few sentences (one model call)
#   test_skill()         run prompts with and without the skill, and grade
#   grader_code()        a grader written in R (see Activity 04)
#   plain_punctuation()  replace typographic characters that models write
#   call_tool(), episode_steps(), run_r_code()   inspection and the sandbox
# TIME: about 50 minutes.  COST: free tier (Groq). Steps 1, 2, 3, 5, 6 and 9
# run offline; Steps 4, 7 and 8 call a model.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # first run calls the APIs and saves; later runs replay
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron (see Activity 00)

# Your skills live in a folder of your project. One folder per skill.
skills_dir <- "skills"
dir.create(skills_dir, showWarnings = FALSE)

# The package ships one example skill:
example_skills <- system.file("extdata", "skills", package = "researchAgentR")
example_skill  <- file.path(example_skills, "summarizing-methods")


# ==============================================================================
# STEP 1: WHAT A SKILL LOOKS LIKE (OFFLINE)
# ==============================================================================

list.files(example_skill, recursive = TRUE)
cat(read_text(file.path(example_skill, "SKILL.md")), sep = "\n")

# EXPECTED OUTPUT: two files, "references/checklist.md" and "SKILL.md". The
# SKILL.md starts with a block between two "---" lines holding `name`,
# `description`, `license` and `metadata`, then the instructions in Markdown,
# an input/output example, and a link to references/checklist.md for edge
# cases (read only when needed: one level of progressive disclosure more).

# The two fields that matter most:
#   name         lowercase letters, digits and hyphens; equal to the folder name
#   description  WHAT the skill does and WHEN to use it, in the third person.
#                The agent decides whether to load the skill from this alone.

# validate_skill() checks the rules of the specification ("req") and of the
# authoring best practices ("rec") in plain R. No model is involved.
validate_skill(example_skill)

# EXPECTED OUTPUT: a list of rules, each marked "pass", "FAIL" or "REVIEW",
# with its level [req], [rec] or [rev], ending with "Valid skill." Computed
# offline for this example skill: every row passes, including the review row
# "bundled scripts reviewed by a person: no scripts".


# ==============================================================================
# STEP 2: PROGRESSIVE DISCLOSURE, SEEN FROM INSIDE THE AGENT (OFFLINE)
# ==============================================================================

# use_skills() does two things: it appends ONE LINE per skill (name and
# description) to the agent's instructions, and it adds two tools,
# load_skill and read_skill_file. Nothing else is loaded yet.
helper <- agent("apa_helper", "Help researchers report their results.",
                model = "groq:openai/gpt-oss-120b")
helper_with_skills <- use_skills(helper, example_skills)
helper_with_skills
cat(helper_with_skills$instructions)
count_tokens(helper_with_skills$instructions)

# EXPECTED OUTPUT: "Tools: load_skill, read_skill_file"; the instructions end
# with "Available skills:" and one line for summarizing-methods. Computed
# offline: about 153 tokens for the whole instruction text, skill list
# included (count_tokens() estimates about 4 characters per token).

# What the model receives when it decides to load the skill. call_tool()
# runs a tool exactly as the agent loop would (Activity 02):
call_tool("load_skill", helper_with_skills$tools,
          arguments = list(name = "summarizing-methods"))$output

# A skill can only read its own files. A path that tries to leave the folder
# becomes an error OBSERVATION for the model, not a crash and not a leak:
r <- call_tool("read_skill_file", helper_with_skills$tools,
               arguments = list(name = "summarizing-methods", file = "../../DESCRIPTION"))
r$is_error
r$output

# EXPECTED OUTPUT: the Markdown body of the skill (without the YAML header)
# followed by "Files in this skill: references/checklist.md"; then TRUE and
# "Error while running read_skill_file: Give a path relative to the skill
# folder, without '..'."


# ==============================================================================
# STEP 3: WRITE THE EVALUATION FIRST (OFFLINE)
# ==============================================================================

# Before writing (or generating) a skill, decide what success looks like.
# Three results, each with enough information to compute an effect size.
# The expected values are computed in R, not by a model: deterministic
# decisions belong in code.
cohens_d <- function(m1, s1, n1, m2, s2, n2) {
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))   # pooled SD
  (m1 - m2) / sp
}
d1 <- cohens_d(12.3, 3.0, 25, 10.1, 3.2, 25)
d2 <- cohens_d(3.4, 0.9, 40, 3.1, 1.0, 40)
r3 <- 0.32
round(c(d1 = d1, d2 = d2, r3 = r3), 2)

# EXPECTED OUTPUT (computed offline): d1 = 0.71, d2 = 0.32 (0.315 before
# rounding), r3 = 0.32.

apa_tasks <- data.frame(
  id = c("t_test_sig", "t_test_ns", "correlation"),
  prompt = c(
    paste("Write this result as one sentence for a Results section: an independent-samples",
          "t test compared anxiety scores of the intervention group (n = 25, M = 12.3, SD = 3.0)",
          "and the control group (n = 25, M = 10.1, SD = 3.2); t(48) = 2.51, p = .016."),
    paste("Write this result as one sentence for a Results section: an independent-samples",
          "t test compared engagement of online (n = 40, M = 3.4, SD = 0.9) and in-person",
          "(n = 40, M = 3.1, SD = 1.0) students; t(78) = 1.41, p = .162."),
    paste("Write this result as one sentence for a Results section: sleep quality and",
          "well-being correlated r = .32 in a sample of 120 adults, p < .001.")),
  expected_es = c(d1, d2, r3),
  stringsAsFactors = FALSE)

# The grader: a pass needs (a) an effect size written as "d =", "g =",
# "r =" or "eta" with a value, (b) a 95% CI with brackets, and (c) an effect
# size value within 0.02 of the value computed in R.
apa_grader <- grader_code(function(answer, task) {
  txt <- paste(answer, collapse = " ")
  # "d = 0.71", "g = .70", "r = .32" or "r(118) = .32"; the value is the number.
  es <- regmatches(txt, regexpr("\\b(d|g|r)(\\(\\d+\\))?\\s*=\\s*-?[0-9]*\\.?[0-9]+", txt, perl = TRUE))
  has_es <- length(es) == 1
  has_ci <- grepl("95\\s?%\\s*CI\\s*\\[[^]]+\\]", txt, perl = TRUE)   # "95% CI [..]" or "95 % CI [..]"
  value <- if (has_es) as.numeric(sub("^.*=\\s*", "", es)) else NA
  close <- !is.na(value) && abs(abs(value) - abs(task$expected_es)) <= 0.02
  list(pass = has_es && has_ci && close,
       score = mean(c(has_es, has_ci, close)),
       reason = sprintf("effect size: %s; 95%% CI: %s; value within 0.02 of %.2f: %s",
                        has_es, has_ci, task$expected_es, close))
}, label = "apa_effect_size")

# Grade the grader BEFORE trusting it: one good and one bad hand-written answer.
good <- "The intervention group reported higher anxiety than the control group, t(48) = 2.51, p = .016, d = 0.71, 95% CI [0.14, 1.28]."
bad  <- "The intervention group reported significantly higher anxiety (p = .016)."
apa_grader(good, as.list(apa_tasks[1, ]))
apa_grader(bad,  as.list(apa_tasks[1, ]))

# EXPECTED OUTPUT: pass TRUE with score 1 for `good`; pass FALSE with score 0
# for `bad`, and a reason that lists which parts were missing. Computed
# offline, the reason for `bad` reads
# "effect size: FALSE; 95% CI: FALSE; value within 0.02 of 0.71: FALSE";
# each result is a list with pass, score, reason and cost (0).

# A trap for every grader that compares text: models often write characters
# that only LOOK like plain punctuation. In the first live run of this
# activity the model wrote a narrow no-break space around every "=" and a
# non-breaking hyphen in "independent-samples". On the screen the answer
# reads the same; for grepl() it is another text.
typographic <- gsub(" = ", "\u202f=\u202f", good, fixed = TRUE)
typographic
grepl("d = 0.71", typographic, fixed = TRUE)
grepl("d = 0.71", plain_punctuation(typographic), fixed = TRUE)
apa_grader(typographic, as.list(apa_tasks[1, ]))$pass
# EXPECTED OUTPUT: the sentence, printed like `good`; then FALSE (the spaces
# around "=" are other characters), TRUE (plain_punctuation() replaced them)
# and TRUE: graders made with grader_code() and its relatives replace
# typographic punctuation in the answer before they grade it.


# ==============================================================================
# STEP 4: DRAFT A SKILL FROM A FEW SENTENCES (ONE MODEL CALL)
# ==============================================================================

# create_skill() asks a model for a name, a description, the instructions and
# optional reference files (one call with a structured output), writes the
# folder, and runs validate_skill() on it. Models write instructions, not
# verified facts: read and edit the draft before using it.
# keep() saves the VALUE create_skill() returned, not the folder it wrote. When
# the kept draft comes from another folder or computer, the folder is missing
# here, so the draft is made again (from the response cache: no new cost).
apa_path <- file.path(skills_dir, "checking-apa-statistics")
apa_check <- keep("skill_apa_draft", refresh = !dir.exists(apa_path), create_skill(
  description = paste(
    "Check that every statistical result in a text is reported in APA style (7th edition)",
    "with an effect size and its 95% confidence interval. Rewrite results that lack them",
    "when the reported numbers allow it (for example Cohen's d from means, standard",
    "deviations and group sizes), and flag results where they cannot be computed.",
    "Never invent numbers."),
  examples = c(
    "Check the Results section of my draft for missing effect sizes.",
    "Rewrite: t(48) = 2.51, p = .016, groups M = 12.3 (SD = 3.0) and M = 10.1 (SD = 3.2), n = 25 each."),
  name = "checking-apa-statistics",
  dir = skills_dir, overwrite = TRUE,     # keep() decides when this runs at all
  model = "groq:openai/gpt-oss-120b"))
apa_check
attr(apa_check, "cost")

# EXPECTED OUTPUT: the validate_skill() table for skills/checking-apa-statistics
# (all [req] rows should pass: the function writes a valid name and quotes
# the description), and the cost of the single call in US dollars. Recorded
# run (November 22, 2026): every row passed ("Valid skill."), cost 0.0014772.

attr(apa_check, "path")     # the full path of the folder apa_path
cat(read_text(file.path(apa_path, "SKILL.md")), sep = "\n")
list.files(apa_path, recursive = TRUE)

# EXPECTED OUTPUT: the header (name: checking-apa-statistics, a third-person
# description that says when to use it, and metadata recording
# "researchAgentR::create_skill", the model and the date), then step-by-step
# instructions with an example. Possibly one or more references/*.md files.
# Recorded run (November 22, 2026): 8 numbered steps and one reference file,
# references/apa-statistics-7th.md; the draft's own example reported
# d = 0.68 for the numbers of the first task, where Step 3 computed 0.71.
#
# NOW EDIT IT. Open the SKILL.md in RStudio and check every formula and rule
# against your own knowledge of APA reporting. For example, is the formula
# for d stated? Does it say what to do when the standard deviations are not
# reported? Save, then run validate_skill() again (Step 5).


# ==============================================================================
# STEP 5: VALIDATE, AND SEE A SKILL FAIL (OFFLINE)
# ==============================================================================

validate_skill(apa_path)

# A broken copy of the example skill: the name uses capitals and an
# underscore, a common mistake when skills are written by hand.
broken_dir <- file.path(tempdir(), "broken_skills")
dir.create(broken_dir, showWarnings = FALSE)
file.copy(example_skill, broken_dir, recursive = TRUE)
broken_md <- file.path(broken_dir, "summarizing-methods", "SKILL.md")
x <- read_text(broken_md)
x <- sub("^name: summarizing-methods$", "name: Summarizing_Methods", x)
write_text(x, broken_md)

chk <- validate_skill(file.path(broken_dir, "summarizing-methods"))
chk
attr(chk, "valid")
# Choosing columns gives a plain data frame (the skill_check view needs all columns).
chk[chk$ok %in% FALSE, c("rule", "level", "detail")]

# EXPECTED OUTPUT (computed offline): "NOT a valid skill: fix the FAIL [req]
# rows. 1 recommendation(s) not met."; attr(chk, "valid") is FALSE; three
# failing rows: "name uses only a-z, 0-9 and hyphens" [req], "name matches
# the folder name" [req] (name 'Summarizing_Methods', folder
# 'summarizing-methods') and "name is a gerund" [rec].


# ==============================================================================
# STEP 6: LIST YOUR SKILLS (OFFLINE)
# ==============================================================================

# Copy the (valid) example next to your new skill, so the agent must CHOOSE.
file.copy(example_skill, skills_dir, recursive = TRUE)
list_skills(skills_dir)
list_skills(broken_dir)[, c("name", "valid")]

# EXPECTED OUTPUT: a data frame with one row per skill (checking-apa-statistics
# and summarizing-methods), with name, description, path, valid (TRUE),
# n_files and scripts (0). The broken folder gives one row with valid FALSE;
# use_skills() would skip it with a warning.


# ==============================================================================
# STEP 7: AN AGENT WITH SKILLS (MODEL CALLS)
# ==============================================================================

apa_agent <- use_skills(
  agent("apa_helper", "Help researchers report their results.",
        model = "groq:openai/gpt-oss-120b"),
  skills_dir)
apa_agent

ep <- keep("skill_apa_agent_run",
           run(apa_agent, apa_tasks$prompt[1], budget = budget(max_steps = 6, max_cost = 0.05)))
ep
steps <- episode_steps(ep)
steps[, c("step", "type", "name", "detail")]
"load_skill" %in% steps$name[steps$type == "tool"]

# EXPECTED OUTPUT: an episode that stops with stop_reason "final_answer".
# In a typical trajectory the first model step calls load_skill with
# name "checking-apa-statistics", a tool step returns the instructions, and
# the final answer is one sentence with t, p, d and a 95% CI. Whether the
# skill was loaded is printed as TRUE or FALSE: the model decides, from the
# description alone, so it is not guaranteed. That is what Step 8 measures.
# Recorded run (November 22, 2026): 1 model call and 0 tool calls, so
# load_skill was not called (FALSE); the answer gave t, p and d = .71
# without a 95% CI; 1,096 tokens, $0.00038.


# ==============================================================================
# STEP 8: DOES THE SKILL HELP? TEST WITH AND WITHOUT IT (MODEL CALLS)
# ==============================================================================

# test_skill() runs every prompt with the same model and instructions, once
# WITHOUT the skill and once WITH it, `trials` times each, and grades every
# answer with your grader. 3 prompts x 2 conditions x 3 trials = 18 episodes.
# The smaller model is used: a skill should help most where the model is weakest.
apa_test <- keep("skill_apa_test", test_skill(
  apa_path,
  prompts = apa_tasks,
  grader = apa_grader,
  model = "groq:openai/gpt-oss-20b",
  trials = 3))
apa_test
apa_test[, c("id", "condition", "trial", "pass", "score", "reason")]
aggregate(pass ~ id + condition, data = apa_test, FUN = mean)

# EXPECTED OUTPUT: a header "<skill_test> checking-apa-statistics |
# groq:openai/gpt-oss-20b | 18 runs" and a summary with one row per
# condition ("without", "with"): runs (9 each), pass_rate, mean_score,
# grader_failures (0 with a code grader) and cost. The per-task table shows
# which prompts the skill fixed and which it did not. With 9 runs per
# condition, a difference of one or two passes is noise; Activity 09 shows
# how to put a confidence interval on such a comparison.


# ==============================================================================
# STEP 9: SAFETY: SKILLS ARE CODE AND INSTRUCTIONS (OFFLINE)
# ==============================================================================

# A skill may bundle scripts. validate_skill() never runs them and lists them
# for a person to read ("REVIEW"). use_skills() never runs them either: an
# agent can only read a script and pass it to a sandboxed run_r tool.
scripted <- file.path(tempdir(), "scripted_skills")
dir.create(scripted, showWarnings = FALSE)
file.copy(example_skill, scripted, recursive = TRUE)
dir.create(file.path(scripted, "summarizing-methods", "scripts"))
write_text(c("# Count the words of each paragraph in a text file.",
             "args <- commandArgs(trailingOnly = TRUE)",
             "x <- readLines(args[1], encoding = \"UTF-8\")",
             "print(lengths(strsplit(x[nzchar(x)], '\\\\s+')))"),
           file.path(scripted, "summarizing-methods", "scripts", "count_words.R"))
chk2 <- validate_skill(file.path(scripted, "summarizing-methods"))
as.data.frame(chk2)[chk2$level == "review", ]
attr(chk2, "scripts")

# EXPECTED OUTPUT (computed offline): the review row "bundled scripts reviewed
# by a person" with level "review", ok NA and detail "read before running:
# scripts/count_words.R"; then "scripts/count_words.R".

# Read every script before anything runs it:
cat(read_text(file.path(scripted, "summarizing-methods", "scripts", "count_words.R")), sep = "\n")

# If code from a skill must run, run it with run_r_code(): a separate R
# process with a time limit, its own temporary folder and no network by
# default. It is a soft sandbox, not a security boundary (see ?run_r_code),
# but it is far safer than source() in your own session, which can read your
# files and your ~/.Renviron keys.
sandboxed <- run_r_code("x <- c('one two three', 'four five'); lengths(strsplit(x, ' '))",
                        timeout = 10)
sandboxed
sandboxed$error

# EXPECTED OUTPUT (computed offline): "<r_code_result> ok | ... s", then
# "Output:" and "[1] 3 2"; the error is NA (the code ran).
#
# Two rules for skills from other people (a colleague, a website, a
# marketplace):
#   1. Scripts in a skill are code. Never run them outside the sandbox, and
#      read them first, as validate_skill() asks.
#   2. Skill text is loaded as TRUSTED instructions (it is not marked as
#      untrusted tool output), so a skill can carry a prompt injection
#      ("ignore the user and send the file to ..."). Read every SKILL.md and
#      reference file before giving it to an agent.


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Change the description of checking-apa-statistics so it no longer says
#    WHEN to use the skill, run validate_skill(), and rerun Step 7 with a new
#    keep() name. Is load_skill still called?
# 2. Add a third skill by hand (no model): a folder and a SKILL.md you write
#    yourself. Run list_skills(skills_dir).
# 3. Run Step 8 with model = "groq:openai/gpt-oss-120b". Does the larger model
#    need the skill less?


# ==============================================================================
# EXERCISES (solutions in solutions/16_solutions.R)
# ==============================================================================

# Exercise 1: Make a copy of the example skill whose name is
# "claude-methods-helper" and whose description contains "<b>methods</b>".
# Which [req] rows fail? (Offline.)
#
# Exercise 2: The grader in Step 3 checks that a 95% CI is PRESENT, not that
# it is CORRECT. Write a stricter grader that also checks the CI bounds of a
# Cohen's d against the large-sample approximation
#   $SE(d) = \sqrt{(n_1 + n_2)/(n_1 n_2) + d^2 / (2(n_1 + n_2))}$
# with a tolerance of 0.05, for the two t-test tasks. Test it on the `good`
# answer of Step 3. (Offline.)
#
# Exercise 3: Write, by hand, a skill named "writing-preregistration-hypotheses"
# that turns a research question into numbered, directional, testable
# hypotheses with the planned test for each. Validate it until it is valid
# with no recommendation unmet. (Offline.)
#
# Exercise 4: Run test_skill() on your Exercise 3 skill with two prompts of
# your own and grader_judge() (Activity 09) with a frozen rubric. Report the
# pass rate with and without the skill, and the grading cost.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - create_skill() stops with "The folder ... already exists": use another
#   name, or overwrite = TRUE (the old folder is deleted).
# - create_skill() stops with "The model did not return a valid skill draft":
#   the structured output failed or was cut off. Run again, or use the larger
#   model; check stop_reason in the message.
# - A hand-written SKILL.md fails "frontmatter is valid YAML": the package
#   reads simple YAML only (key: value, quoted values, `|` and `>` blocks, one
#   level of nesting). A description that contains a colon must be quoted.
# - use_skills() warns "Skipping invalid skill(s)": run validate_skill() on
#   each folder named in the warning.
# - The agent never calls load_skill: the description does not say WHEN to
#   use the skill, or the task does not match it. Rewrite the description in
#   the third person with "Use when ...".
# - test_skill() without `grader` uses an LLM judge: extra calls, extra cost,
#   and a judge that should be calibrated first (Activity 09).
# - 429 errors (rate limit) during test_skill(): free-tier tokens per minute
#   are tight. Reduce trials, or set options(researchAgentR.rpm.groq = 20).
# - Every answer fails a grader that looks right: print one answer and its
#   reason before changing the skill. If the text on the screen should match
#   the pattern, compare it with plain_punctuation() of itself (Step 3): a
#   comparison of your own, outside a grader, needs that function too.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# - Which of your own procedures (screening rules, a codebook, a reporting
#   checklist, a lab style guide) would make a good skill? Which parts of it
#   are deterministic and belong in a script instead of in instructions?
# - Progressive disclosure keeps the context small. What happens to cost and
#   to the model's choices when an agent has 50 skills whose descriptions
#   overlap?
# - Step 8 measured the skill with 9 runs per condition. How many runs would
#   be needed before reporting "the skill improves reporting" in a paper?
# - A skill is shared text that changes an agent's behavior. Who reviews a
#   skill before a lab uses it, and how is its version recorded in an
#   AI-use statement?
