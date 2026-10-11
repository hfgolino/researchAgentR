# Solutions: Activity 16, Create Your Own Skills
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

library(researchAgentR)
use_cache("auto")
set_groq_key()

skills_dir <- "skills"
dir.create(skills_dir, showWarnings = FALSE)
example_skill <- system.file("extdata", "skills", "summarizing-methods", package = "researchAgentR")


# ==============================================================================
# Exercise 1: a reserved word and XML tags (offline)
# ==============================================================================

ex1_root <- file.path(tempdir(), "exercise1")
ex1 <- file.path(ex1_root, "claude-methods-helper")   # folder name = skill name
dir.create(ex1, recursive = TRUE, showWarnings = FALSE)
file.copy(file.path(example_skill, "references"), ex1, recursive = TRUE)
md <- readLines(file.path(example_skill, "SKILL.md"))
md <- sub("^name: summarizing-methods$", "name: claude-methods-helper", md)
md <- sub("^description: Summarizes the methods section",
          "description: Summarizes the <b>methods</b> section", md)
writeLines(md, file.path(ex1, "SKILL.md"))

chk1 <- validate_skill(ex1)
chk1
as.data.frame(chk1)[chk1$ok %in% FALSE, c("rule", "level")]

# EXPECTED OUTPUT (computed offline): two [req] rows fail, "name has no
# reserved word (anthropic, claude)" and "description has no XML tags", and
# one [rec] row, "name is a gerund". The name still matches its folder, so
# that rule passes. Reserved words and XML tags are refused because the name
# and description are pasted into the agent's system prompt.


# ==============================================================================
# Exercise 2: a grader that checks the CI, not only its presence (offline)
# ==============================================================================

cohens_d <- function(m1, s1, n1, m2, s2, n2) {
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  (m1 - m2) / sp
}
d_ci <- function(d, n1, n2) {
  se <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
  c(lower = d - 1.96 * se, upper = d + 1.96 * se)
}

apa_tasks_ci <- data.frame(
  id = c("t_test_sig", "t_test_ns"),
  expected_es = c(cohens_d(12.3, 3.0, 25, 10.1, 3.2, 25), cohens_d(3.4, 0.9, 40, 3.1, 1.0, 40)),
  n1 = c(25, 40), n2 = c(25, 40))
apa_tasks_ci$ci_lower <- mapply(function(d, a, b) d_ci(d, a, b)[["lower"]],
                                apa_tasks_ci$expected_es, apa_tasks_ci$n1, apa_tasks_ci$n2)
apa_tasks_ci$ci_upper <- mapply(function(d, a, b) d_ci(d, a, b)[["upper"]],
                                apa_tasks_ci$expected_es, apa_tasks_ci$n1, apa_tasks_ci$n2)
round(apa_tasks_ci[, c("expected_es", "ci_lower", "ci_upper")], 3)

# EXPECTED OUTPUT (computed offline): d = 0.709, CI 0.138 to 1.281 for the
# first task; d = 0.315, CI -0.126 to 0.756 for the second.

strict_grader <- grader_code(function(answer, task) {
  txt <- paste(answer, collapse = " ")
  es <- regmatches(txt, regexpr("\\b(d|g)\\s*=\\s*-?[0-9]*\\.?[0-9]+", txt, perl = TRUE))
  ci <- regmatches(txt, regexec("95%\\s*CI\\s*\\[\\s*(-?[0-9]*\\.?[0-9]+)\\s*,\\s*(-?[0-9]*\\.?[0-9]+)\\s*\\]",
                                txt, perl = TRUE))[[1]]
  if (!length(es) || length(ci) < 3) {
    return(list(pass = FALSE, score = 0, reason = "effect size or 95% CI missing"))
  }
  value <- as.numeric(sub("^.*=\\s*", "", es))
  bounds <- as.numeric(ci[2:3])
  ok <- c(es = abs(value - task$expected_es) <= 0.02,
          lower = abs(bounds[1] - task$ci_lower) <= 0.05,
          upper = abs(bounds[2] - task$ci_upper) <= 0.05)
  list(pass = all(ok), score = mean(ok),
       reason = paste(names(ok), ifelse(ok, "ok", "off"), collapse = "; "))
}, label = "apa_d_with_ci")

good <- "The intervention group reported higher anxiety than the control group, t(48) = 2.51, p = .016, d = 0.71, 95% CI [0.14, 1.28]."
wrong_ci <- "The intervention group reported higher anxiety than the control group, t(48) = 2.51, p = .016, d = 0.71, 95% CI [0.40, 1.00]."
strict_grader(good, as.list(apa_tasks_ci[1, ]))$pass
strict_grader(wrong_ci, as.list(apa_tasks_ci[1, ]))$reason

# EXPECTED OUTPUT (computed offline): TRUE for `good`; for `wrong_ci` the
# reason "es ok; lower off; upper off".


# ==============================================================================
# Exercise 3: a skill written by hand (offline)
# ==============================================================================

prereg <- file.path(skills_dir, "writing-preregistration-hypotheses")
dir.create(prereg, recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  "---",
  "name: writing-preregistration-hypotheses",
  "description: \"Turns a research question into numbered, directional and testable hypotheses, each with the planned statistical test and the result that would count against it. Use when the user drafts a preregistration or asks for hypotheses.\"",
  "---",
  "",
  "# Writing preregistration hypotheses",
  "",
  "1. Restate the research question in one sentence.",
  "2. Write each hypothesis as H1, H2, ...: name the variables, the direction of",
  "   the effect and the population.",
  "3. For each hypothesis, give the planned test (for example a two-sided Welch",
  "   t test, alpha = .05) and the smallest effect size of interest.",
  "4. State the result that would count AGAINST the hypothesis.",
  "5. If the question is exploratory, say so instead of writing a hypothesis.",
  "",
  "Never invent prior results or citations. Mark a needed citation as [CITATION NEEDED].",
  "",
  "## Example",
  "",
  "Input: Does mindfulness training reduce exam anxiety in first-year students?",
  "",
  "Output:",
  "",
  "H1. First-year students randomized to mindfulness training report lower exam",
  "anxiety than waitlist controls. Test: two-sided Welch t test, alpha = .05;",
  "smallest effect size of interest d = 0.30. Against H1: d below 0.30 or in the",
  "opposite direction."),
  file.path(prereg, "SKILL.md"))
validate_skill(prereg)

# EXPECTED OUTPUT (computed offline): "Valid skill." with every [req] and
# [rec] row passing. Quoting the description is a safe habit: a colon inside
# an unquoted description would break the YAML header.


# ==============================================================================
# Exercise 4: test the hand-written skill with an LLM judge (model calls)
# ==============================================================================

prereg_prompts <- data.frame(
  id = c("sleep", "feedback"),
  prompt = c("Write preregistration hypotheses for: does sleep restriction impair working memory in older adults?",
             "Write preregistration hypotheses for: does written feedback improve the quality of student essays more than grades alone?"),
  stringsAsFactors = FALSE)

prereg_judge <- grader_judge(paste(
  "Pass if the answer gives numbered hypotheses (H1, H2, ...), each directional,",
  "each with a named statistical test and alpha, and each with a result that would",
  "count against it. Fail if any hypothesis lacks one of these, or if the answer",
  "cites studies or numbers that the task did not give."),
  model = "groq:openai/gpt-oss-120b")

prereg_test <- keep("skill_prereg_test", test_skill(
  prereg, prompts = prereg_prompts, grader = prereg_judge,
  model = "groq:openai/gpt-oss-20b", trials = 3))
prereg_test
sum(prereg_test$cost)

# EXPECTED OUTPUT: the summary table with "without" and "with" rows (6 runs
# each): pass_rate, mean_score, grader_failures and cost. The cost column
# includes the judge's calls, which are part of the episode ledger of an
# evaluation (Activity 09 keeps grading cost in a separate column).
