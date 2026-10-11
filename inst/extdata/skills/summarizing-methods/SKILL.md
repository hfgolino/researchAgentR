---
name: summarizing-methods
description: Summarizes the methods section of an empirical paper as a fixed list of design elements. Use when the user asks for a methods summary, a study design overview, or a table of methods across papers.
license: GPL-3
metadata:
  author: "researchAgentR example"
  version: "1.0"
---

# Summarizing methods

Return exactly these fields, one line each, using only information stated in the text:

1. Design (for example cross-sectional survey, randomized experiment)
2. Sample (size, population, recruitment)
3. Measures (instrument names and number of items)
4. Analysis (models and software)
5. Preregistration (yes, no, or not reported)

Write "not reported" when the text does not say. Never infer a value.

For edge cases (several studies, missing sections), read [the checklist](references/checklist.md).

## Example

Input: "Two hundred adults completed the 10-item scale online; data were analyzed with EGA in R."

Output:

1. Design: not reported
2. Sample: 200 adults, recruited online
3. Measures: a 10-item scale
4. Analysis: exploratory graph analysis in R
5. Preregistration: not reported
