# Capstone rubric: team proposal brief

The capstone team (a lead agent coordinating a literature scout, a screener, a
measurement finder, a design advisor and a funding scout) produces a short
proposal brief. The brief is graded on evidence that its content can be
trusted, not on how polished it reads.

Score each criterion 0 to 3. Maximum: 24 points.

| Score | Meaning |
|---|---|
| 0 | Missing |
| 1 | Present but not verifiable, or with unreported errors |
| 2 | Present and mostly verified; gaps are reported |
| 3 | Complete, verified and reproducible from the logs |

## Criteria

| # | Criterion | 3 points requires | Score |
|---|---|---|---|
| 1 | Verified references | Every reference passed `verify_citations()` (DOI resolves, metadata match, not retracted); failures are listed with what was done about them. | [ ] |
| 2 | Claim-evidence table | Every substantive claim in the brief has a row with a quoted passage, its source, and the outcome of `check_claims()`; unsupported claims are removed or marked. | [ ] |
| 3 | Measurement and design | Instruments come with reliability or validity evidence and a citation; the design includes a power analysis run in the R-code sandbox, with its code. | [ ] |
| 4 | Funding match | Opportunities or funded projects come from the logged queries (NIH RePORTER, NSF, Grants.gov), with identifiers that can be looked up. | [ ] |
| 5 | AI-use statement from the logs | Drafted with `ai_use_statement()` from the team's episodes; every placeholder is completed; no statement goes beyond the logs. | [ ] |
| 6 | Episode cost ledger | Cost and tokens per agent and for the whole team, failed attempts and retries included, with cost per successful brief section. | [ ] |
| 7 | Evaluation evidence | At least one component evaluated on held-out tasks with repeated trials: pass^k, cost per success, and a comparison with an equal-budget single agent or another baseline. | [ ] |
| 8 | Trace and integrity | Episode logs are submitted; the team identifies at least one failure found in a trajectory and how it was handled; stop reasons and budgets are reported. | [ ] |

Total: [ ] / 24

## Automatic deductions

- A fabricated or unverifiable reference presented as verified: criterion 1
  scores 0.
- A claim in the brief that contradicts its own quoted evidence: criterion 2
  scores at most 1.
- Confidential or participant data sent to a cloud provider without
  justification: criterion 8 scores 0.

## Feedback

Strengths: [ ]

Most important fix: [ ]
