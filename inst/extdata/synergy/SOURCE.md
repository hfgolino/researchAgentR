# Oud_2018 screening labels (from the SYNERGY dataset)

`Oud_2018_ids.csv`: one row per record screened for a systematic review of
specialized psychotherapies for adults with borderline personality disorder,
with the authors' FINAL inclusion decision (`label_included`: 1 = included in
the review, 0 = excluded at the title-and-abstract or the full-text stage).

- Columns: `openalex_id`, `doi` (empty when unknown), `label_included`.
- 986 records, 20 included. The SYNERGY file lists 1,053 records; the 67
  records that have neither an OpenAlex ID nor a DOI (all excluded) are left
  out, because their titles and abstracts cannot be retrieved.
- Titles and abstracts are not stored here: `get_works()` retrieves them from
  OpenAlex (CC0 metadata) when Activity 12 runs.

Source of the labels: the SYNERGY dataset, file
`datasets/Oud_2018/Oud_2018_ids.csv` of https://github.com/asreview/synergy-dataset
(commit dc2dadfdbb98eb1b4259604789abd640aa3b693e, 24 June 2025), released
under CC0 1.0. Retrieved 2026-10-12.

Cite:

- De Bruin, J., Ma, Y., Ferdinands, G., Teijema, J., & Van de Schoot, R.
  (2023). SYNERGY - Open machine learning dataset on study selection in
  systematic reviews (Version 1) [Data set]. DataverseNL.
  https://doi.org/10.34894/HE6NAQ
- Oud, M., Arntz, A., Hermens, M. L., Verhoef, R., & Kendall, T. (2018).
  Specialized psychotherapies for adults with borderline personality
  disorder: A systematic review and meta-analysis. Australian & New Zealand
  Journal of Psychiatry, 52(10), 949-961. https://doi.org/10.1177/0004867418791257

Eligibility criteria of the review, as recorded in SYNERGY (datasets.toml):
"We included RCTs on four specialized psychotherapies (DBT, MBT, TFP and ST)
for adults (18 years and older) with BPD, which included an individual
psychotherapy component and had a duration of 16 weeks or more. Eligible
comparison groups were other protocolized and specialized psychotherapies,
or control groups, for example, treatment as usual (TAU), waiting list,
attention control or community treatment by experts (CTBE). Studies were
excluded with an arbitrary cut-off of <66% of the participants having BPD,
unless disaggregated data were provided. Also, studies were excluded that
tested incomplete versions of specialized treatment, for example, studies
that investigated only skills training instead of the full DBT program."
