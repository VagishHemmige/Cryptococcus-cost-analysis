# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI coding agents working for them. It covers how to work here: conventions, data handling, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design and statistics: what the analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| [GitHub Issues](https://github.com/VagishHemmige/Cryptococcus-cost-analysis/issues) | Implementation status: where the code does not yet match the plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others.

## The project

This repository estimates the excess Medicare payments attributable to cryptococcosis after kidney transplant. It uses United States Renal Data System (USRDS) data, which link OPTN/UNOS transplant records to Medicare claims, and it publishes its code and methods as a Quarto website.

- **`analysis-plan.md` is the source of truth for design.** It covers the cohort, exposure, time zero, matching, cost construction, estimands, models, and sensitivity analyses. Code implements the plan; it does not redefine it.
  - If the code needs to depart from the plan, change the plan in the same pull request.
  - Plan changes are tracked in git, not in a change log inside the plan. Commit each design change on its own, with a message that gives the reason and says whether any results that depend on it had already been seen.
  - The plan lists open design choices as **Decision D*n*** in its "Open decisions" table. Do not settle one in code. Raise it with the maintainer.
- **This is an analysis project, not an R package**, even though the scripts live in `R/` and there is an `.Rbuildignore`. Do not add DESCRIPTION, NAMESPACE, roxygen, or pkgdown infrastructure. Do not apply CRAN conventions.
- **Implementation status lives in GitHub Issues**, not in these files. When you find a gap between the code and the plan, or a defect, open or update an issue. Reference the issue in the pull request that closes it.

## R scripts

- **Running the analysis.** Open the `.Rproj` and run `R/main.R` from the project root. `main.R` sources the other scripts in dependency order. `ARCHITECTURE.md` describes what each script does and how data flows between them.
- **One job per script:** setup, cohort construction, matching, cost processing, modeling, tables, figures. When you add a script:
  - source it from `main.R` at the right point
  - update `ARCHITECTURE.md` in the same commit
- **Header comment.** Each script begins with a comment saying what it does and which objects it needs from earlier scripts.
- **Constants, libraries, and code lists** live in `R/setup.R`. Other scripts do not call `library()`, re-source `setup.R`, or hard-code values. Any number a reader might want to change, such as a window length, a year, a caliper, or the matching ratio, is a named constant in `setup.R`.
- **Helper functions** go in `R/functions.R`, sourced after `setup.R`. A function takes its inputs as arguments and does not read global objects, including through default arguments.
- **No side effects outside the project.** Do not use `setwd()`, absolute paths, or writes outside the project. Locate data only through usRds's `set_USRDS_wd()`.
- **Exploration code** does not stay in pipeline scripts. Delete it, or move it below a chunk label that no Quarto page uses. The website shows the scripts as they are.
- **Deprecated scripts.** `R/define_cohort.R` is deprecated (it uses `strobe`) and is not sourced. Do not build on it; `R/create_cohort.R` is the active script.

## Chunk labels

Scripts are divided into labelled sections, so that Quarto pages can show the code without copying it:

```r
## ---- merge-costs
```

- Every logical step has a label. A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all scripts**. Quarto pulls chunks by label, so a duplicate label silently picks up the wrong code.
- Renaming or deleting a label breaks the Quarto page that uses it. Search the `.qmd` files before you change one.
- Scripts must still run end to end through `main.R`. Labels are comments and must never change what the code does.

## Variable labels

- Label analysis variables with human-readable descriptions, for example with `labelled::set_variable_labels()`. Tables then show readable names without renaming code by hand.
- For USRDS variables, use usRds's `apply_usrds_varlabels()` and `apply_usrds_factors()` rather than writing labels by hand.
- Label each derived variable where it is created. Include units, the price year for costs, and windows where they apply (for example, "Medicare payments, month 0, January 2021 dollars").

## Quarto website

The Quarto site documents the code. It does not run the analysis.

- **Setup files:** `_quarto.yml` and `index.qmd` sit at the root. There is one page per pipeline script in `quarto/`, named after the script (for example, `quarto/create_cohort.qmd` for `R/create_cohort.R`). The `custom-callout` extension in `_extensions/` provides the `Rcode` callout.
- **Showing code:** each page loads its script with `knitr::read_chunk("../R/<script>.R")` in a hidden chunk. It then shows each labelled section in a chunk with `#| eval: false`, with prose explaining what the section does and why. Pages link to the script on GitHub and to the scripts it depends on.
- **Never evaluate analysis code in the site.** Pages must render without USRDS data. The rendered site therefore contains code and explanation only, never results. The one exception is the Setup page, which runs the `constants` and `icd-code-lists` sections of `R/setup.R` and the tables that summarize them; those sections only define values and read nothing. No other section may run, and those two must never read data.
- **Rendering:** the site renders to `docs/`, which is not committed. `.github/workflows/publish.yml` renders it on every pull request as a check and, on each push to `master`, publishes it to the `gh-pages` branch, which GitHub Pages serves. Render locally to preview a change to a script's labels or a page's prose, but do not commit the output.
- **New scripts:** when you add a script, add its page and its sidebar entry in `_quarto.yml` in the same commit.
- **Prose must match the code.** If a change alters what a section does, update that section's explanation in the same pull request.

## Outputs

- **Exported files:** tables and figures are exported by `R/tables.R` and `R/figures.R` to `tables/` and `figures/`. Both folders are gitignored, because their contents are results under the disclosure rules below.
- **Figures** need alt text when they are published.
- `README.Rmd` is the source of `README.md`. Edit the `.Rmd` and re-knit; do not edit `README.md` directly.

## Restricted data (applies to everyone)

USRDS and Medicare data are covered by a data use agreement (DUA). The rules below apply to people and agents alike.

- **Never commit data.** That includes USRDS files, extracts, and any saved R object or output derived from patient-level data (`.rds`, `.RData`, `.parquet`, `.csv`, and the like). Keep data outside the repository, and check `.gitignore` before adding a new output type.
- **AI agents must not read restricted data.** Anything an agent opens, prints, or summarizes is sent to the model provider. Agents therefore must not:
  - open data files or patient-level objects
  - print rows or `head()` of them
  - display USRDS IDs, person-level dates, ZIP codes, or individual cost records

  This applies unless your own DUA explicitly permits that use. Agents may write code, review it, and run it on **synthetic data** that mimics the column names and types. Results on real data are produced by an authorized person.
- **Use synthetic data for development and tests.** Build small synthetic data frames in code, label them clearly as synthetic, and never derive them from real records.
- **Disclosure.** No table, figure, flowchart box, or text may show a count from 1 to 10. That includes counts that can be recovered by subtraction or from complementary cells, and counts in risk tables under plots. Figures that plot individual people (beeswarm, scatter) are not published. Check every output before it leaves the secure environment, and check the exact wording of the governing DUA before dissemination.

## Look in usRds first

The [usRds](https://github.com/VagishHemmige/usRds) package is the **first place to look** for anything that touches USRDS files: loading files, extracting claims, diagnosis dates, Medicare coverage, cohort helpers, costs, proration, and inflation adjustment.

- Check the package's function reference before writing project-specific code. Use an existing function, or extend it.
- **Package problems belong in the package.** If a usRds function is wrong, or does almost but not exactly what the plan needs, open an issue in the [usRds repository](https://github.com/VagishHemmige/usRds/issues) rather than documenting it here. Then do one of two things:
  - fix it in usRds, which is preferred when the behavior is generally useful
  - wrap it in this project, with a code comment that links the issue

  Remove the wrapper when the issue is fixed.
- Before relying on a function's behavior, read its documentation and its open issues. This matters most for `verify_medicare_primary()`, `establish_dx_date()`, `prorate_costs_by_day()`, and `adjust_costs_for_inflation()`, whose behavior directly sets the cohort and the cost outcome.
- Record the usRds version (or commit) used for any result.

## USRDS data facts

Sources: USRDS 2025 Researcher's Guide, Appendix B (data file descriptions), and Appendix C (formats). Check these against the release you are using.

**Institutional ("IN") claims cover more than inpatient care.** They include inpatient, outpatient, skilled nursing facility (SNF), home health, and hospice claims.

- `HCFASAF` gives the claim type: `I` inpatient, `O` outpatient, `N` SNF, `H` home health, `S` hospice. In older files, `M` is a REBUS inpatient stay.
- From 2002 on, each type has its own files: `ip_*`, `op_*`, `sn_*`, `hh_*`, and `hs_*`, each with a CLM (claim), DXP (diagnosis/procedure), and DET (revenue center) file.
- Institutional claims are uniquely identified by `USRDS_ID`, `CLM_FROM`, and `SEQ_KEYC`.

**Physician/supplier (PS) claims** come as a CLM (claim), LINE (line item), and DX (diagnosis) file.

- Each line carries one diagnosis and one procedure code.
- One visit produces several lines, so lines must be grouped into encounters for counts of visits.
- Be explicit about whether you use line-level or claim-level amounts and diagnoses.

**Payment and charge fields.** The cost outcome uses Medicare **payments**, never charges.

| File | Medicare payment | Charges (do not use as cost) |
|---|---|---|
| Institutional claim (`*_CLM`) | `CLM_AMT` | `CLM_TOT` |
| Institutional revenue center (`*_DET`) | `REVPMT` | `REV_CH` |
| Physician/supplier claim (`PS_CLM`) | `CLM_PMT_AMT` | `NCH_CARR_SBMT_CHRG_AMT` (submitted), `NCH_CARR_ALOW_CHRG_AMT` (allowed) |
| Physician/supplier line | `PMTAMT` | `SBMTCH` (submitted), `ALOWCH` (allowed) |

- `IP_DET` and `SN_DET` have no `REVPMT`. Revenue-center payments exist only for outpatient, home health, and hospice lines, so revenue-center payment totals do not include inpatient or SNF care.
- Summed `REVPMT` for an outpatient claim does not always equal that claim's `CLM_AMT` (about 3% higher overall). Never add institutional claim payments and revenue-center payments together; they describe the same services.
- Revenue center code `0001` is the sum of a claim's other revenue-center charges. Exclude it when summing revenue-center lines.
- Inpatient `CLM_AMT` excludes CMS pass-through payments (indirect medical education, capital, kidney acquisition). The pass-through amount is `PER_DIEM × CVR_DCNT`. Whether to include it is a design choice for the plan.
- From the 2015 files on, `PRPAYAMT` (primary payer paid amount) and `TDEDAMT` (beneficiary deductible and coinsurance) are also available.

**Claims year.** Claims are assigned to a SAF year by the service **end** date, not the start date.

**Data currency (2025 release).**

- Claims are complete through 2023-12-31, except for the Q1 2024 inpatient file.
- The core files, including deaths, run through 2024-12-31.
- Vital status runs ahead of claims by about a year.

**Payer history (`payhist`)**

- Eligible Medicare fee-for-service primary coverage: `PAYER` is `MPAB` or `MPO`. This is the Researcher's Guide definition (Example 8), and claims are complete only for these periods.
- Not eligible: `HMO` (Medicare Advantage), `MSP-EGHP`, `MSP-nonEGHP`, `OTH`, `WAIT`.
- A month without eligible coverage is unobserved, not zero cost. Code must never turn missing claims during uncovered time into zeros.
- Medicare entitlement based only on ESRD ends three years after a successful transplant. Expect coverage loss after that point.

**ICD transition.** ICD-9-CM applies to services before 2015-10-01. ICD-10-CM and ICD-10-PCS apply on or after that date.

- Codes are stored without periods.
- Decide, and document, which date sets the code system for a hospital stay that crosses the transition.

**Unclear variables.** If a variable's meaning is unclear, check Appendix B and Appendix C. Do not guess from the name.

## Coding conventions

- **Flowchart labels and text** are built from the constants in `setup.R`, not hard-coded. For example, a label that says "2007-2020" or "365+ days" goes stale when the constants change.
- **Code lists**
  - Each code carries its category, its ICD version or code type (diagnosis or procedure), and its matching rule (exact or prefix).
  - Each code belongs to exactly one category.
  - Check every code against the official CMS ICD-9-CM, ICD-10-CM, ICD-10-PCS, and HCPCS code sets.
  - Never invent codes, variables, files, or clinical definitions.
- **Cohort flow** uses the `flowchart` package (`as_fc()`, `fc_filter()`, `fc_split()`, `fc_draw()`). Every inclusion or exclusion goes through `fc_filter()`. Never drop rows from `$data` outside the flowchart; the diagram must reproduce the cohort exactly. The cohort used downstream is the data from the final flowchart step.
- **Matching**
  - Matching must be reproducible from a recorded seed. Control selection must not depend on row order.
  - The matched-set identifier is kept as an ordinary column through to the modeling data, so models and bootstraps can cluster on it.
  - Report the number of cases with each number of matched controls, including zero.
- **Costs**
  - Each person-month records both its cost and its number of observed days, so partial months are explicit.
  - Every cost carries its price year. Inflation adjustment uses the constants in `setup.R`.
  - Floors, caps, or other changes to cost values (for example, setting negative months to zero) are design choices. They belong in the plan before they go in code.
- **Joins**
  - Use `join_by()` with an explicit `relationship` where you can.
  - After every interval join (`between()`, `closest()`, `overlaps()`) and every join of claims to people, assert that it did not multiply rows.
  - Decide deliberately what happens to people with no match (NA rows): keep them, exclude them through a flowchart step, or stop with an error.
- **Dates and windows**
  - Write windows as inclusive day offsets from time zero, matching the plan (for example, month 0 is days 0 to 29).
  - Test the window edges on synthetic data.
- **Models.** Keep the code that turns model coefficients into reported dollars (contrasts, retransformation, sums over months) in named functions in `R/functions.R`, and test them on synthetic data with a known answer.
- **Style:** the tidyverse style guide. Style changes go in their own commits, separate from changes that alter analytical behavior.

## Checks for any change

Before opening a pull request, check the items below that apply to your change.

**Cohort construction**

- Do the order, the denominator, and the labels of each flowchart step match the condition it applies?
- Can each count be reproduced from the code?
- Is the downstream cohort the data from the final flowchart step?

**Exposure and time zero**

- Which claim files, claim types (`HCFASAF`), diagnosis positions, and date fields define cryptococcosis?
- Is matching exact or prefix, and are both ICD-9 and ICD-10 covered for the right dates?
- Is time zero defined as in the plan for cases and for controls?
- Is every covariate measured using only information from before time zero?

**Matching**

- Are controls at risk and undiagnosed at the case's time since transplant?
- Does control eligibility use only information available at that time, with no conditioning on future events?
- Is selection reproducible from the seed, and independent of row order?
- Are unmatched and partially matched cases counted and reported?

**Coverage and follow-up**

- Does Medicare coverage span the whole look-back window, not just its start?
- Are adjacent coverage spans merged without allowing uncovered days?
- Does follow-up end at every event the plan lists (death, loss of coverage, maximum follow-up, administrative end)?

**Costs**

- Are payment fields used, not charge fields?
- Is any service counted twice (claim plus revenue-center amounts, or revenue code `0001`)?
- Are uncovered months excluded rather than recorded as zero?
- Are claims assigned to the right month around time zero, including multi-day claims?

**Models and reported numbers**

- Does the reported estimate match the estimand in the plan (which months, which scale, which contrast)?
- Do standard errors account for clustering as the plan specifies?
- Do the tables, figures, and website prose say what the model actually estimates?

**Structure and documentation**

- Does `ARCHITECTURE.md` still describe the scripts and the data flow?
- Are the chunk labels unique, and does each script's Quarto page still find every label it uses?
- Does every script have a Quarto page and a sidebar entry, and does the pull request's render check pass?
- Are all site chunks `eval: false`, apart from the Setup page's `constants` and `icd-code-lists` sections and their summary tables?
- Are new derived variables labelled?

**Execution**

- Does `main.R` run from a clean session (`Ctrl+Shift+F10` in RStudio, then source `main.R`), with no objects left over from earlier work?
- Are paths relative, with no hard-coded data directories? Use `set_USRDS_wd()`.
- Are package versions recorded?
- Are any saved intermediates stale?

**Validation on real data** (run by an authorized person; report only aggregate, suppressed results)

- row counts before and after each join of claims to people
- the number of cases with 2, 1, and 0 matched controls
- standardized mean differences at time zero, including cost in months −3 to −1
- person-months by group and coverage status, and person-months after death
- case counts by ICD-9 versus ICD-10 era

## Working with agents

- **Autonomy.** The person directing an agent decides how much it may do. That person may also give the agent stricter rules of their own, and those take precedence over this section. By default, an agent may:
  - edit code and documentation
  - add scripts and tests
  - run code and render the site on synthetic data
  - commit to a branch and open a pull request
- **Collaborators:** `master` is protected. Changes reach it only through a pull request that the maintainer reviews.
- **Every agent, under any level of autonomy:**
  - follows the restricted-data rules above
  - asks before any step that would read real data, run the full pipeline, or make expensive calls
  - does not settle an open decision in the plan
- Keep pull requests focused. In each one, state which section of the plan or which issue it addresses, and what was checked. Say whether it was checked on synthetic data or by reading only. Never describe synthetic results as results.
- For references, use the USRDS Researcher's Guide and its appendices, the CMS code sets, and PubMed. Cite what you used.
