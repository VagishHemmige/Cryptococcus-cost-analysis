# Analysis plan: Potential missed opportunities for diagnosis of cryptococcosis among kidney transplant recipients using USRDS

- **Version:** 0.2 (draft for review)
- **Date:** 2026-10-24
- **Reporting frameworks:** STROBE and RECORD. Bracketed references indicate applicable reporting elements, not methodological validation.
- **Repository:** https://github.com/VagishHemmige/Cryptococcus-missed-diagnosis-USRDS
- **Status:** Prespecified design decisions are recorded below. Outstanding items marked **[VERIFY]** or **[TO FINALIZE]** must be resolved before analyses dependent on them are examined. Any subsequent changes should be entered in the change log.

## 1. Title, data source, and overview [S1, R1]

This retrospective claims-based study will identify potential missed diagnostic opportunities before the first observed cryptococcosis diagnosis among kidney transplant recipients in the United States Renal Data System (USRDS). It will use linked USRDS demographic, transplant, payer-history, residence, facility, inpatient (IN), and physician/supplier (PS) data. The primary analysis describes encounters within 90 days before the first observed diagnosis. Secondary analyses evaluate all-cause mortality and geographic associations.

The existing repository extracts IN and PS claims from **2006–2021**; these are extraction years rather than an independently verified final accrual period. **[VERIFY]** USRDS release, exact availability by claims source, calendar-year coverage, and feasible beginning/end dates for case accrual. The study's data linkage, source populations, and selection flow will be reported under RECORD.

## 2. Background and rationale [S2]

Cryptococcosis is a potentially severe infection in kidney transplant recipients. Preceding encounters for neurologic, respiratory, or nonspecific infectious manifestations, or associated diagnostic procedures, may be identifiable in administrative claims. Published cryptococcosis missed-opportunity studies provide a starting point for the diagnostic phenotype and 90-day prediagnosis window. This study expands assessment to USRDS IN and PS claims and distinguishes newly recorded symptom/condition categories from previous documentation.

An encounter meeting the algorithm represents a **claims-based potential missed opportunity**, not an adjudicated diagnostic error. Claims cannot establish symptom onset, prove that cryptococcosis was clinically recognizable at that encounter, or measure the actual duration of diagnostic delay.

**[TO FINALIZE]** Full clinical-background citations and source-study bibliographic details prior to manuscript submission.

## 3. Objectives and estimands [S3, S7]

**Primary objective:** Estimate the proportion of eligible recipients with at least one potential missed diagnostic opportunity in days −90 through −1 before the first observed cryptococcosis diagnosis. Report diagnosis-based, procedure-based, and combined opportunities separately; additionally describe numbers of distinct encounters, clinical categories, and elapsed time from earliest qualifying encounter to index.

**Secondary objectives:** (1) Estimate the association between at least one potential missed opportunity and 90-day all-cause mortality; (2) estimate associations of rural versus urban residence and continuous distance to the transplant center with having at least one potential missed opportunity.

**Additional mortality endpoints:** 30-day and 365-day all-cause mortality. **Exploratory:** Graft outcomes, subject to defensible definition and sufficient follow-up.

The mortality analysis is associational, not a causal effect estimate for diagnostic delay.

## 4. Design and setting [S4–S5]

A retrospective cohort of kidney transplant recipients with a first observed qualifying post-transplant cryptococcosis episode, using claims before index to ascertain potential missed diagnostic opportunities and USRDS mortality information after index. No noncryptococcosis matched control group is required for the primary analysis. All eligible cases constitute its denominator.

**Index date (day 0):** Earliest observed cryptococcosis diagnosis claim (IN or PS) among patients who ultimately meet the case-confirmation algorithm. Case-confirmation date may be later than index. A prior isolated cryptococcosis-coded PS encounter can establish the index date even if a later encounter confirms eligibility. The implication of arbitrarily distant confirmatory events will be evaluated in sensitivity analysis. **[VERIFY]** Assign clinically appropriate IN dates and determine relationship between claim-level `CLM_FROM`, admission, and discharge dates.

## 5. Participants, selection, and follow-up [S6, S13, R6, R13]

Selection will be recorded as a flowchart with counts remaining at each step:

1. Start with USRDS patient records and identify recipients with at least one kidney transplant.
2. Search available IN and PS diagnoses for cryptococcosis, including pretransplant history, using any available diagnosis position.
3. Confirm case status by **either one inpatient hospitalization** coded for cryptococcosis **or two distinct PS clinical encounters** coded for cryptococcosis **within 30 calendar days**. Multiple PS claim lines within one clinical encounter cannot satisfy this criterion.
4. Set index to the earliest observed cryptococcosis claim in a confirmed case, including an earlier isolated PS code.
5. Exclude patients whose **first observed cryptococcosis diagnosis is before their first kidney transplant**. Do not use a transplant occurring after index to retroactively exclude a qualifying earlier episode. Only the first observed eligible episode per recipient enters the cohort.
6. Require the index to occur after kidney transplantation, with transplant characteristics and time since transplantation derived from the **most recent kidney transplant preceding index**.
7. Require continuous eligible Medicare fee-for-service (FFS) primary coverage from day −365 through day 0. Use the longitudinal payer-history file; merge overlapping or adjacent eligible coverage spells without allowing uncovered days.
8. Exclude records with invalid critical chronology or insufficient claims observability, applying documented rules.

Recipients remain eligible regardless of graft function at diagnosis or return to dialysis. Graft status is descriptive; restriction to functioning grafts is a sensitivity analysis. Residence, center location, or other geographic missingness does **not** exclude a recipient from the main cohort or mortality analysis.

**[VERIFY]** The current repository accepts both USRDS payer categories `MPAB` (Medicare FFS primary for both Parts A and B) and `MPO` (Medicare FFS primary for other). Both were previously selected for the study, but `MPO` does not affirm both A/B parts. Validate with documentation applicable to the actual USRDS release, report the operational definition honestly, and prespecify an MPAB-only sensitivity analysis. Exclude Medicare Advantage and Medicare-secondary periods from eligible FFS-primary spells.

The first *observed* diagnosis is not necessarily incident disease. Available claims may not reveal cryptococcosis outside the observable period or payer system. **[TO FINALIZE]** Rules for contradictory transplant and diagnosis dates, same-day transplantation, and differences between initial isolated codes and much later confirmatory episodes.

## 6. Linkage and clinical covariates [S7, S8, R6.3, R12.3]

Use the USRDS patient identifier to link patient, transplantation, coverage, residence-history, IN, and PS data. For transplant-center geography, use the center assigned to the most recent transplant before index and year-appropriate facility information. Select the residence interval containing the index date. Confirm the expected number of matches for each person-date or person-transplant linkage; diagnose rather than silently discard overlapping or duplicate records. Report linkage completeness and any nonunique mappings.

Covariates for the principal mortality model: age at index, sex, time since most recent transplant, calendar year, and four targeted comorbidities—**diabetes mellitus, HIV infection, chronic liver disease, and malignancy**.

For each comorbidity, ascertain documentation during **days −365 through −91**. Classify as documented if at least one IN hospitalization or at least two distinct PS encounters contain relevant diagnosis codes in any available diagnosis position during this fixed period. Two PS encounters may occur anywhere within the period; identical codes are not required if they belong to the same comorbidity definition. Otherwise classify as *not documented* if claims were observable. This is not evidence of clinical absence. Missing denotes genuinely unknown or uninterpretable status, not lack of a qualifying code.

Describe baseline utilization (numbers of distinct inpatient hospitalizations and PS encounters during days −365 through −91) by opportunity status, but **do not include utilization as a covariate in mortality models**. Varying encounter frequency is a possible surveillance-bias source notwithstanding regular transplant follow-up.

## 7. Diagnosis and procedure phenotype [S8, R7.1]

Case-identifying cryptococcosis codes and candidate neurologic, pulmonary, infectious, and diagnostic-procedure categories are maintained in the repository. These lists will be clinically reviewed against source literature and official code definitions appropriate to each year before the corresponding implementation is finalized. Detailed individual code enumeration is outside this plan; publish the finalized code lists and matching rules as reproducible supplementary methodological material.

Use ICD-9-CM and ICD-10-CM diagnosis codes, ICD-9-CM Volume 3 and ICD-10-PCS institutional procedure codes, and clinically relevant CPT/HCPCS procedures in PS claims, as appropriate. Verify code system and effective date for each source: do not classify cross-transition inpatient hospitalizations solely from the claim start date. Verify available diagnosis positions, exact/prefix matching, leading zeros, decimals, valid category versus billable codes, and whether procedure fields correspond to the intended clinical action. Clinical judgment is required for ICD-9/ICD-10 mappings; automated crosswalks alone are insufficient.

Cryptococcosis diagnosis codes cannot independently count as missed-opportunity symptoms or conditions. The prediagnosis clinical-category and procedure lists will be finalized without reviewing dependent effect estimates. **[VERIFY]** Claims and historical code-system availability for the specific USRDS release.

## 8. Prediagnosis periods [S6, S7]

All times are relative to index day 0:

| Period | Days (inclusive) | Role |
|---|---|---|
| Fixed prior symptom history | −365 to −91 | Determine whether qualifying clinical diagnosis categories were previously documented; ascertain baseline comorbidities and utilization |
| Primary opportunity window | −90 to −1 | Identify potential missed opportunities |
| Nested sensitivity window | −60 to −1 | Repeat opportunity analyses in same cohort |
| Nested sensitivity window | −30 to −1 | Repeat opportunity analyses in same cohort |
| Index | 0 | First observed cryptococcosis diagnosis |

All windows use the same eligible cohort and the **same fixed history period**. Prior symptoms are assessed at the *clinical-category* level: a different code in an already documented category is not newly recorded. “Newly recorded” is a claims observation, not proof of new clinical onset. Qualifying procedures need **not** be new and can occur more than once, including when a similar procedure was recorded previously.

A claim's date alone does not establish its encounter boundary. **[VERIFY]** Date attribution for IN admissions, PS services, and claims overlapping the index date.

## 9. Potential missed-opportunity algorithm [S8, S12]

**Unit of analysis for counting:** Distinct clinical encounters, not claim lines or diagnosis codes.

**Diagnosis-based pathway:** An otherwise eligible encounter in the opportunity window has at least one qualifying diagnosis in a clinical category **absent during the fixed −365 to −91 history**.

**Procedure-based pathway:** An otherwise eligible encounter contains at least one qualifying diagnostic procedure; a prior procedure in the same category does not disqualify the encounter.

**Combined pathway:** An encounter meeting either definition qualifies. An encounter with multiple categories and/or procedures counts **once** in the combined encounter count, while contributing to all applicable category and pathway summaries. Separate subsequent encounters can qualify, including repeated procedures or the same qualifying category.

**Index diagnostic episode:** Exclude the entire index hospitalization and PS services associated with it. For a documented direct transfer into the index hospitalization, treat the transferring and receiving hospitalization(s) as one continuous diagnostic episode and exclude the full episode. Earlier, separate completed encounters remain eligible if temporally and clinically qualifying. For PS-index cases, identify and exclude an associated hospitalization where applicable; do not automatically designate a later hospitalization as the index hospitalization.

Combine PS claims associated with an inpatient stay rather than double-counting them. Deduplicate PS claim lines into distinct clinical encounters using defensible identifiers and service dates; different services on the same calendar date are not necessarily one encounter. Verify admission/discharge boundaries, direct-transfer indicators, and professional/institutional overlaps. **[TO FINALIZE BEFORE IMPLEMENTATION]** Encounter-grouping rules in edge cases where exact linkage is unavailable; transfer-identification validation.

Report the proportion of cases with any diagnosis-based, any procedure-based, and either type of opportunity; number of distinct opportunities per patient; category distributions; and days from earliest qualifying opportunity to index. This elapsed time is **not** a clinically measured diagnostic delay. Summarize the nested 30/60-day estimates using identical phenotype and counting rules.

## 10. Mortality outcomes and statistical plan [S10, S12]

The principal secondary outcome is **90-day all-cause mortality** after index. Additional outcomes are **30-day and 365-day all-cause mortality**. Follow from index until death, known administrative end of death ascertainment, or the horizon. Do **not** require survival through the outcome window for eligibility. Loss of Medicare FFS coverage does not automatically imply loss of USRDS death ascertainment; verify the death-data follow-up mechanisms and last observable dates. The outcome window should not extend beyond reliable mortality ascertainment without appropriate censoring.

Use Kaplan–Meier estimates, with absolute estimated mortality risk and 95% intervals at each horizon, stratified by any potential missed opportunity. The principal adjusted association uses a Cox proportional hazards model with the prespecified covariates in section 6 and exposure of any qualifying opportunity during days −90 to −1. Report adjusted hazard ratios and 95% confidence intervals. Check proportional hazards (including Schoenfeld residuals), functional forms for continuous covariates, influential observations, sparse events, and overfitting. Do not select covariates solely because of p values; if deaths are too few for the full model, document a prespecified or explicitly labeled simplified analysis rather than a data-driven model.

The exposure is classified from the pre-index window and fixed at index, so the risk period begins only after exposure ascertainment. Nonetheless, selection on confirmed cryptococcosis, survival until index, encounter frequency, and earlier care pathways may bias associations. Avoid adjusting for clinical findings first documented during the pre-index opportunity period or index hospitalization as if they were baseline confounders. Interpret adjusted associations cautiously; they do not demonstrate preventable mortality from diagnostic delay.

**[TO FINALIZE]** Confidence-interval methods for primary proportions and encounter counts, and details of any mortality analyses beyond the principal Cox model. **[VERIFY]** Date-of-death and follow-up coverage for 30/90/365-day horizons.

## 11. Geographic secondary objectives [S7, S12]

Use the patient’s residential ZIP code **at index**, from USRDS residence history, for both exposures. Patients with missing geography remain in nongeographic analyses.

**Rurality:** Use ZIP-code-based Rural–Urban Commuting Area (RUCA) classifications, with primary urban versus rural grouping based on primary RUCA codes **1–3 versus 4–10**, respectively. Assign a **period-appropriate RUCA version based on index year**, documenting release dates, mappings across study years, ZIP linkage method, and unmatched ZIPs. RUCA releases are not necessarily annual. **[VERIFY]** Historical versions and applicable ZIP crosswalks before geographic implementation.

**Distance:** Straight-line distance (miles) from residence ZIP centroid to the transplant center for the **most recent kidney transplant before index**; not driving distance or travel time. Model as continuous distance, reporting adjusted prevalence ratios for a **50-mile increase** and checking whether a nonlinear relation needs consideration. **[VERIFY]** Historical ZIP centroid data, facility geographic linkage, and geographic measurement method.

For each formal geographic exposure separately, fit a modified Poisson model with robust standard errors for binary outcome of any opportunity within the 90-day window, adjusted for age, sex, time since most recent transplant, calendar year, diabetes, HIV, chronic liver disease, and malignancy, subject to adequate sample size and model diagnostics. Report adjusted prevalence ratios and 95% intervals. Analyze complete cases for variables required by each geographic model and disclose denominators and exclusions. Distance to the transplant center is an imperfect proxy for location of prediagnosis care.

## 12. Missing information, bias, and sensitivity analyses [S9, S12c–e]

Keep all otherwise eligible patients in descriptive and primary frequency analyses; use complete cases for adjusted models and report missing covariate frequency, model sample sizes, and observed differences between retained and excluded records. No imputation is prespecified. Absence of a code under continuous eligible coverage is defined as *not documented*, not a missing value or confirmed absence of disease.

Prespecified sensitivity analyses include: primary 90-day versus nested 60- and 30-day windows; one cryptococcosis claim versus the confirmed definition; inpatient-only versus combined IN/PS case definitions; principal/first-listed cryptococcosis diagnoses versus any diagnosis position; functioning-graft restriction; MPAB-only payer-history restriction; and ICD-9 versus ICD-10 eras. Evaluate nonspecific symptom categories and possible prolonged direct-transfer encounters separately if the code list and ascertainment support those comparisons. Consider a confirmation-proximity restriction because a very early isolated PS cryptococcosis code can set the index despite much later confirmation. Record any added sensitivity analysis before viewing its results or explicitly label it post hoc.

Main threats include left truncation of historical diagnoses, imperfect FFS claims capture, selection by payer and case confirmation, code-list misclassification, ICD transition differences, detection/surveillance bias, indistinguishable dates within admissions, geographic exposure measurement error, confounding by severity, sparse death counts, and missing information. None of the algorithms confirms diagnostic error.

## 13. Data processing and reproducibility [R12, R22]

Document release year, actual SAF filenames, functions used to load claims, exact variable definitions and formats, linkage cardinality, coverage-interval consolidation, encounter consolidation, handling of duplicates/overlaps, code-system transitions, and analysis software/package versions. Include a reproducible participant-flow diagram and sensitivity definitions. Repository code and nonrestricted algorithms may be made public; restricted USRDS patient-level data must not be published. The plan is for an **analysis project**, not an R package.

Existing scripts `R/define_cohort.R` and `R/missed_diagnoses.R` are preliminary implementations and **do not yet implement all rules in this plan**. Changes to these files require separate approval and implementation, not merely adoption of this plan.

## 14. Disclosure

Follow applicable USRDS/CMS data-use restrictions. Do not publish counts of 1–10 or allow their derivation by subtraction or complementary cells, including in tables, flowcharts, figures, and prose. Check the exact disclosure language in the governing data-use agreement before dissemination.

## 15. Open verification items and change log

**Required before executing dependent analyses:** verify source release and claims availability; exact code-list matching and diagnosis/procedure fields; MPAB/MPO coverage interpretation; index-admission and transfer identification; encounter-level PS/IN deduplication; historical RUCA data and ZIP/center linkage; mortality follow-up; clinical code-list validation. Resolve remaining `[TO FINALIZE]` algorithm choices before examining their dependent results.

| Date | Version | Decision/change | Rationale | Timing relative to dependent results |
|---|---|---|---|---|
| 2026-10-24 | 0.2 | Reconciled protocol with decisions in project discussion: confirmed case criteria; earliest observed index; transplant and prior-disease chronology; fixed history and opportunity periods; encounter grouping; procedure pathway; mortality and geographic analyses; missingness | Move from generic STROBE/RECORD scaffold to study-specific prespecification | Before implementation/review of dependent results, to be confirmed |

