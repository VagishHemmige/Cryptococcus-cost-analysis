# Statistical Analysis Plan: Medicare Costs of Cryptococcosis After Kidney Transplant

**Version:** 0.1 (draft) · **Date:** 2026-10-23 · **Author:** Vagish Hemmige

This plan states the intended design and analysis. It does not describe the current state of the code; implementation status is tracked separately. Items marked **Decision D*n*** are intended choices not yet made; they are collected in [Open decisions](#13-open-decisions).

Reporting will follow STROBE, its RECORD extension for routinely collected health data, and, for the target-trial sensitivity analysis, the TARGET guideline. The [reporting map](#12-reporting-map) links each checklist item to a section of this plan.

---

## 1. Objective

To estimate the excess Medicare payments attributable to cryptococcosis in the 12 months after diagnosis among adult kidney transplant recipients, compared with similar recipients at the same time since transplant who have not been diagnosed.

**Secondary objectives**

- Describe the month-by-month profile of excess cost from 3 months before to 12 months after diagnosis.
- Partition excess cost by care setting (inpatient, outpatient, skilled nursing, home health, hospice, dialysis, physician/supplier).

## 2. Design

Retrospective cohort study in linked registry and claims data, using **risk-set (incidence density) matching**. Each case is matched, at the time since transplant when they are diagnosed, to controls who are still at risk and undiagnosed at that same time.

**Decision D10:** the matching method — direct covariate matching, or matching on a time-varying propensity score ([Section 6](#6-matching)).

A **sequential target-trial emulation** with propensity-score weighting is the planned main sensitivity analysis ([Section 9](#9-sensitivity-analyses); protocol in [Appendix A](#appendix-a-target-trial-protocol)). It requires a propensity score model whichever matching method is chosen.

## 3. Data source

- **United States Renal Data System (USRDS)** standard analysis files, accessed under a data use agreement through the `usRds` R package.
  - Core files: patients, transplant (TX), payer history (PAYHIST), treatment history (RXHIST), death.
  - Transplant files linked from OPTN/UNOS transplant recipient registration (TRR) records for kidney and kidney–pancreas recipients.
  - Medicare Parts A and B claims: institutional claims and revenue-center lines, and physician/supplier claims. Part D prescription events where a covariate requires them.
- **Linkage:** USRDS links CMS and OPTN records to a single USRDS patient identifier. The study uses this linkage as supplied; no further linkage is performed.
- **Period:** claims 2006–2021; incident cryptococcosis 2007–2020; administrative end of follow-up 2022-01-01.
- **Geography:** United States.

## 4. Study population

### 4.1 Source population

Kidney transplant recipients in USRDS with a transplant before 2021.

### 4.2 Eligibility at time zero (cases and controls alike)

At the risk-set time ([Section 5.3](#53-time-zero)), a person is eligible if they:

1. Have had at least one kidney transplant before that date.
2. Are aged 18 or older.
3. Have no prior cryptococcosis diagnosis.
4. Have Medicare fee-for-service as primary payer for both Part A and Part B for the 365 days before time zero, without gaps, so that baseline covariates and baseline cost are fully observed. **Decision D1:** whether Medicare as secondary payer or other payer categories are ever eligible.
5. Are alive and under observation on that date.

### 4.3 Index transplant

The time scale is time since the **most recent** kidney transplant before time zero. Retransplant recipients are eligible; prior transplants enter as covariates. **Decision D2:** inclusion of kidney–pancreas and other multi-organ recipients (exclude, or include with an indicator).

## 5. Exposure and time zero

### 5.1 Exposure definition

Incident cryptococcosis, identified from ICD-9-CM and ICD-10-CM diagnosis codes on Medicare institutional or physician/supplier claims. The full code list is published as a supplementary table.

**Decision D3:** the claim algorithm, meaning the claim types and diagnosis positions accepted and the number of claims required (for example, one inpatient claim, or two outpatient claims within a set window). The choice will cite any available validation literature for fungal infection codes in claims.

### 5.2 Diagnosis date

The service date of the first qualifying claim.

### 5.3 Time zero

- **Cases:** the diagnosis date.
- **Controls:** the date on which the control reaches the same number of days since their most recent transplant as the case had at diagnosis.

Eligibility, covariates and baseline cost are measured as of time zero, using only information available before it.

### 5.4 Control eligibility

Any person meeting [Section 4.2](#42-eligibility-at-time-zero-cases-and-controls-alike) at the case's risk-set time is eligible, **including people who are diagnosed with cryptococcosis later**. Their time before diagnosis is unexposed time. Excluding them would condition on the future.

**Decision D4:** whether a person may serve as a control in more than one risk set (sampling with replacement across risk sets, as in standard incidence density sampling) or at most once.

## 6. Matching

### 6.1 Common elements

Whichever method is chosen under D10:

- **Exact matching** on current graft status (functioning, or failed and on dialysis).
- **Ratio:** up to 2 controls per case.
- **Order:** cases are matched in a random order fixed by a recorded seed. Ties among controls are broken at random using the same seed.
- **Pre-period cost:** months −3 to −1 are **not** used for matching. They are kept to check balance on pre-diagnosis cost (Daw and Hatfield, *Health Services Research* 2018).

### 6.2 Covariates

These are the confounders the design must address. Under Option A ([Section 6.3](#63-option-a-direct-covariate-matching)), a subset is matched on directly and the rest are checked for balance. Under Option B ([Section 6.4](#64-option-b-time-varying-propensity-score-matching)), all enter the score. All are measured before time zero.

| Domain | Covariates |
|---|---|
| Demographics | Age, sex, race and ethnicity, calendar year |
| Transplant | Time since transplant (the time scale), donor type (living or deceased), retransplant and number of prior transplants, cause of kidney failure, time on dialysis before transplant, induction regimen |
| Graft function | Current graft status (functioning, or failed and on dialysis) |
| Immunosuppression intensity | Rejection treatment in the prior 3–6 months; maintenance regimen where observable |
| Comorbidity | Diabetes, cirrhosis, HIV, other solid organ transplant, a claims-based comorbidity index |
| Other infections | CMV, BK virus, *Pneumocystis* pneumonia, other invasive fungal infection, sepsis |
| Prior use of care | Hospital admissions, emergency visits, skilled nursing days in the prior year |
| Prior cost | Medicare payments in months −12 to −4, as a smoothed measure (mean or log of monthly cost) |
| Payer and place | Medicaid dual eligibility, region |

Each covariate's code list or registry field will be specified and published before analysis. **Decision D5:** the lookback window for rejection treatment, and the comorbidity index (Charlson or Elixhauser).

### 6.3 Option A: direct covariate matching

- Exact matching on diabetes, cirrhosis and HIV status, in addition to graft status.
- Calipers on age (±10 years) and calendar time (±3 years).
- Among eligible controls, the closest on smoothed prior cost (months −12 to −4) is chosen first.
- The remaining covariates in Section 6.2 are checked for balance and, if imbalanced, adjusted for in the outcome model.

**Strength:** simple to explain and to reproduce. **Weakness:** exact matching becomes sparse as matching factors are added, so few of the covariates in Section 6.2 can be matched on directly.

### 6.4 Option B: time-varying propensity score matching

- A time-varying propensity score for cryptococcosis diagnosis is estimated on the person-time of the eligible cohort, with a discrete-time logistic model on person-months (or, equivalently, a Cox model with time-varying covariates). Each person-interval's linear predictor is its score at that time (Lu, *Biometrics* 2005).
- All covariates in Section 6.2 enter the score.
- Controls are chosen by nearest neighbour on the logit of the score, within a caliper of 0.2 standard deviations of the logit (Austin 2011).
- **Decision D6:** whether to keep the hard calipers on age and calendar time from Option A on top of the score.

**Strength:** balances many covariates at once, and the same score model serves the target-trial sensitivity analysis. **Weakness:** depends on a correctly specified score model and is harder to explain.

### 6.5 Matching outcomes reported

- The number of cases with 2, 1 and 0 matched controls.
- The characteristics of unmatched cases compared with matched cases.

## 7. Outcome

### 7.1 Cost measure

**Medicare paid amounts** (not charges) from institutional claims and physician/supplier claims. Institutional claims spanning several days are prorated evenly across days of service.

### 7.2 Inflation adjustment

All costs are expressed in January 2021 US dollars. **Decision D7:** the price index (for example, the medical care component of the CPI-U, or the PCE health price index).

### 7.3 Person-month structure

- Each person's follow-up is divided into 30-day months relative to time zero, from month −3 (days −90 to −61) through month 11 (days 330 to 359).
- Each person-month's cost is the sum of prorated, inflation-adjusted payments for services in that month.
- Partial months are kept, with the number of observed days recorded as exposure.
- A month with Medicare coverage and no claims has a cost of zero. A month without qualifying coverage is not observed; it is never recorded as zero cost.

### 7.4 End of follow-up

Follow-up ends at the earliest of:

- day 365 after time zero;
- death;
- loss of Medicare fee-for-service primary coverage for Parts A and B;
- the administrative end date, 2022-01-01.

Graft failure after time zero does not end follow-up; it is an outcome pathway, not a censoring event.

**Decision D8:** the treatment of death, which defines the estimand. In option (a), cost accrues only while alive, and death ends follow-up. In option (b), months after death count as zero cost, so the estimand includes the cost consequences of mortality. Whichever is primary, the other is a sensitivity analysis.

## 8. Statistical analysis

### 8.1 Descriptive analysis

- A cohort flow diagram from the source population through eligibility, case identification, and matching.
- Baseline characteristics at time zero, by group, before and after matching.

### 8.2 Balance

- Standardized mean differences at time zero for every covariate in [Section 6.2](#62-covariates), and for cost in months −3, −2 and −1.
- An absolute standardized mean difference below 0.1 is considered adequate balance.
- Balance on months −3 to −1, which were not matched on, is reported as a check on residual confounding.

### 8.3 Estimand

The **average treatment effect in the treated (ATT)**: the mean difference in total Medicare payments over months 0–11 between cases and their matched controls, per case.

### 8.4 Primary model

- Person-month cost is modelled with a generalized linear model with log link and Tweedie variance, including group, month (as a categorical variable), and their interaction, with log observed days as an offset.
- Standard errors are estimated with generalized estimating equations clustered on matched set; robust standard errors also cluster on patient if [D4](#13-open-decisions) allows a person in more than one set.
- **Retransformation to dollars:** predicted monthly cost for each group is computed at a full 30-day month by recycled predictions. The ATT is the sum of case-minus-control differences over months 0–11.
- Confidence intervals come from a cluster bootstrap that resamples matched sets (1,000 replicates, or more if needed for stability).
- **Decision D9:** whether the primary estimate subtracts the pre-period difference (months −3 to −1), giving a difference-in-differences, or relies on matching alone with the pre-period reported as a balance check.

### 8.5 Secondary analyses

- The month-by-month excess cost profile for months −3 to 11, from the primary model.
- Excess cost by care setting, using the same model on each setting's cost.

### 8.6 Missing data

- Cost outcomes are observed only during covered months, by design ([Section 7.3](#73-person-month-structure)).
- For covariates with missing registry values, missingness is described. If it exceeds 5% for any covariate in the score, multiple imputation by chained equations is used, with the score estimated within each imputation.

### 8.7 Software

Analyses are run in R. Code is public at https://github.com/VagishHemmige/Cryptococcus-cost-analysis, and a website at https://vagishhemmige.github.io/Cryptococcus-cost-analysis documents each step. Data are not shared, as the data use agreement requires.

## 9. Sensitivity analyses

1. **Sequential target-trial emulation** ([Appendix A](#appendix-a-target-trial-protocol)). Monthly trials of everyone eligible, with the newly diagnosed as the exposed group. The comparison group is weighted by propensity odds (ATT weights), trials are pooled, and robust standard errors are clustered on patient. Weights are truncated at the 1st and 99th percentiles, with untruncated results also reported.
2. **Alternative death handling:** the option in D8 not chosen as primary.
3. **Alternative outcome models:** Gaussian identity-link and log-cost models, to show sensitivity to distributional assumptions.
4. **Alternative exposure algorithm:** a stricter claims definition than D3 (for example, requiring an inpatient claim), to address misclassification.
5. **Matching variations:** a 1:1 ratio; exact matching on CMV status; under Option B, a caliper of 0.1 standard deviations. The matching method not chosen under D10 may also be run as a sensitivity analysis.
6. **Restriction to first transplant** recipients.
7. **Pre-2015 and 2015-or-later** diagnoses, to check stability across the ICD-9-CM to ICD-10-CM transition (October 2015).

## 10. Disclosure control

- No cell with a count under 11 is reported in any table, figure, or text, including flow diagrams and website pages. Complementary cells are suppressed where they would allow a small count to be derived.
- Figures that show individual-level data (for example, beeswarm or scatter plots) are not published.
- All outputs are reviewed against the USRDS data use agreement before release.

## 11. Limitations to address in reporting

- Claims-based diagnoses carry misclassification of both exposure and covariates.
- Costs are Medicare payments only; patient cost-sharing, other payers, and Part D are excluded unless Part D is added.
- Restricting to Medicare fee-for-service primary coverage limits generalizability to recipients with other primary payers or Medicare Advantage.
- Confounding by unmeasured immunosuppression intensity may remain despite the covariates in Section 6.2.
- Coverage requirements depend on eligibility rules that change over time, such as the end of Medicare entitlement 36 months after a successful transplant for patients without another qualifying reason.

## 12. Reporting map

| Checklist item | Topic | Plan section |
|---|---|---|
| STROBE 1 / RECORD 1.1–1.3 | Title, abstract, data type, region and period, linkage | 3 |
| STROBE 2–3 | Background, objectives | 1 |
| STROBE 4 | Study design | 2 |
| STROBE 5 | Setting, dates | 3 |
| STROBE 6 / RECORD 6.1–6.3 | Eligibility, selection codes and algorithms, validation, linkage | 4, 5 |
| STROBE 6b | Matching criteria and number of controls | 6 |
| STROBE 7 / RECORD 7.1 | Variables, with complete code lists | 5, 6.2, 7 |
| STROBE 8 | Data sources and measurement | 3, 7 |
| STROBE 9 | Bias | 5.4, 6, 9, 11 |
| STROBE 10 | Study size | 6.5, 8.1 |
| STROBE 11 | Quantitative variables | 6.2, 7 |
| STROBE 12 / RECORD 12.1–12.3 | Statistical methods, data access, cleaning, linkage | 3, 8 |
| STROBE 12d | Matching taken into account in analysis | 8.4 |
| STROBE 12e | Sensitivity analyses | 9 |
| STROBE 13 / RECORD 13.1 | Participants, flow diagram | 8.1 |
| STROBE 14–17 | Descriptive data, outcomes, main and other results | 8 |
| STROBE 18–21 | Discussion, limitations, interpretation, generalizability | 11 |
| RECORD 19.1 | Limitations of routinely collected data | 11 |
| STROBE 22 / RECORD 22.1 | Funding; access to protocol, code and supplementary material | 8.7 |
| TARGET | Target-trial emulation | 9, Appendix A |

## 13. Open decisions

| ID | Decision | Section |
|---|---|---|
| D1 | Eligible payer categories beyond Medicare fee-for-service primary for Parts A and B | 4.2 |
| D2 | Inclusion of kidney–pancreas and other multi-organ recipients | 4.3 |
| D3 | Claim algorithm for cryptococcosis: claim types, diagnosis positions, number of claims | 5.1 |
| D4 | Controls in more than one risk set, or at most once | 5.4 |
| D5 | Rejection-treatment lookback window; comorbidity index | 6.2 |
| D6 | Under Option B, hard calipers on age and calendar time in addition to the score | 6.4 |
| D7 | Price index for inflation adjustment | 7.2 |
| D8 | Death: end of cost accrual, or zero cost after death | 7.4 |
| D9 | Primary estimate as matched difference, or difference-in-differences | 8.4 |
| D10 | Matching method: direct covariate matching (Option A) or time-varying propensity score (Option B) | 2, 6 |

## Appendix A: Target-trial protocol

| Component | Target trial | Emulation |
|---|---|---|
| Eligibility | Adult kidney transplant recipients, no prior cryptococcosis, 12 months of Medicare fee-for-service primary Parts A and B coverage | Same criteria, applied at the start of each monthly trial |
| Treatment strategies | Develop cryptococcosis in month *t*, or not | Diagnosis in month *t* (exposed) versus no diagnosis in month *t* (comparison) |
| Assignment | Not randomizable; the trial is hypothetical | Exchangeability approximated by ATT weights from a propensity score with the covariates in Section 6.2 |
| Time zero | Start of month *t* | Start of each monthly trial; aligned with eligibility and exposure |
| Follow-up | 12 months, ending as in Section 7.4 | Same |
| Outcome | Medicare payments over months 0–11 | Same as Section 7 |
| Causal contrast | Effect in those who develop cryptococcosis (ATT) | Weighted difference pooled across trials |
| Analysis | — | Weighted model as in Section 8.4; robust standard errors clustered on patient, because people appear in several trials |

## References

- von Elm E, et al. The Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) statement. *PLoS Med* 2007.
- Benchimol EI, et al. The REporting of studies Conducted using Observational Routinely-collected health Data (RECORD) statement. *PLoS Med* 2015.
- Cashin AG, et al. Transparent reporting of observational studies emulating a target trial: the TARGET statement. *JAMA* and *BMJ* 2025.
- Lu B. Propensity score matching with time-dependent covariates. *Biometrics* 2005.
- Daw JR, Hatfield LA. Matching and regression to the mean in difference-in-differences analysis. *Health Serv Res* 2018.
- Austin PC. Optimal caliper widths for propensity-score matching. *Pharm Stat* 2011.
- United States Renal Data System. *Researcher's Guide to the USRDS Database*, 2025.
