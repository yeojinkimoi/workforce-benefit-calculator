# CLAUDE.md

## Project Overview

**Workforce Training Fiscal Calculator** — a Shiny-based tool that estimates federal and state government fiscal gains from workforce training programs. Built for the Opportunity Insights (OI) team and intended for researchers, policymakers, and external stakeholders.

**Status:** Early prototype. Core logic works end-to-end. The app supports all 50 states + DC via dynamic dropdowns, but **funding shares currently use UT 2024 defaults for every state** (see TODO). Parameter validation needs to be done for all state x year.

**How it works:** The calculator uses the Policy Rules Database (PRD) — an R engine by the Atlanta Fed that computes eligibility and benefit values for U.S. public assistance programs and taxes. We run PRD twice (at pre-program earnings, then at post-program earnings after adding the treatment effect) and compute the fiscal impact as the change in government costs and revenues.

**Program-agnostic:** Designed to work with any workforce training program, not tied to a specific one.

**Deployment:** Hosted on [Posit Connect](https://connect.posit.cloud/).

## Conceptual Model

```
Pre-program earnings -> PRD -> Benefits & taxes at baseline
Post-program earnings (pre + treatment effect) -> PRD -> Benefits & taxes after training
Fiscal Gain = (Benefit spending reduction) + (Tax revenue increase)
Split by state/federal using program-specific funding shares
```

## Two Modes

### Simple Mode
Manual input via dynamic dropdowns and numeric fields:
- State (dropdown, all 50 + DC), county (dropdown, updates reactively based on state)
- Household type (single / single parent + 1 kid / two parents + 1 kid)
- Average pre-program earnings, average treatment effect on earnings
- Number of participants, rule year
- Results are per-participant gains scaled by N

### Dataset Mode
Upload a participant-level CSV for heterogeneous microsimulation:
- Each row is a participant with demographics and pre-program income
- Treatment effect applied uniformly (for now; heterogeneous effects planned)
- State/county come from each row's `locations` column — the full `fundingSharesData` (all states) is passed for the join
- See **Dataset CSV Schema** below for required columns

## Key Outputs
- **Fiscal gains** by government level (state vs federal) — total and per-participant
- **Component-level breakdown** — gains by program (SNAP, TANF, Medicaid, EITC, CTC, etc.)
- **Benefit payments** pre vs post (spending reduction)
- **Tax revenue** pre vs post (revenue increase)
- **Average Marginal Tax Rate (AMTR)** — tax gain / earnings change
- Visualizations of payments by government and component

## Key Directories and Files

### Our code (`functions_oi/`)
- `run_prd_for_input_functions.R` — `compute_fiscal_effect_simple()`: builds a representative household, calls PRD, scales by N participants
- `run_prd_for_df_functions.R` — `compute_fiscal_effect_df()` and `run_prd_for_df()`: runs PRD on participant-level dataframes, computes pre/post fiscal effects, applies funding shares. Funding shares join is by `(component, stateAbbrev)` — no county dimension.
- `nice_table_functions.R` — formatting utilities (`nice_table()`, `fmt_money()`, `OI_COLORS`)

### Scripts (`code/`)
- `create_funding_shares.R` — One-time script to generate `policy-rules-database/prd_parameters/funding.shares.rdata`. Cross-joins UT 2024 default shares with all 51 state abbreviations from `table.countypop`. Re-run after updating shares for states x year
- `export_parameters_to_csv.R` — Exports all data frames from PRD RData files to `validation/csv_parameters/` for inspection
- `fix_parameter.R` — One-off parameter fixes (e.g., correcting `UT_Phaseout`)

### App
- `app.R` — Shiny dashboard with simple and dataset modes, visualizations, and assumptions tab. State/county are dynamic `selectInput` dropdowns populated from `table.countypop`. County list updates reactively when state changes.

### PRD engine (`policy-rules-database/`) (do not modify without care)
The PRD is a nested subdirectory containing the Atlanta Fed's Policy Rules Database code and parameters.
- `policy-rules-database/functions/benefits_functions.R` — Individual program calculations (SNAP, Medicaid, EITC, CTC, SSI, SSDI, WIC, ACA, Section 8, etc.)
- `policy-rules-database/functions/BenefitsCalculator_functions.R` — High-level block aggregators and data creation
- `policy-rules-database/functions/TANF.R` — TANF calculations (state-specific, ~4800 lines)
- `policy-rules-database/functions/CCDF.R` — Child Care Development Fund calculations (~6200 lines)
- `policy-rules-database/functions/expense_functions.R` — ALICE cost-of-living budget assignment
- `policy-rules-database/libraries.R` — PRD library/package loading
- `policy-rules-database/projects/` — YAML project configs (e.g., `UT_single_parent.yaml`)
- `policy-rules-database/output/` — PRD CSV output files

### Parameters (`policy-rules-database/prd_parameters/`)

All program rules are stored in RData files. Each `.rdata` file contains data frames loaded into `prd_env` and then `attach()`ed onto the search path. **These are the numbers we must validate state-by-state.**

#### `benefit.parameters.rdata` — Program-specific rules

#### `funding.shares.rdata` — State/federal cost shares

Contains `fundingSharesData`: a tibble with columns `stateAbbrev`, `component`, `state_share`, `federal_share`, `ruleYear`. Currently 765 rows (51 states × 15 components). **All states currently use UT 2024 defaults — real per-state shares need to be populated** (see TODO).

Generated by `code/create_funding_shares.R`. The 15 components are: `medicaid_adult`, `medicaid_child`, `snap`, `aca`, `ccdf`, `wic`, `eitc_fed`, `eitc_state`, `ctc_fed`, `ctc_state`, `cdctc_fed`, `cdctc_state`, `tax_income_fed`, `tax_income_state`, `tanf`.

#### `tables.rdata` — Lookup/crosswalk tables
- `table.countypop` — County names, FIPS codes, population, state abbreviations (used to populate app dropdowns)
- `table.FPL` — Federal Poverty Line by year and family size
- `table.SMI` — State Median Income by state, year, family size
- `table.stateabbrevs` — State name to abbreviation mapping

#### `expenses.rdata` — ALICE cost-of-living defaults
- Default expense values by state/county and household composition
- Categories: `exp.food`, `exp.rentormortgage`, `exp.childcare`, `exp.healthcare`, `exp.utilities`, `exp.transportation`

#### `parameters.defaults.rdata` — General computation defaults
- School days/year, summer days, work hour assumptions, standard deductions

#### Parameter Issue Workflow

When a PRD parameter error is discovered, follow these steps in order:

1. **Log the issue** — Append to `validation/parameter_change_log.txt` with date, parameter, old/new values, source, and explanation. The log is append-only (historical record). Use this template:
   ```
   ----------------------------------------------------------------------
   Date:       YYYY-MM-DD
   Parameter:  <param_name> in <data_frame> (<scope, e.g. all UT rows>)
   Old value:  <wrong value>
   New value:  <correct value>
   File:       <rdata filename>
   Source:     <authoritative reference>
   Why:        <what broke, how it was detected>
   ----------------------------------------------------------------------
   ```

2. **Add a fix block to `code/fix_parameter.R`** — Add a new numbered section following the existing pattern (isolated env, check before, apply, verify). Include the date in a code comment. Fixes are cumulative and idempotent. Template:
   ```r
   # ============================================================
   # Fix N: <param> — <old> -> <new>  (YYYY-MM-DD)
   #
   # <brief explanation of the error and its impact>
   # ============================================================

   df <- prd_env$<dataFrame>
   before <- df %>% filter(<scope>) %>% pull(<param>) %>% unique()
   cat("<param> before:", before, "\n")

   df <- df %>% mutate(<param> = if_else(<scope>, <new_value>, <param>))

   after <- df %>% filter(<scope>) %>% pull(<param>) %>% unique()
   cat("<param> after: ", after, "(verified)\n")
   prd_env$<dataFrame> <- df
   ```

3. **Re-run the fix script** — `source("code/fix_parameter.R")`
4. **Re-export CSVs** — `source("code/export_parameters_to_csv.R")` to update `validation/csv_parameters/`
5. **Re-run validation** — `source("code/validate_parameters.R"); run_all_validations("STATE")` to regenerate plots
6. **Verify visually** — Check `validation/plots/` (especially AMTR curves) to confirm the anomaly is gone

#### Exporting parameters to CSV for validation
Run `code/export_parameters_to_csv.R` in RStudio to export all data frames to `validation/csv_parameters/`. This makes the binary RData contents readable for inspection and cross-referencing.

### Testing
- We should add a testing script. For example, UT_Phaseout in the state income tax parameters was mistakenly coded as 1.3 instead of 0.013. That typo makes the credit phaseout ~100× too fast, creating an artificial discrete jump in computed state tax and inflating the implied AMTR—especially when pre-tax income is $0 (e.g., moving from ~$11k to ~$18k). We caught this by computing AMTR by income and plotting it, so this can be one validation check for state and federal income tax parameters. We should build similar validation checks for other parameters too.

## Validation Process Plan

### Step 1: Validate Input Numbers (PRD Parameters)
**Goal:** Ensure every parameter in the RData files is correct for the target state and rule year.

**Process for each state** (start with Utah, stateFIPS=49):
1. Run `export_parameters_to_csv.R` to extract all data frames to CSVs
2. Filter each CSV to the target state and inspect every value:
   - `stateinctaxData` — tax brackets, rates, deductions, state-specific fields (e.g. `UT_Phaseout`)
   - `snapData` — max/min benefits, standard deductions, gross income limits by family size
   - `tanfData` — max benefit, standard of need, asset tests, earned income disregard
   - `fedeitcData` / `stateeitcData` — phase-in/out rates, max credit, income thresholds
   - `fedctcData` — CTC amounts, phaseout thresholds, refundability
   - `medicaidExpData` — income thresholds as % FPL by household type
   - `acaData` — Second Lowest Cost Silver Plan premiums, income bins (county-level)
   - `ssiData`, `ssdiData`, `wicData`, `section8Data`, `liheapData`, `ccdfData`
3. Cross-reference **every** threshold, rate, and dollar amount against authoritative sources:
   - IRS publications (EITC, CTC, federal tax brackets)
   - State tax agency websites (state income tax rates, brackets)
   - USDA (SNAP parameters)
   - CMS/MACPAC (Medicaid thresholds, FMAP rates)
   - ACF (TANF, CCDF parameters)
   - Healthcare.gov / state exchange (ACA Silver plan premiums)
4. Document discrepancies and fix
5. Known issue found and fixed: `UT_Phaseout` was 1.3 instead of 0.013

### Step 2: Validate Output Reasonableness (Tax/Benefit Calculations)
**Goal:** Verify that for known income levels and household types, PRD produces reasonable benefit amounts and tax liabilities.

**Process:**
1. Define test scenarios with known expected outputs:
   - Single adult, no kids, UT, income = $0 / $15,000 / $30,000 / $50,000 / $75,000
   - Single parent + 1 kid, UT, same income ladder
   - Married + 1 kid, UT, same income ladder
2. Run PRD for each scenario and check:
   - **SNAP:** Does eligibility phase out at the right income? Is max benefit correct for family size?
   - **EITC:** Does the credit follow expected phase-in / plateau / phase-out shape? Is max credit correct?
   - **CTC:** Is the credit amount correct per child? Does phaseout start at right threshold?
   - **State income tax:** Does the effective tax rate make sense? Are brackets applied correctly?
   - **Federal income tax:** Standard deduction applied? Brackets correct?
   - **Medicaid:** Does eligibility cut off at the right FPL percentage?
   - **TANF:** Does benefit amount match expected levels for family size?
3. Compare against hand-calculated expected values or known external calculators (e.g., Tax Foundation, CBPP benefit calculators)
4. Check that the **fiscal gain direction makes sense**: as earnings increase, benefits should decrease and taxes should increase
5. Check edge cases: $0 income, very high income, eligibility cliffs

## TODO / Outstanding Work

### High Priority
- **Populate real funding shares per state.** `fundingSharesData` currently uses UT 2024 defaults for all 51 states. Each state needs its own:
  - **FMAP rate** — varies by state, updated annually by CMS (affects `medicaid_adult`, `medicaid_child`)
  - **CCDF matching rate** — varies by state (affects `ccdf`)
  - **TANF federal grant vs state MOE split** — varies by state (affects `tanf`)
  - Update `code/create_funding_shares.R` with real values, or build a lookup from authoritative sources, then re-run the script.
- **Multi-state parameter validation** — PRD parameters (`benefit.parameters.rdata`) have only been validated for Utah. Each new state needs Step 1 + Step 2 validation before results can be trusted.

### Medium Priority
- **Funding shares by ruleYear.** The `ruleYear` column exists in `fundingSharesData` but is hardcoded to 2024. FMAP rates change annually — support multiple years.
- **Data pipeline and testing** — Input validation, test coverage, CI/CD improvements.

### Future Roadmap
- Heterogeneous treatment effects (varying by subgroup)
- Time-varying treatment effects (decay/growth over time)
- Multi-year projections with NPV calculations
- Expanded component list (as PRD supports more programs)

## Coding Conventions

- **Always plan before coding.** Discuss approach before writing code. Never make changes without approval.
- **Prefer `data.table`** for new code where performance matters.
- **Match existing R style:** dot-notation for names (e.g., `function.snapBenefit()`, `value.snap`, `exp.childcare`).
- **Naming patterns:**
  - Benefit values: `value.snap`, `value.medicaid.adult`, `value.eitc.fed`
  - Expenses: `exp.childcare`, `exp.food`; net: `netexp.childcare`
  - Program switches: `APPLY_SNAP`, `APPLY_MEDICAID_ADULT`
- **Keep PRD engine code stable.** Modify `policy-rules-database/functions/` files cautiously — they are the upstream PRD engine. Our customizations live in `functions_oi/`.
- **Upstream compatibility:** This repo was cloned from the Atlanta Fed PRD. Architecture should stay flexible to incorporate upstream rule updates when PRD parameters change.
- **Funding shares are state-level, not county-level.** The join in `compute_fiscal_effect_df()` is by `(component, stateAbbrev)` only. Do not add county to the funding shares schema.

## Components Currently Tracked

Benefits: SNAP, TANF, Medicaid (adult & child), ACA, CCDF, WIC
Tax credits: EITC (fed/state), CTC (fed/state), CDCTC (fed/state)
Taxes: Federal income tax, state income tax

Goal is to expand to the full set of programs PRD supports as we validate each state.

## How to Add a New Component to the Fiscal Calculator

When adding a new benefit or tax component (e.g., we added CDCTC this way):

### Step 1: Verify PRD computes it
Check that the PRD engine already calculates the value. Look in `run_prd_for_df()` output select (in `functions_oi/run_prd_for_df_functions.R`) — the variable should already be in the select list (e.g., `value.cdctc.fed`, `value.cdctc.state`). If not, the PRD engine may need a new `APPLY_*` switch turned on in `app.R`.

### Step 2: Add to `long_raw` in `compute_fiscal_effect_df()`
In `functions_oi/run_prd_for_df_functions.R`, add the new value columns to the `dplyr::select()` inside the `long_raw` creation (where pre/post results are pivoted to long format). This is the central data pipeline — everything downstream (gains, payments, summaries) flows from `long_raw`.

### Step 3: Add label mappings
In the same function, add entries to both `component_label` (grouped label, e.g., "CDCTC") and `component_label2` (gov-specific label, e.g., "CDCTC (Fed)" / "CDCTC (State)") `case_when` blocks.

### Step 4: Add funding shares
In `code/create_funding_shares.R`, add the new component(s) to `base_shares` with the correct state/federal split (e.g., `cdctc_fed` is 100% federal, `cdctc_state` is 100% state). Then re-run the script to regenerate `policy-rules-database/prd_parameters/funding.shares.rdata`.

### Step 5: Update CLAUDE.md
- Update the component count and list under `funding.shares.rdata`
- Update the "Components Currently Tracked" section

### Notes
- The component name in `long_raw` is derived automatically: `value.cdctc.fed` → `cdctc_fed` (strip `value.`, replace `.` with `_`, lowercase). This name must match the `component` column in `fundingSharesData` for the funding shares join to work.
- The `type` column ("benefit" or "tax") is assigned in the `case_when` — only `tax_income_fed` and `tax_income_state` are type "tax"; everything else (including tax credits) is type "benefit".
- No changes needed in `app.R` unless a new `APPLY_*` switch needs to be turned on.
- No changes needed in `run_prd_for_df()` output select unless the PRD variable isn't already selected there.

## PRD Engine Reference (minimal)

PRD computes benefits in sequential blocks — each block's output feeds into the next:
```
createData -> ALICEExpenses -> OtherBenefits (TANF,SSI,SSDI) -> Childcare (CCDF,HeadStart,PreK) -> Healthcare (Medicaid,ACA) -> FoodandHousing (SNAP,WIC,Section8,LIHEAP) -> TaxesandTaxCredits (EITC,CTC,CDCTC,income taxes) -> createVars
```
Block order matters. Supports up to 12 family members, state/county-level parameters, multi-year rules.
