#-----------------------------
# Function to create data from dataframe
#-----------------------------
function.createData.from.DF <- function(data, ruleYear) {
  
  # 1. Add ruleYear and Year (data year)
  data <- data %>%
    mutate(
      ruleYear = ruleYear,
      Year     = ruleYear
    )
  
  # 2. Split locations:
  #    "Salt Lake County, UT" -> countyortownName + stateAbbrev
  data <- data %>%
    separate(
      col    = locations,
      into   = c("countyortownName", "stateAbbrev"),
      sep    = ", "
    )
  
  # 3. Income lag for tax credits 
  data <- data %>%
    mutate(income_tm12 = income)
  
  # 4. Core PRD transforms (joins, numkids/numadults, income1–income6, etc.)
  data <- function.InitialTransformations(data)
  
  # 5. Filing status and uninsured flag
  data <- data %>%
    mutate(
      FilingStatus = case_when(
        married == 0 & numkids == 0 ~ 1L,  # Single
        married == 1                ~ 2L,  # Married filing jointly
        married == 0 & numkids > 0  ~ 3L,  # Head of household
        TRUE                        ~ 1L
      ),
      uninsured = case_when(
        empl_healthcare == 0 ~ 1L,
        empl_healthcare == 1 ~ 0L,
        TRUE                 ~ 0L
      )
    )
  
  data <- as.data.frame(data)
  
  return(data)
}

#-----------------------------
# Run PRD for any dataframe
#-----------------------------
run_prd_for_df <- function(df, ruleYear = 2024, data_name = NULL) {
  
  # 1. Create PRD-ready data
  data <- function.createData.from.DF(df, ruleYear)
  
  # 2. Attach ALICE expenses
  data <- BenefitsCalculator.ALICEExpenses(data)
  
  # 3. Apply benefits blocks
  data <- BenefitsCalculator.OtherBenefits(data, APPLY_TANF, APPLY_SSI, APPLY_SSDI)
  data <- BenefitsCalculator.Childcare(
    data,
    APPLY_CHILDCARE,
    APPLY_HEADSTART,
    APPLY_PREK,
    APPLY_CCDF,
    APPLY_FATES
  )
  data <- BenefitsCalculator.Healthcare(
    data,
    APPLY_HEALTHCARE,
    APPLY_MEDICAID_ADULT,
    APPLY_MEDICAID_CHILD,
    APPLY_ACA
  )
  data <- BenefitsCalculator.FoodandHousing(
    data,
    APPLY_SECTION8,
    APPLY_LIHEAP,
    APPLY_SNAP,
    APPLY_SLP,
    APPLY_WIC,
    APPLY_RAP,
    APPLY_FRSP
  )
  data <- BenefitsCalculator.TaxesandTaxCredits(
    data,
    APPLY_EITC,
    APPLY_CTC,
    APPLY_CDCTC
  )
  
  # 4. Extra vars
  data <- function.createVars(data)
  
  # 5. Select output variables
  data2 <- data %>%
    dplyr::select(
      id, ruleYear, stateFIPS, stateName, stateAbbrev, countyortownName,
      famsize, numadults, numkids,
      agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6,
      agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12,
      empl_healthcare,
      income, assets.cash,
      exp.childcare, exp.food, exp.rentormortgage, exp.healthcare, exp.utilities,
      exp.misc, exp.transportation,
      netexp.childcare, netexp.food, netexp.rentormortgage,
      netexp.healthcare, netexp.utilities,
      value.snap, value.schoolmeals, value.section8, value.liheap,
      value.medicaid.adult, value.medicaid.child, value.aca, value.employerhealthcare,
      value.CCDF, value.HeadStart, value.PreK,
      value.cdctc.fed, value.cdctc.state,
      value.ctc.fed,  value.ctc.state,
      value.eitc.fed, value.eitc.state,
      value.eitc, value.ctc, value.cdctc, value.ssdi, value.ssi, value.tanf,
      value.wic,
      AfterTaxIncome, NetResources,
      tax.income.fed, tax.income.state
    )
  
  # 6. Optional: save
  if (!is.null(data_name)) {
    write.csv(
      data2,
      file = paste0(current_directory, "/output/results_", data_name, ".csv"),
      row.names = FALSE
    )
  }
  
  return(data2)
}


#-----------------------------------------
# Compute state/federal gains for avg TE
#-----------------------------------------
#-----------------------------------------
# Compute state/federal gains for avg TE
#-----------------------------------------
compute_fiscal_effect_df <- function(df_pre,
                                     avg_te,
                                     ruleYear       = 2024,
                                     data_name      = NULL,
                                     funding_shares) {
  
  # number of participants
  N <- nrow(df_pre)
  
  
  
  # 1. PRD at pre earnings ----------------------------------------------
  pre_res <- run_prd_for_df(
    df        = df_pre,
    ruleYear  = ruleYear,
    data_name = if (!is.null(data_name)) paste0(data_name, "_pre") else NULL
  )
  
  # 2. Create post-earnings DF and run PRD -------------------------------
  df_post <- df_pre %>%
    dplyr::mutate(income = income + avg_te)
  
  post_res <- run_prd_for_df(
    df        = df_post,
    ruleYear  = ruleYear,
    data_name = if (!is.null(data_name)) paste0(data_name, "_post") else NULL
  )
  
  # 3. Long format: pre vs post, one row per id × component --------------
  long_raw <- dplyr::bind_rows(
    pre  = pre_res,
    post = post_res,
    .id  = "period"
  ) %>%
    dplyr::select(
      id,
      period,
      stateAbbrev, countyortownName,
      # benefits
      value.snap,
      value.tanf,
      value.medicaid.adult,
      value.medicaid.child,
      value.aca,
      value.CCDF,
      value.wic,
      # credits
      value.eitc.fed, value.eitc.state,
      value.ctc.fed,  value.ctc.state,
      # taxes
      tax.income.fed, tax.income.state
    ) %>%
    tidyr::pivot_longer(
      cols      = -c(id, period, stateAbbrev, countyortownName),
      names_to  = "component",
      values_to = "dollar"
    ) %>%
    dplyr::mutate(
      component = stringr::str_remove(component, "^value\\."),
      component = stringr::str_replace_all(component, "\\.", "_"),
      component = tolower(component),
      type      = dplyr::case_when(
        component %in% c("tax_income_fed", "tax_income_state") ~ "tax",
        TRUE                                                    ~ "benefit"
      ),
      component_label = dplyr::case_when(
        component == "snap"             ~ "SNAP",
        component == "tanf"             ~ "TANF",
        component == "medicaid_adult"   ~ "Medicaid (adult)",
        component == "medicaid_child"   ~ "Medicaid (child)",
        component == "aca"              ~ "ACA subsidies",
        component == "ccdf"             ~ "CCDF child care",
        component == "wic"              ~ "WIC",
        component == "eitc_fed"         ~ "EITC",
        component == "eitc_state"       ~ "EITC",
        component == "ctc_fed"          ~ "CTC",
        component == "ctc_state"        ~ "CTC",
        component == "tax_income_fed"   ~ "Income tax",
        component == "tax_income_state" ~ "Income tax",
        TRUE ~ stringr::str_to_title(stringr::str_replace_all(component, "_", " "))
      ),
      component_label2 = dplyr::case_when(
        component == "snap"             ~ "SNAP",
        component == "tanf"             ~ "TANF",
        component == "medicaid_adult"   ~ "Medicaid (adult)",
        component == "medicaid_child"   ~ "Medicaid (child)",
        component == "aca"              ~ "ACA subsidies",
        component == "ccdf"             ~ "CCDF child care",
        component == "wic"              ~ "WIC",
        component == "eitc_fed"         ~ "EITC (Fed)",
        component == "eitc_state"       ~ "EITC (State)",
        component == "ctc_fed"          ~ "CTC (Fed)",
        component == "ctc_state"        ~ "CTC (State)",
        component == "tax_income_fed"   ~ "Income tax (Fed)",
        component == "tax_income_state" ~ "Income tax (State)",
        TRUE ~ stringr::str_to_title(stringr::str_replace_all(component, "_", " "))
      )
    )
  
  # 4. Attach funding shares once, derive $ payments ---------------------
  long_raw <- long_raw %>%
    dplyr::left_join(
      funding_shares,
      by = c("component", "stateAbbrev", "countyortownName")
    ) %>%
    dplyr::mutate(
      state_payment   = dollar * state_share,
      federal_payment = dollar * federal_share
    )
  
  # 5. Gains (pre → post) by component & government ----------------------
  #    - First aggregate to pre/post by component
  delta_by_component <- long_raw %>%
    dplyr::group_by(
      period, component, component_label, type,
      stateAbbrev, countyortownName, state_share, federal_share
    ) %>%
    dplyr::summarise(
      dollar = sum(dollar, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      id_cols     = c(component, component_label, type,
                      stateAbbrev, countyortownName,
                      state_share, federal_share),
      names_from  = period,
      values_from = dollar
    ) %>%
    dplyr::mutate(
      diff = dplyr::case_when(
        type == "benefit" ~ pre - post,  # benefits shrink → savings
        type == "tax"     ~ post - pre,  # taxes rise     → revenue
        TRUE              ~ NA_real_
      ),
      state_gain   = diff * state_share,
      federal_gain = diff * federal_share
    )
  
  # 6. Total gains by government ----------------------------------------
  delta_by_government <- delta_by_component %>%
    dplyr::summarise(
      state_gain_sum   = sum(state_gain,   na.rm = TRUE),
      federal_gain_sum = sum(federal_gain, na.rm = TRUE)
    )
  
  # 7. Level payments (pre/post) by component & government ---------------
  #    This is the object you were calling payment_by_component_gov
  payment_by_component_government <- long_raw %>%
    dplyr::group_by(
      period, type, component, component_label
    ) %>%
    dplyr::summarise(
      state_payment_total   = sum(state_payment,   na.rm = TRUE),
      federal_payment_total = sum(federal_payment, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 8. Total level payments by government (pre/post) ---------------------
  payment_by_government <- payment_by_component_government %>%
    tidyr::pivot_longer(
      cols      = c(state_payment_total, federal_payment_total),
      names_to  = "gov_type",
      values_to = "dollar"
    ) %>%
    dplyr::mutate(
      gov_type = dplyr::case_when(
        gov_type == "state_payment_total"   ~ "state",
        gov_type == "federal_payment_total" ~ "federal",
        TRUE                                ~ gov_type
      )
    ) %>%
    dplyr::group_by(period, gov_type) %>%
    dplyr::summarise(
      total_payment = sum(dollar, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 9. Return all building blocks ----------------------------------------
  list(
    
    n_participants = N,
    
    # id × component long file, already with shares & payments
    long_raw                      = long_raw,
    
    # net gains (reductions in benefits + increases in taxes)
    delta_by_component            = delta_by_component,
    delta_by_government           = delta_by_government,
    
    # level payments (pre/post) by component & gov
    payment_by_component_government = payment_by_component_government,
    
    # total level payments (pre/post) by gov
    payment_by_government         = payment_by_government
    
    # # raw PRD outputs in case you want to inspect
    # pre_res                       = pre_res,
    # post_res                      = post_res
  )
}
