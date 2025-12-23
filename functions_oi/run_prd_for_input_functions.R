#-----------------------------
# Function to create data from inputs
#-----------------------------

build_simple_df_for_prd <- function(state_abbrev,
                                    county_name,
                                    avg_pre,
                                    avg_age,
                                    hh_scenario = c("single", "single_parent_1kid", "two_parent_1kid"),
                                    child_age = 4L,
                                    spouse_age = 25L) {
  
  hh_scenario <- match.arg(hh_scenario)
  
  # Scenario -> household structure
  married <- if (hh_scenario == "two_parent_1kid") 1L else 0L
  numkids <- if (hh_scenario == "single") 0L else 1L
  
  # Put spouse in agePerson2, child in agePerson3 (keeps your existing schema intact)
  agePerson1 <- as.integer(avg_age)
  agePerson2 <- if (married == 1L) as.integer(spouse_age) else NA_integer_
  agePerson3 <- if (numkids == 1L) as.integer(child_age)  else NA_integer_
  
  tibble::tibble(
    id        = 1L,
    locations = paste0(county_name, ", ", state_abbrev),
    
    married   = married,
    numkids   = numkids,
    
    agePerson1  = agePerson1,
    agePerson2  = agePerson2,
    agePerson3  = agePerson3,
    agePerson4  = NA_integer_,
    agePerson5  = NA_integer_,
    agePerson6  = NA_integer_,
    agePerson7  = NA_integer_,
    agePerson8  = NA_integer_,
    agePerson9  = NA_integer_,
    agePerson10 = NA_integer_,
    agePerson11 = NA_integer_,
    agePerson12 = NA_integer_,
    
    # Disability / SSI / blindness – assume none
    disability1 = 0L,
    disability2 = 0L,
    disability3 = 0L,
    disability4 = 0L,
    disability5 = 0L,
    disability6 = 0L,
    disability7 = 0L,
    disability8 = 0L,
    disability9 = 0L,
    disability10 = 0L,
    disability11 = 0L,
    disability12 = 0L,
    prev_ssi    = 0L,
    
    blind1 = 0L,
    blind2 = 0L,
    blind3 = 0L,
    blind4 = 0L,
    blind5 = 0L,
    blind6 = 0L,
    
    ssdiPIA1 = 0,
    ssdiPIA2 = 0,
    ssdiPIA3 = 0,
    ssdiPIA4 = 0,
    ssdiPIA5 = 0,
    ssdiPIA6 = 0,
    
    # Employer health insurance – start with 0 (no coverage)
    empl_healthcare = 0L,
    
    # Earnings – pre-program income
    income = avg_pre,
    
    # Other income sources
    income.investment    = 0,
    income.gift          = 0,
    income.child_support = 0,
    
    # Housing, assets
    ownorrent    = "rent",
    assets.cash  = 0,
    assets.car1  = 0,
    
    # Work disability expenses
    disab.work.exp = 0
  )
}


#-----------------------------------------
# Compute state/federal gains for avg TE – SIMPLE MODE
#-----------------------------------------
compute_fiscal_effect_simple <- function(state_abbrev,
                                         county_name,
                                         avg_pre,
                                         avg_te,
                                         avg_age,
                                         n_participants,
                                         ruleYear       = 2024,
                                         funding_shares,
                                         hh_scenario    = "single",
                                         child_age      = 4L,
                                         spouse_age     = 25L) {
  
  df_pre_one <- build_simple_df_for_prd(
    state_abbrev = state_abbrev,
    county_name  = county_name,
    avg_pre      = avg_pre,
    avg_age      = avg_age,
    hh_scenario  = hh_scenario,
    child_age    = child_age,
    spouse_age   = spouse_age
  )
  
  
  # 2) Use dataset-based pipeline for that DF
  res_one <- compute_fiscal_effect_df(
    df_pre         = df_pre_one,
    avg_te         = avg_te,
    ruleYear       = ruleYear,
    data_name      = NULL,         # do not save CSVs by default in app
    funding_shares = funding_shares
  )
  
  long_raw                      <- res_one$long_raw
  delta_by_component            <- res_one$delta_by_component
  delta_by_government           <- res_one$delta_by_government
  payment_by_component_government <- res_one$payment_by_component_government
  payment_by_government         <- res_one$payment_by_government
  
  # 3) Scale EVERYTHING to totals (so simple mode matches dataset mode)
  
  long_raw <- res_one$long_raw %>%
    mutate(
      dollar         = dollar * n_participants,
      state_payment  = state_payment * n_participants,
      federal_payment= federal_payment * n_participants
    )
  
  # scale delta_by_component (pre/post/diff/gains)
  delta_by_component <- res_one$delta_by_component %>%
    mutate(
      pre          = pre * n_participants,
      post         = post * n_participants,
      diff         = diff * n_participants,
      state_gain   = state_gain * n_participants,
      federal_gain = federal_gain * n_participants
    )
  
  # scale payment_by_component_government (level payments)
  payment_by_component_government <- res_one$payment_by_component_government %>%
    mutate(
      state_payment_total   = state_payment_total * n_participants,
      federal_payment_total = federal_payment_total * n_participants
    )
  
  # scale payment_by_government (overall payments)
  payment_by_government <- res_one$payment_by_government %>%
    mutate(total_payment = total_payment * n_participants)
  
  # scale delta_by_government (totals)
  delta_by_government <- res_one$delta_by_government %>%
    mutate(
      state_total   = state_gain_sum   * n_participants,
      federal_total = federal_gain_sum * n_participants
    )
  
  
  list(
    n_participants                = n_participants,
    delta_by_component            = delta_by_component,
    delta_by_government           = delta_by_government,
    long_raw                      = res_one$long_raw,   # optional; still per-person unless you scale it too
    payment_by_component_government = payment_by_component_government,
    payment_by_government         = payment_by_government
  )
  
}
