# ============================================================
# validate_parameters.R
# PRD Parameter Validation System
#
# What it does:
#   Runs the actual PRD pipeline (same code path as app.R) across
#   an income grid for every household type and rule year, then
#   checks that the outputs and underlying parameters make sense.
#
# How to run:
#   source("code/validate_parameters.R")
#   run_all_validations()                             # all configured states (UT + TX for now)
#   run_all_validations("UT")                         # Utah only
#   run_all_validations("TX")                         # Texas only
#
# What it produces:
#   validation/plots/    — AMTR, benefits, tax, and net position curves (PNG)
#   validation/results/  — per-scenario CSV sweep data for inspection
#   validation/csv_parameters/ — all PRD parameter tables as CSV
#
# Sections:
#   1. Setup            — packages, PRD engine, parameters, switches
#   2. Helpers          — build_income_grid(), label maps, color maps
#   3. AMTR sweep       — income sweep with effective AMTR
#   4. Save CSV         — export sweep data for inspection
#   5. Save plots       — AMTR, benefits, taxes, net position 
#   6. Runner           — run_all_validations() ties it all together
# ============================================================


# =========================================================== #
#                      1. SETUP                                #
# =========================================================== #

# --- Load necessary packages ---
current_directory <- getwd()

source(file.path(current_directory, "libraries_oi.R"),                       local = TRUE)
source(file.path(current_directory, "policy-rules-database", "libraries.R"),                       local = TRUE)

# --- Source PRD engine ---
source(file.path(current_directory, "policy-rules-database", "functions", "benefits_functions.R"),  local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "expense_functions.R"),   local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "BenefitsCalculator_functions.R"), local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "TANF.R"),               local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "CCDF.R"),               local = TRUE)

# --- Source our functions ---
source(file.path(current_directory, "functions_oi/run_prd_for_df_functions.R"),    local = TRUE)
source(file.path(current_directory, "functions_oi/run_prd_for_input_functions.R"), local = TRUE)

# --- Load all PRD parameter files into a dedicated environment ---
prd_env <- new.env(parent = emptyenv())
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "expenses.rdata"),            envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "benefit.parameters.rdata"),  envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "tables.rdata"),              envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "parameters.defaults.rdata"), envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "funding.shares.rdata"),      envir = prd_env)

# Safe to re-source: detach previous attachment if it exists
if ("PRD_PARAMS_VALIDATE" %in% search()) detach("PRD_PARAMS_VALIDATE")
attach(prd_env, name = "PRD_PARAMS_VALIDATE", warn.conflicts = FALSE)

# --- PRD global switches (identical to app.R) ---
source(file.path(current_directory, "program_global_setting.R"),                       local = TRUE)

# --- Create output directories ---
dir.create(file.path(current_directory, "validation", "plots"),          recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(current_directory, "validation", "results"),        recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(current_directory, "validation", "csv_parameters"), recursive = TRUE, showWarnings = FALSE)

# --- States to validate ---
# Each row defines a state with its FIPS code.
# Add new rows here as more states are validated.
# County is auto-picked (most populous) via get_default_county().
VALIDATION_STATES <- tibble::tribble(
  ~state_abbrev, ~state_fips,
  "UT",          49L,
  "TX",          48L
)


# =========================================================== #
#                      2. HELPERS                              #
# =========================================================== #

# --- Auto-pick the most populous county for a state ---
#
# All validated programs use state-level parameters, so county
# choice doesn't affect results. We pick the largest county so
# adding a new state only requires state_abbrev + state_fips.
get_default_county <- function(state_abbrev) {
  table.countypop %>%
    filter(stateAbbrev == state_abbrev) %>%
    arrange(desc(countyortownpop)) %>%
    slice(1) %>%
    pull(countyortownName)
}

# --- Build a multi-row income grid for PRD ---
#
# Creates N copies of a single household template (via
# build_simple_df_for_prd), each with a different income.
# This is the input for run_prd_for_df() — one PRD call
# processes all N rows at once (vectorized).
build_income_grid <- function(state_abbrev, county_name, scenario,
                              income_values = seq(0, 100000, by = 1000),
                              avg_age    = 25L,
                              child_age  = 4L,
                              spouse_age = 25L) {

  template <- build_simple_df_for_prd(
    state_abbrev = state_abbrev,
    county_name  = county_name,
    avg_pre      = 0,
    avg_age      = avg_age,
    hh_scenario  = scenario,
    child_age    = child_age,
    spouse_age   = spouse_age
  )

  n  <- length(income_values)
  df <- template[rep(1, n), ]
  df$id     <- seq_len(n)
  df$income <- income_values
  df
}


# --- Column names used throughout ---
#
# These match the output of run_prd_for_df()
BENEFIT_COLS <- c(
  "snap", "tanf", "medicaid_adult", "medicaid_child",
  "aca", "ccdf", "wic",
  "eitc_fed", "eitc_state",
  "ctc_fed",  "ctc_state",
  "cdctc_fed", "cdctc_state"
)

# --- Human-readable component labels ---
COMPONENT_LABELS <- c(
  snap           = "SNAP",
  tanf           = "TANF",
  medicaid_adult = "Medicaid (Adult)",
  medicaid_child = "Medicaid (Child)",
  aca            = "ACA Subsidies",
  ccdf           = "CCDF",
  wic            = "WIC",
  eitc_fed       = "EITC (Fed)",
  eitc_state     = "EITC (State)",
  ctc_fed        = "CTC (Fed)",
  ctc_state      = "CTC (State)",
  cdctc_fed      = "CDCTC (Fed)",
  cdctc_state    = "CDCTC (State)",
  tax_fed        = "Federal Income Tax",
  tax_state      = "State Income Tax"
)

# --- Human-readable scenario labels ---
SCENARIO_LABELS <- c(
  single             = "Single Adult",
  single_parent_1kid = "Single Parent + 1 Child",
  two_parent_1kid    = "Two Parents + 1 Child"
)

# --- OI color assignments for benefit components ---
COMPONENT_COLORS <- c(
  "SNAP"              = OI_COLORS[1],
  "TANF"              = OI_COLORS[2],
  "Medicaid (Adult)"  = OI_COLORS[3],
  "Medicaid (Child)"  = OI_COLORS[7],
  "ACA Subsidies"     = OI_COLORS[4],
  "CCDF"              = OI_COLORS[5],
  "WIC"               = OI_COLORS[6],
  "EITC (Fed)"        = OI_COLORS[8],
  "EITC (State)"      = OI_COLORS[9],
  "CTC (Fed)"         = OI_COLORS[10],
  "CTC (State)"       = "#8B6914",
  "CDCTC (Fed)"       = "#4169E1",
  "CDCTC (State)"     = "#DC143C"
)


# =========================================================== #
#                   3. AMTR INCOME SWEEP                       #
#                                                              #
#  For each (scenario x year):                                 #
#    1. Build $0–$100k income grid (101 rows, $1000 steps)     #
#    2. Run PRD once on the whole grid (vectorized)            #
#    3. Compute discrete AMTR (tax-only + effective)           #
#    4. Return sweep tibble for plotting / CSV export          #
# =========================================================== #

run_amtr_sweep <- function(state_abbrev, state_fips, county_name,
                           scenario, rule_year,
                           income_step = 1000, income_max = 100000) {

  cat(sprintf("  AMTR sweep: %s, %s, year=%d\n", scenario, state_abbrev, rule_year))

  income_values <- seq(0, income_max, by = income_step)

  # ---- Run PRD on the full income grid ----
  df_grid <- build_income_grid(state_abbrev, county_name, scenario, income_values)

  prd_out <- tryCatch(
    run_prd_for_df(df_grid, ruleYear = rule_year),
    error = function(e) {
      cat(sprintf("    ERROR in PRD: %s\n", e$message))
      return(NULL)
    }
  )

  if (is.null(prd_out)) return(NULL)

  # ---- Extract benefit/tax components into a clean table ----
  sweep <- prd_out %>%
    transmute(
      id, income,
      # Benefits
      snap           = value.snap,
      tanf           = value.tanf,
      medicaid_adult = value.medicaid.adult,
      medicaid_child = value.medicaid.child,
      aca            = value.aca,
      ccdf           = value.CCDF,
      wic            = value.wic,
      # Tax credits (counted as benefits in our framework)
      eitc_fed       = value.eitc.fed,
      eitc_state     = value.eitc.state,
      ctc_fed        = value.ctc.fed,
      ctc_state      = value.ctc.state,
      cdctc_fed      = value.cdctc.fed,
      cdctc_state    = value.cdctc.state,
      # Taxes
      tax_fed        = tax.income.fed,
      tax_state      = tax.income.state
    ) %>%
    mutate(
      total_benefits   = rowSums(across(all_of(BENEFIT_COLS)), na.rm = TRUE),
      total_taxes      = rowSums(across(c(tax_fed, tax_state)), na.rm = TRUE),
      net_gov_position = total_taxes - total_benefits
    ) %>%
    arrange(income)

  # ---- Compute discrete AMTR ----
  #
  # Tax-only AMTR :
  #   amtr_fed   = (change in federal tax) / (change in income)
  #   amtr_state = (change in state tax)   / (change in income)
  #   amtr_total = (change in total tax)   / (change in income)
  #
  # Effective AMTR (the key fiscal diagnostic):
  #   amtr_effective = (tax increase + benefit reduction) / (change in income)
  #   Shows total fiscal gain per dollar of earnings — reveals benefit cliffs
  sweep <- sweep %>%
    mutate(
      d_income    = income - lag(income),
      d_tax_fed   = tax_fed - lag(tax_fed),
      d_tax_state = tax_state - lag(tax_state),
      d_benefits  = lag(total_benefits) - total_benefits,
      amtr_fed       = if_else(d_income > 0, d_tax_fed / d_income, NA_real_),
      amtr_state     = if_else(d_income > 0, d_tax_state / d_income, NA_real_),
      amtr_total     = if_else(d_income > 0, (d_tax_fed + d_tax_state) / d_income, NA_real_),
      amtr_effective = if_else(d_income > 0, (d_tax_fed + d_tax_state + d_benefits) / d_income, NA_real_)
    ) %>%
    select(-d_income, -d_tax_fed, -d_tax_state, -d_benefits)

  sweep
}


# =========================================================== #
#                   4. SAVE SWEEP CSV                          #
# =========================================================== #

save_sweep_csv <- function(sweep_data, state_abbrev, scenario, rule_year) {

  if (is.null(sweep_data)) return(invisible(NULL))

  results_dir <- file.path(current_directory, "validation", "results")
  tag         <- sprintf("%s_%s_%d", state_abbrev, scenario, rule_year)
  out_path    <- file.path(results_dir, paste0("sweep_", tag, ".csv"))

  write_csv(sweep_data, out_path)
  cat(sprintf("    Saved: sweep_%s.csv\n", tag))
}


# =========================================================== #
#                   5. SAVE SWEEP PLOTS                        #
#                                                              #
#  4 figures per (state, scenario, year), all OI-styled:       #
#    1. Effective AMTR                                         #
#    2. Benefits by component (stacked area)                   #
#    3. Tax liability (federal + state)                        #
#    4. Net government position                                #
# =========================================================== #

save_sweep_plots <- function(sweep_data, state_abbrev, scenario, rule_year) {

  if (is.null(sweep_data)) return(invisible(NULL))

  plot_dir       <- file.path(current_directory, "validation", "plots")
  tag            <- sprintf("%s_%s_%d", state_abbrev, scenario, rule_year)
  scenario_label <- SCENARIO_LABELS[scenario]
  if (is.na(scenario_label)) scenario_label <- scenario
  title_prefix   <- sprintf("%s, %s (%d)", state_abbrev, scenario_label, rule_year)

  # ---- Plot 1: AMTR (tax gain / earnings change) ----
  amtr_plot_data <- sweep_data %>%
    filter(!is.na(amtr_total)) %>%
    select(income, amtr_total, amtr_fed, amtr_state) %>%
    pivot_longer(-income, names_to = "level", values_to = "rate") %>%
    mutate(level = factor(level,
      levels = c("amtr_total", "amtr_fed", "amtr_state"),
      labels = c("Total", "Federal", "State")))

  p_amtr <- ggplot(amtr_plot_data, aes(x = income, y = rate * 100, color = level)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_x_continuous(labels = scales::dollar) +
    scale_color_manual(values = c(
      "Total"   = OI_COLORS[3],
      "Federal" = OI_COLORS[1],
      "State"   = OI_COLORS[2]
    )) +
    labs(
      title    = paste0("Average Marginal Tax Rate \u2014 ", title_prefix),
      x = "Annual Earnings ($)", y = "Avg Marginal Tax Rate (%)",
      color = NULL
    ) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, paste0("amtr_", tag, ".png")),
         p_amtr, width = 10, height = 6, dpi = 150)

  # ---- Plot 2: Benefits by component (stacked area) ----
  ben_long <- sweep_data %>%
    select(income, all_of(BENEFIT_COLS)) %>%
    pivot_longer(-income, names_to = "component", values_to = "value") %>%
    mutate(label = COMPONENT_LABELS[component])

  # Only show components that are non-zero somewhere
  visible <- ben_long %>%
    group_by(component) %>%
    summarise(total = sum(abs(value), na.rm = TRUE), .groups = "drop") %>%
    filter(total > 0) %>%
    pull(component)

  ben_visible <- ben_long %>% filter(component %in% visible)

  p_benefits <- ggplot(ben_visible,
                       aes(x = income, y = value, fill = label)) +
    geom_area(alpha = 0.6) +
    geom_line(data = sweep_data, aes(x = income, y = total_benefits),
              inherit.aes = FALSE, linewidth = 0.7, color = "black") +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_manual(values = COMPONENT_COLORS) +
    labs(
      title = paste0("Benefits by Component \u2014 ", title_prefix),
      x = "Annual Earnings ($)", y = "Annual Benefit ($)",
      fill = NULL
    ) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, paste0("benefits_", tag, ".png")),
         p_benefits, width = 10, height = 6, dpi = 150)

  # ---- Plot 3: Taxes (Federal + State) ----
  tax_long <- sweep_data %>%
    select(income, tax_fed, tax_state) %>%
    pivot_longer(-income, names_to = "component", values_to = "value") %>%
    mutate(label = COMPONENT_LABELS[component])

  p_taxes <- ggplot(tax_long, aes(x = income, y = value, color = label)) +
    geom_line() +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    scale_color_manual(values = c(
      "Federal Income Tax" = OI_COLORS[1],
      "State Income Tax"   = OI_COLORS[2]
    )) +
    labs(
      title = paste0("Tax Liability \u2014 ", title_prefix),
      x = "Annual Earnings ($)", y = "Annual Tax ($)",
      color = NULL
    ) +
    theme(legend.position = "bottom")
  ggsave(file.path(plot_dir, paste0("taxes_", tag, ".png")),
         p_taxes, width = 10, height = 6, dpi = 150)

  # ---- Plot 4: Net government position ----
  p_net <- ggplot(sweep_data, aes(x = income, y = net_gov_position)) +
    geom_ribbon(aes(ymin = pmin(net_gov_position, 0), ymax = 0),
                fill = OI_COLORS[2], alpha = 0.15) +
    geom_ribbon(aes(ymin = 0, ymax = pmax(net_gov_position, 0)),
                fill = OI_COLORS[1], alpha = 0.15) +
    geom_line(color = OI_COLORS[3], linewidth = 1.0) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title    = paste0("Net Government Position \u2014 ", title_prefix),
      x = "Annual Earnings ($)", y = "Net Position ($)"
    ) +
    theme(legend.position = "none")
  ggsave(file.path(plot_dir, paste0("net_position_", tag, ".png")),
         p_net, width = 10, height = 6, dpi = 150)

  cat(sprintf("    Saved plots: amtr_%s.png, benefits_%s.png, taxes_%s.png, net_position_%s.png\n",
              tag, tag, tag, tag))
}


# =========================================================== #
#                   6. RUNNER                                  #
#                                                              #
#  run_all_validations() runs the sweep for every scenario     #
#  and year, saves CSVs and plots for human inspection.        #
# =========================================================== #

run_all_validations <- function(states = NULL) {

  # Determine which states to validate
  if (is.null(states)) {
    # Default: all configured states
    state_config <- VALIDATION_STATES
  } else {
    # Filter to requested state(s)
    state_config <- VALIDATION_STATES %>% filter(state_abbrev %in% states)
    if (nrow(state_config) == 0) {
      stop(sprintf("State(s) '%s' not found in VALIDATION_STATES. Add them first.",
                   paste(states, collapse = ", ")))
    }
  }

  cat("============================================================\n")
  cat(sprintf("  PRD Validation Sweep: %s\n",
              paste(state_config$state_abbrev, collapse = ", ")))
  cat("============================================================\n\n")

  scenarios       <- c("single", "single_parent_1kid", "two_parent_1kid")
  files_generated <- character()

  # ---- Loop over each state ----
  for (row_i in seq_len(nrow(state_config))) {
    st_abbrev <- state_config$state_abbrev[row_i]
    st_fips   <- state_config$state_fips[row_i]
    st_county <- get_default_county(st_abbrev)

    cat(sprintf("\n************************************************************\n"))
    cat(sprintf("  State: %s (FIPS %d), County: %s\n", st_abbrev, st_fips, st_county))
    cat(sprintf("************************************************************\n"))

    # ---- Auto-detect available rule years ----
    available_years <- c()
    if (exists("stateinctaxData", envir = prd_env)) {
      sit <- get("stateinctaxData", envir = prd_env)
      available_years <- sit %>%
        filter(stateFIPS == st_fips) %>%
        pull(ruleYear) %>%
        unique() %>%
        sort()
    }
    if (length(available_years) == 0) available_years <- 2024
    cat(sprintf("  Available rule years: %s\n\n", paste(available_years, collapse = ", ")))

    # ---- Loop over each rule year and scenario ----
    for (yr in available_years) {
      cat(sprintf("\n=== %s — Rule Year: %d ===\n", st_abbrev, yr))

      for (sc in scenarios) {

        # 1. Run AMTR sweep
        sweep_data <- tryCatch(
          run_amtr_sweep(st_abbrev, st_fips, st_county, sc, yr),
          error = function(e) {
            cat(sprintf("    ERROR in sweep (%s): %s\n", sc, e$message))
            NULL
          }
        )

        # 2. Save CSV
        save_sweep_csv(sweep_data, st_abbrev, sc, yr)

        # 3. Save plots
        save_sweep_plots(sweep_data, st_abbrev, sc, yr)

        if (!is.null(sweep_data)) {
          tag <- sprintf("%s_%s_%d", st_abbrev, sc, yr)
          files_generated <- c(files_generated, tag)
        }
      }
    }
  }

  # ---- Export all parameter tables to CSV ----
  cat("\n--- Exporting parameters to CSV ---\n")
  csv_dir  <- file.path(current_directory, "validation", "csv_parameters")
  exported <- 0
  for (obj_name in ls(envir = prd_env)) {
    obj <- get(obj_name, envir = prd_env)
    if (is.data.frame(obj)) {
      tryCatch({
        write_csv(obj, file.path(csv_dir, paste0(obj_name, ".csv")))
        exported <- exported + 1
      }, error = function(e) NULL)
    }
  }
  cat(sprintf("  Exported %d parameter tables to %s\n", exported, csv_dir))

  # ---- Print summary ----
  cat("\n============================================================\n")
  cat("  SUMMARY\n")
  cat("============================================================\n")
  cat(sprintf("  States validated: %s\n",
              paste(state_config$state_abbrev, collapse = ", ")))
  cat(sprintf("  Sweep configurations completed: %d\n", length(files_generated)))
  for (f in files_generated) {
    cat(sprintf("    - %s\n", f))
  }
  cat(sprintf("\n  Output locations:\n"))
  cat(sprintf("    CSVs:   validation/results/sweep_*.csv\n"))
  cat(sprintf("    Plots:  validation/plots/*.png\n"))
  cat(sprintf("    Params: validation/csv_parameters/*.csv\n"))

  cat("\n============================================================\n")
  cat("  Done.\n")
  cat("============================================================\n")

  invisible(files_generated)
}
