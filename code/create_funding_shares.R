# create_funding_shares.R
# Generates prd_parameters/funding.shares.rdata with state-by-year funding shares.
# Run from project root: source("code/create_funding_shares.R")
#
# UT and TX have real 2024 funding shares; both years use their 2024 values.
# All other states use UT 2024 defaults until real shares are added.

library(dplyr)

# Load tables.rdata to get all state abbreviations
load("policy-rules-database/prd_parameters/tables.rdata")
all_states <- sort(unique(table.countypop$stateAbbrev))
rule_years <- c(2024L)

# ---------------------------------------------------------------------------
# State-specific parameters (2024 values, used for all years)
#   fmap       = Medicaid FMAP (federal share of Medicaid costs)
#   ccdf_match = CCDF federal matching rate (standard rate = FMAP)
#   tanf_fed   = TANF federal block grant ($M)
#   tanf_moe   = TANF state maintenance-of-effort ($M)
# ---------------------------------------------------------------------------
state_params <- tibble::tribble(
  ~stateAbbrev, ~fmap,  ~ccdf_match, ~tanf_fed, ~tanf_moe,

  # Utah (2024 values)
  "UT", 0.6590, 0.6590, 75.4,   24.9,

  # Texas (2024 values)
  "TX", 0.6015, 0.6015, 484.65, 388.37
)

# Default: UT 2024 values (used for all states not listed above)
default_params <- list(
  fmap       = 0.6590,
  ccdf_match = 0.6590,
  tanf_fed   = 75.4,
  tanf_moe   = 24.9
)

# ---------------------------------------------------------------------------
# Build the full grid: every state × every year
# States in state_params get their own values; others get UT 2024 defaults.
# Both years use the same 2024 values so results are year-independent.
# ---------------------------------------------------------------------------
full_grid <- tidyr::crossing(stateAbbrev = all_states, ruleYear = rule_years) %>%
  dplyr::left_join(state_params, by = "stateAbbrev") %>%
  dplyr::mutate(
    fmap       = dplyr::coalesce(fmap,       default_params$fmap),
    ccdf_match = dplyr::coalesce(ccdf_match, default_params$ccdf_match),
    tanf_fed   = dplyr::coalesce(tanf_fed,   default_params$tanf_fed),
    tanf_moe   = dplyr::coalesce(tanf_moe,   default_params$tanf_moe)
  )

# ---------------------------------------------------------------------------
# Build the 15 components from each row's parameters
# ---------------------------------------------------------------------------
build_shares <- function(fmap, ccdf_match, tanf_fed, tanf_moe) {
  tanf_total     <- tanf_fed + tanf_moe
  tanf_fed_share <- tanf_fed / tanf_total
  tanf_st_share  <- tanf_moe / tanf_total
  
  tibble::tribble(
    ~component,         ~state_share,        ~federal_share,
    "medicaid_adult",   1 - fmap,            fmap,
    "medicaid_child",   1 - fmap,            fmap,
    "snap",             0.00,                1.00,
    "aca",              0.00,                1.00,
    "ccdf",             1 - ccdf_match,      ccdf_match,
    "wic",              0.00,                1.00,
    "eitc_fed",         0.00,                1.00,
    "eitc_state",       1.00,                0.00,
    "ctc_fed",          0.00,                1.00,
    "ctc_state",        1.00,                0.00,
    "cdctc_fed",        0.00,                1.00,
    "cdctc_state",      1.00,                0.00,
    "tax_income_fed",   0.00,                1.00,
    "tax_income_state", 1.00,                0.00,
    "tanf",             tanf_st_share,       tanf_fed_share
  )
}

fundingSharesData <- full_grid %>%
  dplyr::rowwise() %>%
  dplyr::mutate(shares = list(build_shares(fmap, ccdf_match, tanf_fed, tanf_moe))) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(shares) %>%
  dplyr::select(stateAbbrev, component, state_share, federal_share, ruleYear)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
save(fundingSharesData, file = "policy-rules-database/prd_parameters/funding.shares.rdata")

cat("Created prd_parameters/funding.shares.rdata\n")
cat(sprintf("  %d states × %d components × %d years = %d rows\n",
            length(all_states), 15L, length(rule_years), nrow(fundingSharesData)))