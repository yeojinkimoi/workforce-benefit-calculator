# fix_parameter.R
# Apply parameter corrections to PRD RData files.
# Run: source("code/fix_parameter.R")

library(dplyr)

current_directory <- getwd()
rdata_path <- file.path(current_directory, "policy-rules-database", "prd_parameters", "benefit.parameters.rdata")
log_path   <- file.path(current_directory, "validation", "parameter_change_log.txt")

# --- Load into isolated environment (not global) ---
prd_env <- new.env(parent = emptyenv())
load(rdata_path, envir = prd_env)

# ============================================================
# Fix 1: UT_Phaseout — 1.3 -> 0.013 (all rule years)
#
# Utah's taxpayer credit phases out at 1.3% per dollar of AGI
# above the threshold. The parameter was coded as 1.3 (130%)
# instead of 0.013 (1.3%) — a 100x error that creates a fake
# 58.7% marginal tax rate spike around $18k income.
# ============================================================

df <- prd_env$stateinctaxData

# Check before
before <- df %>% filter(stateName == "Utah") %>% pull(UT_Phaseout) %>% unique()
cat("UT_Phaseout before:", before, "\n")

# Apply fix
df <- df %>% mutate(UT_Phaseout = if_else(stateName == "Utah", 0.013, UT_Phaseout))

# Verify
after <- df %>% filter(stateName == "Utah") %>% pull(UT_Phaseout) %>% unique()
cat("UT_Phaseout after: ", after, "(verified)\n")

prd_env$stateinctaxData <- df

# ============================================================
# Save and log
# ============================================================

save(list = ls(envir = prd_env), file = rdata_path, envir = prd_env)
cat("Saved:", basename(rdata_path), "\n")

# Append to change log
log_entry <- sprintf(
  paste0(
    "----------------------------------------------------------------------\n",
    "Date:       %s\n",
    "Parameter:  UT_Phaseout in stateinctaxData (all UT rows, all rule years)\n",
    "Old value:  1.3\n",
    "New value:  0.013\n",
    "File:       benefit.parameters.rdata\n",
    "Source:     Utah State Tax Commission; Tax Foundation 2024 state tax data\n",
    "Why:        100x error. Utah taxpayer credit phases out at 1.3%% per dollar\n",
    "            above threshold, not 130%%. Caused the credit ($876 single / $1752\n",
    "            MFJ) to vanish over ~$674 instead of ~$67k. Created artificial\n",
    "            58.7%% marginal state tax spike at ~$18k. Detected via AMTR\n",
    "            validation sweep showing amtr_state jumping 0.25 -> 0.587.\n",
    "----------------------------------------------------------------------\n\n"
  ),
  format(Sys.time(), "%Y-%m-%d")
)
cat(log_entry, file = log_path, append = TRUE)
cat("Logged to:", basename(log_path), "\n")

cat("\nDone.")
