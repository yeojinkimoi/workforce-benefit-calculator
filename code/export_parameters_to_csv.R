# export_parameters_to_csv.R
# ---------------------------------------------------------
# Exports all data frames from the 4 PRD RData parameter
# files into individual CSV files for inspection and
# validation. Run this in RStudio.
#
# Output goes to: validation/csv_parameters/
# ---------------------------------------------------------

library(dplyr)
library(readr)

current_directory <- getwd()
output_dir <- file.path(current_directory, "validation", "csv_parameters")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define the 4 RData files and their prefixes
rdata_files <- list(
  list(
    path = file.path(current_directory, "policy-rules-database", "prd_parameters", "benefit.parameters.rdata"),
    prefix = "benefit_parameters"
  ),
  list(
    path = file.path(current_directory, "policy-rules-database", "prd_parameters", "tables.rdata"),
    prefix = "tables"
  ),
  list(
    path = file.path(current_directory, "policy-rules-database", "prd_parameters", "expenses.rdata"),
    prefix = "expenses"
  ),
  list(
    path = file.path(current_directory, "policy-rules-database", "prd_parameters", "parameters.defaults.rdata"),
    prefix = "parameters_defaults"
  )
)

cat("=== Exporting PRD parameters to CSV ===\n\n")

total_exported <- 0

for (rdata in rdata_files) {
  cat(sprintf("--- Loading: %s ---\n", basename(rdata$path)))

  if (!file.exists(rdata$path)) {
    cat(sprintf("  WARNING: File not found, skipping.\n\n"))
    next
  }

  # Load into a clean environment to avoid cross-contamination
  env <- new.env()
  load(rdata$path, envir = env)

  obj_names <- ls(envir = env)
  cat(sprintf("  Found %d objects: %s\n", length(obj_names), paste(obj_names, collapse = ", ")))

  for (obj_name in obj_names) {
    obj <- get(obj_name, envir = env)

    if (is.data.frame(obj)) {
      csv_name <- sprintf("%s_%s.csv", rdata$prefix, obj_name)
      csv_path <- file.path(output_dir, csv_name)

      write_csv(obj, csv_path)

      cat(sprintf("  -> %s (%d rows x %d cols)\n", csv_name, nrow(obj), ncol(obj)))
      cat(sprintf("     Columns: %s\n", paste(colnames(obj), collapse = ", ")))
      total_exported <- total_exported + 1

    } else if (is.list(obj) || is.vector(obj)) {
      # For non-dataframe objects, try to coerce or save as single-column CSV
      csv_name <- sprintf("%s_%s.csv", rdata$prefix, obj_name)
      csv_path <- file.path(output_dir, csv_name)

      tryCatch({
        df <- as.data.frame(obj)
        write_csv(df, csv_path)
        cat(sprintf("  -> %s (coerced to df: %d rows x %d cols)\n", csv_name, nrow(df), ncol(df)))
        total_exported <- total_exported + 1
      }, error = function(e) {
        cat(sprintf("  SKIPPED %s (class: %s, cannot coerce to data frame)\n", obj_name, class(obj)[1]))
      })

    } else {
      cat(sprintf("  SKIPPED %s (class: %s)\n", obj_name, class(obj)[1]))
    }
  }
  cat("\n")
}

cat(sprintf("=== Done. Exported %d files to %s ===\n", total_exported, output_dir))
