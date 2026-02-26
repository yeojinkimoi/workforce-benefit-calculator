nice_table <- function(df, caption = NULL) {
  knitr::kable(df, format = "html", caption = caption, na = ".") %>%
    kableExtra::kable_styling(
      full_width = TRUE,
      bootstrap_options = c("striped", "hover")
    )
}

fmt_money <- function(x) scales::dollar(x, accuracy=1)
fmt_n <- function(x) scales::comma(x)
scale_money <- function(x, N, mode) {
  if (mode == "pp") x / N else x
}

#' The OI color palette
#' @export
OI_COLORS <- c(
  "#FAA523","#29B6A4", "#003A4F", "#7F4892", "#A4CE4E",
  "#2B8F43", "#0073A2", "#E54060", "#FFD400", "#6BBD45"
)

# Validate N (replaces repeated checks in app.R)
validate_N <- function(N) {
  shiny::validate(shiny::need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
}

# Rename type codes to display labels
rename_type <- function(x, style = c("spending", "savings")) {
  style <- match.arg(style)
  benefit_label <- if (style == "spending") "Benefit spending" else "Benefit savings"
  dplyr::case_when(x == "benefit" ~ benefit_label, x == "tax" ~ "Tax revenue", TRUE ~ x)
}

# Scale then format as dollars
fmt_money_scaled <- function(x, N, mode) {
  scales::dollar(scale_money(x, N, mode), accuracy = 1)
}
