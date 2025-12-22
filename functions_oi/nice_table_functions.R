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
