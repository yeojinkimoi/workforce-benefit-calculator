library(dplyr)
library(tidyr)
library(tibble)

set.seed(123)

n <- 1000

# Targets from Year Up PACE Exhibit 3-2 (All Participants)
# Age: 18–20 (42.8%), 21–24 (57.2%)
# Living arrangements:
#   - Not living w spouse/partner or children: 86.6
#   - Not living w spouse/partner, living w children: 6.5
#   - Living w spouse/partner, not living w children: 4.5
#   - Living w spouse/partner and children: 2.4
# Family income last year bins:
#   - <15k: 37.1, 15–30k: 25.7, 30k+: 37.2
# Mean family income: 27,021

# ------------------------------------------------------------
# 1) Age distribution (match Exhibit 3-2)
# ------------------------------------------------------------
age_group <- sample(
  c("18_20", "21_24"),
  size = n,
  replace = TRUE,
  prob = c(0.428, 0.572)
)

agePerson1 <- ifelse(
  age_group == "18_20",
  sample(18:20, size = n, replace = TRUE),
  sample(21:24, size = n, replace = TRUE)
)

# ------------------------------------------------------------
# 2) Living arrangements (match Exhibit 3-2)
# ------------------------------------------------------------
living_arr <- sample(
  c("no_spouse_no_kids", "no_spouse_with_kids", "spouse_no_kids", "spouse_with_kids"),
  size = n,
  replace = TRUE,
  prob = c(0.866, 0.065, 0.045, 0.024)
)

has_spouse <- living_arr %in% c("spouse_no_kids", "spouse_with_kids")
has_child  <- living_arr %in% c("no_spouse_with_kids", "spouse_with_kids")

# PRD "married" is a proxy for living with spouse/partner
married <- as.integer(has_spouse)

# ------------------------------------------------------------
# 3) Fill household ages into PRD slots
# ------------------------------------------------------------
spouse_age_draw <- function(a) as.integer(pmin(pmax(a + round(rnorm(1, 0, 2)), 18), 35))

# Child age not given -> impute (skew young)
child_ages <- 0:10
child_wts  <- rev(seq_along(child_ages))

agePerson2 <- rep(NA_integer_, n)
agePerson3 <- rep(NA_integer_, n)

# spouse present -> agePerson2 is spouse
idx_spouse <- which(has_spouse)
agePerson2[idx_spouse] <- vapply(agePerson1[idx_spouse], spouse_age_draw, integer(1))

# child present:
idx_child_only <- which(has_child & !has_spouse)
idx_child_both <- which(has_child & has_spouse)

agePerson2[idx_child_only] <- sample(child_ages, length(idx_child_only), replace = TRUE, prob = child_wts)
agePerson3[idx_child_both] <- sample(child_ages, length(idx_child_both), replace = TRUE, prob = child_wts)

# Everyone else NA (keep schema stable for PRD)
agePerson4  <- NA_integer_
agePerson5  <- NA_integer_
agePerson6  <- NA_integer_
agePerson7  <- NA_integer_
agePerson8  <- NA_integer_
agePerson9  <- NA_integer_
agePerson10 <- NA_integer_
agePerson11 <- NA_integer_
agePerson12 <- NA_integer_

# ------------------------------------------------------------
# 4) Income distribution (match Exhibit 3-2 bins; more realistic within bins)
#    - <15k and 15-30k: truncated normal (peaked, not uniform)
#    - 30k+: truncated lognormal (right-skew)
# ------------------------------------------------------------

# Simple truncated-normal sampler (base R only)
rtruncnorm_simple <- function(n, mean, sd, lo, hi) {
  out <- numeric(0)
  while (length(out) < n) {
    x <- rnorm(n, mean, sd)
    x <- x[x >= lo & x <= hi]
    out <- c(out, x)
  }
  out[1:n]
}

inc_bin <- sample(
  c("lt15k", "15to30k", "ge30k"),
  size = n,
  replace = TRUE,
  prob = c(0.371, 0.257, 0.372)
)

income <- numeric(n)

# <15k: centered around ~9k with spread, truncated to [0, 15000]
n1 <- sum(inc_bin == "lt15k")
income[inc_bin == "lt15k"] <- rtruncnorm_simple(
  n1, mean = 9000, sd = 4000, lo = 0, hi = 15000
)

# 15-30k: centered around ~22k, truncated to [15000, 30000]
n2 <- sum(inc_bin == "15to30k")
income[inc_bin == "15to30k"] <- rtruncnorm_simple(
  n2, mean = 22000, sd = 3500, lo = 15000, hi = 30000
)

# 30k+: truncated lognormal (right-skew), tuned for overall mean ~27,021
draw_ge30k <- function(m = 49500, sdlog = 0.55, lo = 30000, hi = 200000) {
  mu <- log(m) - 0.5 * sdlog^2
  x <- rlnorm(1, meanlog = mu, sdlog = sdlog)
  while (x < lo || x > hi) x <- rlnorm(1, meanlog = mu, sdlog = sdlog)
  x
}
idx_ge30 <- which(inc_bin == "ge30k")
income[idx_ge30] <- vapply(seq_along(idx_ge30), function(i) draw_ge30k(), numeric(1))

income <- round(income)

# ------------------------------------------------------------
# 5) Build the fake YearUp dataset in PRD style
# ------------------------------------------------------------
fake_yearup <- tibble(
  id        = 1:n,
  income    = income,
  locations = "Salt Lake County, UT",
  
  # ages
  agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6,
  agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12,
  
  married = married,
  
  # keep for diagnostics (drop if PRD doesn't want them)
  living_arr = living_arr,
  inc_bin    = inc_bin,
  
  # disability / blindness / SSI flags: all zero for now
  disability1 = 0L, disability2 = 0L, disability3 = 0L, disability4 = 0L,
  disability5 = 0L, disability6 = 0L, disability7 = 0L, disability8 = 0L,
  disability9 = 0L, disability10 = 0L, disability11 = 0L, disability12 = 0L,
  prev_ssi = 0L,
  blind1 = 0L, blind2 = 0L, blind3 = 0L, blind4 = 0L, blind5 = 0L, blind6 = 0L,
  
  # SSDI PIA: zero for everyone
  ssdiPIA1 = 0, ssdiPIA2 = 0, ssdiPIA3 = 0,
  ssdiPIA4 = 0, ssdiPIA5 = 0, ssdiPIA6 = 0,
  
  # employer health insurance: assume none for now
  empl_healthcare = 0L,
  
  # housing tenure + assets
  ownorrent   = "rent",
  assets.cash = 0,
  assets.car1 = 0,
  
  # other income sources: zero for now
  income.investment    = 0,
  income.gift          = 0,
  income.child_support = 0,
  
  # work-related disability expenses
  disab.work.exp = 0
)

# ------------------------------------------------------------
# 6) Sanity checks vs report targets
# ------------------------------------------------------------
fake_yearup %>%
  summarize(
    share_18_20 = mean(agePerson1 %in% 18:20),
    share_21_24 = mean(agePerson1 %in% 21:24),
    
    share_no_spouse_no_kids    = mean(living_arr == "no_spouse_no_kids"),
    share_no_spouse_with_kids  = mean(living_arr == "no_spouse_with_kids"),
    share_spouse_no_kids       = mean(living_arr == "spouse_no_kids"),
    share_spouse_with_kids     = mean(living_arr == "spouse_with_kids"),
    
    share_lt15k   = mean(inc_bin == "lt15k"),
    share_15to30k = mean(inc_bin == "15to30k"),
    share_ge30k   = mean(inc_bin == "ge30k"),
    mean_income   = mean(income),
    
    share_child_any  = mean(!is.na(agePerson3) | (!is.na(agePerson2) & !has_spouse)),
    share_spouse_any = mean(married == 1)
  )

dir.create("data", showWarnings = FALSE)
write.csv(fake_yearup, file.path("data", "fake_yearup_1000_pace_like.csv"), row.names = FALSE)
