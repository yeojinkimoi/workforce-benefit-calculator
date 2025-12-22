library(dplyr)
library(tidyr)
library(tibble)

set.seed(123)

n <- 1000

# ------------------------------------------------------------
# 1) Demographics with realistic correlations
# ------------------------------------------------------------

# Adult age: centered early 20s, truncated [18, 29]
agePerson1 <- round(rnorm(n, mean = 23, sd = 3))
agePerson1 <- pmin(pmax(agePerson1, 18), 29)

# Probability of having a child increases with age a bit
p_child <- plogis(-3.0 + 0.18 * (agePerson1 - 22))
has_child <- rbinom(n, size = 1, prob = p_child)

# Marriage probability depends on child status + age
p_married <- plogis(-3.2 + 1.6 * has_child + 0.15 * (agePerson1 - 22))
married <- rbinom(n, size = 1, prob = p_married)

# If has_child == 1, assign a child age (0–10) with more weight on younger kids; else NA
child_ages <- 0:10
child_wts  <- rev(seq_along(child_ages))
agePerson2 <- rep(NA_integer_, n)
agePerson2[has_child == 1] <- sample(
  child_ages,
  size = sum(has_child == 1),
  replace = TRUE,
  prob = child_wts
)

# Everyone else NA (keep schema stable for PRD)
agePerson3  <- NA_integer_
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
# 2) Income model with realistic correlations
#    - higher with age
#    - higher if married
#    - higher if has child (proxy for household contribution/support)
# ------------------------------------------------------------

mu_log <- log(14000) +
  0.04 * (agePerson1 - 23) +
  0.12 * married +
  0.10 * has_child

sigma_log <- 0.35
income <- round(rlnorm(n, meanlog = mu_log, sdlog = sigma_log))
income <- pmax(income, 0)
income <- pmin(income, 80000)  # optional cap to avoid extreme tails

# ------------------------------------------------------------
# 3) Build the fake YearUp dataset in PRD style
# ------------------------------------------------------------

fake_yearup <- tibble(
  id        = 1:n,
  income    = income,
  locations = "Salt Lake County, UT",
  
  # ages
  agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6,
  agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12,
  
  married = as.integer(married),
  
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

# Sanity checks
fake_yearup %>%
  summarize(
    mean_income = mean(income),
    mean_age    = mean(agePerson1),
    share_child = mean(!is.na(agePerson2)),
    share_married = mean(married == 1),
    married_given_child = mean(married[!is.na(agePerson2)] == 1),
    married_given_nochild = mean(married[is.na(agePerson2)] == 1)
  )

dir.create("data", showWarnings = FALSE)
write.csv(fake_yearup, file.path("data", "fake_yearup_1000_v2.csv"), row.names = FALSE)
