library(dplyr)
library(tidyr)

set.seed(123)   # for reproducibility

n <- 1000
avg_te <- 7000

# 10% with a child
has_child <- rbinom(n, size = 1, prob = 0.10)

# Adult age ~ N(23, 3), truncated to [18, 40]
agePerson1 <- round(rnorm(n, mean = 23, sd = 3))
agePerson1 <- pmin(pmax(agePerson1, 18), 29)

# If has_child == 1, assign a child age 0–5; otherwise NA
agePerson2 <- ifelse(
  has_child == 1,
  sample(0:5, size = sum(has_child == 1), replace = TRUE),
  NA_integer_
)

# Everyone else NA
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

# Income ~ N(14000, 4000), truncated at 0
income <- round(rnorm(n, mean = 14000, sd = 4000))
income[income < 0] <- 0  # no negative earnings

# Build the fake YearUp dataset in PRD style
fake_yearup <- tibble(
  id         = 1:n,
  income     = income,
  locations  = "Salt Lake County, UT",   # we'll focus on this for now
  
  # ages
  agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6,
  agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12,
  
  # marital status: assume all not married for now
  married = 0L,
  
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

# Quick sanity checks
mean(fake_yearup$income)
mean(fake_yearup$agePerson1)
mean(has_child)             # should be ~0.10

write.csv(fake_yearup, file.path("data", "fake_yearup_1000_pre.csv"), row.names = FALSE)

fake_yearup <- fake_yearup %>% 
  mutate(income = income+avg_te)

write.csv(fake_yearup, file.path("data", "fake_yearup_1000_post.csv"), row.names = FALSE)
mean(fake_yearup$income)