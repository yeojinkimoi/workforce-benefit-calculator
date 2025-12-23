library(dplyr)
library(tidyr)
library(tibble)

# ============================================================
# Helpers
# ============================================================
rtruncnorm_simple <- function(n, mean, sd, lo, hi) {
  out <- numeric(0)
  while (length(out) < n) {
    x <- rnorm(n, mean, sd)
    x <- x[x >= lo & x <= hi]
    out <- c(out, x)
  }
  out[1:n]
}

rtrunc_lognorm_1 <- function(m = 49500, sdlog = 0.55, lo = 30000, hi = 200000) {
  mu <- log(m) - 0.5 * sdlog^2
  x <- rlnorm(1, meanlog = mu, sdlog = sdlog)
  while (x < lo || x > hi) x <- rlnorm(1, meanlog = mu, sdlog = sdlog)
  x
}

# ============================================================
# Main generator: keeps PACE MARGINS fixed, changes within-bin shapes
# ============================================================
make_fake_yearup_pace_simple <- function(n = 1000,
                                         scenario = c("high_need", "low_need"),
                                         seed = 123) {
  scenario <- match.arg(scenario)
  set.seed(seed)
  
  # ------------------------------------------------------------
  # 1) Age distribution (PACE Exhibit 3-2)
  # ------------------------------------------------------------
  age_group <- sample(
    c("18_20", "21_24"),
    size = n, replace = TRUE,
    prob = c(0.428, 0.572)
  )
  
  agePerson1 <- ifelse(
    age_group == "18_20",
    sample(18:20, size = n, replace = TRUE),
    sample(21:24, size = n, replace = TRUE)
  )
  
  # ------------------------------------------------------------
  # 2) Living arrangements (PACE Exhibit 3-2)
  # ------------------------------------------------------------
  living_arr <- sample(
    c("no_spouse_no_kids", "no_spouse_with_kids", "spouse_no_kids", "spouse_with_kids"),
    size = n, replace = TRUE,
    prob = c(0.866, 0.065, 0.045, 0.024)
  )
  
  has_spouse <- living_arr %in% c("spouse_no_kids", "spouse_with_kids")
  has_child  <- living_arr %in% c("no_spouse_with_kids", "spouse_with_kids")
  
  married <- as.integer(has_spouse)
  
  # ------------------------------------------------------------
  # 3) Household ages (PRD slots)
  # ------------------------------------------------------------
  spouse_age_draw <- function(a) as.integer(pmin(pmax(a + round(rnorm(1, 0, 2)), 18), 40))
  
  # Child age distribution differs by extreme
  if (scenario == "high_need") {
    # very young kids (max childcare / Medicaid-child relevance)
    child_ages <- 0:4
  } else {
    # older kids (lower childcare intensity)
    child_ages <- 8:17
  }
  child_wts <- rev(seq_along(child_ages))
  
  agePerson2 <- rep(NA_integer_, n)
  agePerson3 <- rep(NA_integer_, n)
  
  idx_spouse <- which(has_spouse)
  if (length(idx_spouse) > 0) {
    agePerson2[idx_spouse] <- vapply(agePerson1[idx_spouse], spouse_age_draw, integer(1))
  }
  
  idx_child_only <- which(has_child & !has_spouse)
  idx_child_both <- which(has_child & has_spouse)
  
  if (length(idx_child_only) > 0) {
    agePerson2[idx_child_only] <- sample(child_ages, length(idx_child_only),
                                         replace = TRUE, prob = child_wts)
  }
  if (length(idx_child_both) > 0) {
    agePerson3[idx_child_both] <- sample(child_ages, length(idx_child_both),
                                         replace = TRUE, prob = child_wts)
  }
  
  # keep schema stable
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
  # 4) Income bins (PACE Exhibit 3-2) — SAME BIN SHARES, different shapes
  # ------------------------------------------------------------
  inc_bin <- sample(
    c("lt15k", "15to30k", "ge30k"),
    size = n, replace = TRUE,
    prob = c(0.371, 0.257, 0.372)
  )
  
  income <- numeric(n)
  
  # scenario-specific within-bin targets
  if (scenario == "high_need") {
    # push within-bin toward LOW end
    lt15_mean <- 5500;  lt15_sd <- 3000
    mid_mean  <- 17500; mid_sd  <- 2500
    ge_m      <- 40000; ge_sdlog <- 0.55; ge_hi <- 150000
    
    empl_healthcare <- 0L
    assets_cash <- 0
    assets_car1 <- 0
  } else {
    # push within-bin toward HIGH end
    lt15_mean <- 13500; lt15_sd <- 1200
    mid_mean  <- 28500; mid_sd  <- 1200
    ge_m      <- 55000; ge_sdlog <- 0.55; ge_hi <- 200000
    
    empl_healthcare <- rbinom(n, 1, 0.30)  # simple “some coverage”
    assets_cash <- round(runif(n, 1000, 5000))
    assets_car1 <- round(runif(n, 5000, 15000))
  }
  
  n1 <- sum(inc_bin == "lt15k")
  n2 <- sum(inc_bin == "15to30k")
  
  income[inc_bin == "lt15k"] <- rtruncnorm_simple(n1, mean = lt15_mean, sd = lt15_sd, lo = 0, hi = 15000)
  income[inc_bin == "15to30k"] <- rtruncnorm_simple(n2, mean = mid_mean, sd = mid_sd, lo = 15000, hi = 30000)
  
  idx_ge30 <- which(inc_bin == "ge30k")
  if (length(idx_ge30) > 0) {
    income[idx_ge30] <- vapply(
      seq_along(idx_ge30),
      function(i) rtrunc_lognorm_1(m = ge_m, sdlog = ge_sdlog, lo = 30000, hi = ge_hi),
      numeric(1)
    )
  }
  
  income <- round(income)
  
  # ------------------------------------------------------------
  # 5) Build PRD-style dataset (same schema as yours)
  # ------------------------------------------------------------
  tibble(
    id        = 1:n,
    scenario  = scenario,
    income    = income,
    locations = "Salt Lake County, UT",
    
    agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6,
    agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12,
    
    married = married,
    
    living_arr = living_arr,
    inc_bin    = inc_bin,
    
    disability1 = 0L, disability2 = 0L, disability3 = 0L, disability4 = 0L,
    disability5 = 0L, disability6 = 0L, disability7 = 0L, disability8 = 0L,
    disability9 = 0L, disability10 = 0L, disability11 = 0L, disability12 = 0L,
    prev_ssi = 0L,
    blind1 = 0L, blind2 = 0L, blind3 = 0L, blind4 = 0L, blind5 = 0L, blind6 = 0L,
    
    ssdiPIA1 = 0, ssdiPIA2 = 0, ssdiPIA3 = 0,
    ssdiPIA4 = 0, ssdiPIA5 = 0, ssdiPIA6 = 0,
    
    empl_healthcare = as.integer(empl_healthcare),
    
    ownorrent   = "rent",
    assets.cash = assets_cash,
    assets.car1 = assets_car1,
    
    income.investment    = 0,
    income.gift          = 0,
    income.child_support = 0,
    
    disab.work.exp = 0
  )
}

# ============================================================
# Create the two extremes
# ============================================================
fake_yearup_highneed <- make_fake_yearup_pace_simple(n = 1000, scenario = "high_need", seed = 123)
fake_yearup_lowneed  <- make_fake_yearup_pace_simple(n = 1000, scenario = "low_need",  seed = 456)

# Quick check
bind_rows(fake_yearup_highneed, fake_yearup_lowneed) %>%
  group_by(scenario) %>%
  summarize(
    share_18_20 = mean(agePerson1 %in% 18:20),
    share_21_24 = mean(agePerson1 %in% 21:24),
    share_no_spouse_no_kids   = mean(living_arr == "no_spouse_no_kids"),
    share_no_spouse_with_kids = mean(living_arr == "no_spouse_with_kids"),
    share_spouse_no_kids      = mean(living_arr == "spouse_no_kids"),
    share_spouse_with_kids    = mean(living_arr == "spouse_with_kids"),
    share_lt15k   = mean(inc_bin == "lt15k"),
    share_15to30k = mean(inc_bin == "15to30k"),
    share_ge30k   = mean(inc_bin == "ge30k"),
    mean_income   = mean(income),
    share_empl_ins = mean(empl_healthcare == 1),
    share_child_any = mean(!is.na(agePerson3) | (!is.na(agePerson2) & living_arr == "no_spouse_with_kids"))
  )

# Save
dir.create("data", showWarnings = FALSE)
write.csv(fake_yearup_highneed, file.path("data", "fake_yearup_1000_pace_high_need.csv"), row.names = FALSE)
write.csv(fake_yearup_lowneed,  file.path("data", "fake_yearup_1000_pace_low_need.csv"),  row.names = FALSE)
