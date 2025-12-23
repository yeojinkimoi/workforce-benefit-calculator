# app.R  (optimized for ShinyApps memory)

# ----------------- PACKAGES -----------------
# Avoid pacman + tidyverse in production (big memory win)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)
library(tibble)
library(yaml)
library(oiplot)

set_oi_theme()

current_directory <- getwd()

# ----------------- HARD-LOCK GEO: UTAH -----------------
UT_STATE  <- "UT"
UT_COUNTY <- "Salt Lake County"   # change if needed
LOCK_COUNTY <- TRUE               # TRUE = UT + county; FALSE = UT only

# ----------------- CORE FUNCTIONS -----------------
source(file.path(current_directory, "libraries.R"), local = TRUE)
source(file.path(current_directory, "functions/benefits_functions.R"), local = TRUE)
source(file.path(current_directory, "functions/expense_functions.R"), local = TRUE)
source(file.path(current_directory, "functions/BenefitsCalculator_functions.R"), local = TRUE)
source(file.path(current_directory, "functions/TANF.R"), local = TRUE)
source(file.path(current_directory, "functions/CCDF.R"), local = TRUE)

source(file.path(current_directory, "functions_oi/nice_table_functions.R"), local = TRUE)
source(file.path(current_directory, "functions_oi/run_prd_for_df_functions.R"), local = TRUE)     # compute_fiscal_effect_df()
source(file.path(current_directory, "functions_oi/run_prd_for_input_functions.R"), local = TRUE)  # compute_fiscal_effect_simple()

# ----------------- PRD: LOAD ONCE AT STARTUP (NO SHRINKING) -----------------
prd_env <- new.env(parent = emptyenv())

load(file.path(current_directory, "prd_parameters/expenses.rdata"),            envir = prd_env)
load(file.path(current_directory, "prd_parameters/benefit.parameters.rdata"),  envir = prd_env)
load(file.path(current_directory, "prd_parameters/tables.rdata"),              envir = prd_env)
load(file.path(current_directory, "prd_parameters/parameters.defaults.rdata"), envir = prd_env)

# If PRD functions expect these objects to be on the search path:
attach(prd_env, name = "PRD_PARAMS", warn.conflicts = FALSE)

onStop(function() {
  try(detach("PRD_PARAMS"), silent = TRUE)
})

# ----------------- YAML SWITCHES (UT single parent) -----------------
PROJECT <- "UT_single_parent"
inputs  <- read_yaml(file.path(current_directory, "projects", paste0(PROJECT, ".yaml")))

k_ftorpt                <- inputs$k_ftorpt
schoolagesummercare     <- inputs$schoolagesummercare
headstart_ftorpt        <- inputs$headstart_ftorpt
preK_ftorpt             <- inputs$preK_ftorpt
contelig.headstart      <- inputs$contelig.headstart
contelig.earlyheadstart <- inputs$contelig.earlyheadstart
contelig.ccdf           <- inputs$contelig.ccdf
USEALICE                <- inputs$USEALICE
budget.ALICE            <- inputs$budget.ALICE

APPLY_CHILDCARE      <- inputs$APPLY_CHILDCARE
APPLY_CCDF           <- inputs$APPLY_CCDF
APPLY_HEADSTART      <- inputs$APPLY_HEADSTART
APPLY_PREK           <- inputs$APPLY_PREK
APPLY_LIHEAP         <- FALSE
APPLY_HEALTHCARE     <- inputs$APPLY_HEALTHCARE
APPLY_MEDICAID_ADULT <- inputs$APPLY_MEDICAID_ADULT
APPLY_MEDICAID_CHILD <- inputs$APPLY_MEDICAID_CHILD
APPLY_ACA            <- inputs$APPLY_ACA
APPLY_SECTION8       <- inputs$APPLY_SECTION8
APPLY_RAP            <- inputs$APPLY_RAP
APPLY_FRSP           <- inputs$APPLY_FRSP
APPLY_SNAP           <- inputs$APPLY_SNAP
APPLY_SLP            <- inputs$APPLY_SLP
APPLY_WIC            <- inputs$APPLY_WIC
APPLY_EITC           <- inputs$APPLY_EITC
APPLY_TAXES          <- inputs$APPLY_TAXES
APPLY_CTC            <- inputs$APPLY_CTC
APPLY_CDCTC          <- inputs$APPLY_CDCTC
APPLY_FATES          <- inputs$APPLY_FATES
APPLY_TANF           <- inputs$APPLY_TANF
APPLY_SSI            <- inputs$APPLY_SSI
APPLY_SSDI           <- inputs$APPLY_SSDI

# ----------------- FUNDING SHARES (UT / Salt Lake) -----------------
FMAP_UT_2024        <- 0.6590
CCDF_MATCH_UT_2024  <- 0.6840
TANF_FED_UT         <- 75.4
TANF_MOE_UT         <- 24.9
TANF_STATE_SHARE_UT <- TANF_MOE_UT / (TANF_FED_UT + TANF_MOE_UT)
TANF_FED_SHARE_UT   <- TANF_FED_UT / (TANF_FED_UT + TANF_MOE_UT)

funding_shares <- tibble::tribble(
  ~component,          ~state_share,                 ~federal_share,
  "medicaid_adult",    1 - FMAP_UT_2024,             FMAP_UT_2024,
  "medicaid_child",    1 - FMAP_UT_2024,             FMAP_UT_2024,
  "snap",              0.00,                         1.00,
  "aca",               0.00,                         1.00,
  "ccdf",              1 - CCDF_MATCH_UT_2024,       CCDF_MATCH_UT_2024,
  "wic",               0.00,                         1.00,
  "eitc_fed",          0.00,                         1.00,
  "eitc_state",        1.00,                         0.00,
  "ctc_fed",           0.00,                         1.00,
  "ctc_state",         1.00,                         0.00,
  "tax_income_fed",    0.00,                         1.00,
  "tax_income_state",  1.00,                         0.00,
  "tanf",              TANF_STATE_SHARE_UT,          TANF_FED_SHARE_UT
) %>%
  dplyr::mutate(
    stateAbbrev      = "UT",
    countyortownName = "Salt Lake County"
  )

# Best-effort shrink: filter any PRD data.frames that key on geography

# Best-effort shrink: filter any PRD data.frames that key on geography
shrink_prd_env_to_geo <- function(env, state_abbrev, county_name = NULL) {
  nms <- ls(env, all.names = TRUE)
  for (nm in nms) {
    obj <- get(nm, envir = env)
    if (is.data.frame(obj)) {
      cols <- names(obj)
      
      if (!is.null(county_name) && all(c("stateAbbrev", "countyortownName") %in% cols)) {
        obj2 <- dplyr::filter(obj, stateAbbrev == state_abbrev, countyortownName == county_name)
        if (nrow(obj2) > 0) obj <- obj2
      } else if ("stateAbbrev" %in% cols) {
        obj2 <- dplyr::filter(obj, stateAbbrev == state_abbrev)
        if (nrow(obj2) > 0) obj <- obj2
      }
      
      assign(nm, obj, envir = env)
    }
  }
  invisible(env)
}


shrink_prd_env_to_suffix <- function(env, suffix = "_UT", keep_also = character()) {
  nms <- ls(env, all.names = TRUE)
  
  # keep: anything ending in _UT, plus explicitly allowed "global" objects
  keep <- union(grep(paste0(suffix, "$"), nms, value = TRUE), keep_also)
  
  # drop everything else
  drop <- setdiff(nms, keep)
  if (length(drop) > 0) rm(list = drop, envir = env)
  
  invisible(env)
}

alias_suffix_objects <- function(env, suffix = "_UT") {
  nms <- ls(env, all.names = TRUE)
  ut  <- grep(paste0(suffix, "$"), nms, value = TRUE)
  
  for (nm_ut in ut) {
    nm_generic <- sub(paste0(suffix, "$"), "", nm_ut)
    # Only alias if generic doesn't exist (or you want to overwrite)
    assign(nm_generic, get(nm_ut, envir = env), envir = env)
  }
  invisible(env)
}

# ----------------- UI -----------------

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"
    ),
    tags$style(HTML("
      html, body,
      h1, h2, h3, h4, h5, h6,
      p, span, a, li,
      label, input, select, textarea, button,
      table, th, td,
      .nav, .nav-tabs, .navbar,
      .btn, .form-control, .shiny-input-container {
        font-family: 'Roboto', -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif !important;
      }

      h3 { font-size: 14px; margin-top: 15px; margin-bottom: 8px; color: #000000; }
      h4 { font-size: 14px; margin-top: 15px; margin-bottom: 8px; color: #000000; }

      .assumption-title { color: #FAA523; }

      .nav-tabs > li > a {
        font-size: 11px;
        padding: 5px 10px;
        background-color: #29B6A4 !important;
        color: #ffffff !important;
        margin-right: 4px;
      }

      .nav-tabs > li > a[data-value='Assumption'] { background-color: #FAA523 !important; }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #29B6A4 !important;
        color: #ffffff !important;
      }

      .nav-tabs > li.active > a[data-value='Assumption'],
      .nav-tabs > li.active > a[data-value='Assumption']:hover,
      .nav-tabs > li.active > a[data-value='Assumption']:focus {
        background-color: #FAA523 !important;
        color: #ffffff !important;
      }

      #run.btn-primary,
      #run.btn-primary:hover,
      #run.btn-primary:focus,
      #run.btn-primary:active {
        background-color: #29B6A4 !important;
        border-color: #29B6A4 !important;
        color: #ffffff !important;
      }

      input[type='radio']{
        -webkit-appearance: none;
        -moz-appearance: none;
        appearance: none;
        width: 14px;
        height: 14px;
        border: 2px solid #FAA523;
        border-radius: 50%;
        background: #fff;
        vertical-align: middle;
        margin-right: 6px;
        position: relative;
        top: -1px;
      }

      input[type='radio']:checked::before{
        content: '';
        display: block;
        width: 8px;
        height: 8px;
        border-radius: 50%;
        background: #FAA523;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }

      input[type='radio']:focus{
        outline: none;
        box-shadow: 0 0 0 3px rgba(250, 165, 35, 0.35);
      }
    "))
  ),
  
  titlePanel("Program Fiscal Impact Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "mode",
        "Select mode:",
        choices = c(
          "Simple calculator (no dataset)" = "simple",
          "Dataset mode (microsimulation)" = "dataset"
        ),
        selected = "simple"
      ),
      
      conditionalPanel(
        condition = "input.mode == 'simple'",
        textInput("state_simple", "State abbreviation", value = "UT"),
        textInput("county_simple", "County name", value = "Salt Lake County"),
        selectInput(
          "hh_scenario_simple",
          "Household scenario",
          choices = c(
            "1) Single (no kids)"        = "single",
            "2) One parent + one child"  = "single_parent_1kid",
            "3) Two parents + one child" = "two_parent_1kid"
          ),
          selected = "single"
        ),
        
        conditionalPanel(
          condition = "input.hh_scenario_simple != 'single'",
          numericInput("child_age_simple", "Child age", value = 4, min = 0, max = 17)
        ),
        
        conditionalPanel(
          condition = "input.hh_scenario_simple == 'two_parent_1kid'",
          numericInput("spouse_age_simple", "Spouse age", value = 25, min = 16, max = 80)
        ),
        numericInput("avg_pre_simple", "Average pre-program earnings", value = 18000, min = 0),
        numericInput("avg_te_simple", "Average treatment effect on earnings", value = 7000, min = 0),
        numericInput("avg_age_simple", "Average age", value = 25, min = 16, max = 80),
        numericInput("n_participants_simple", "Number of participants", value = 1000, min = 1),
        numericInput("rule_year_simple", "Rule year", value = 2024, min = 2000, max = 2100)
      ),
      
      conditionalPanel(
        condition = "input.mode == 'dataset'",
        fileInput(
          "data_file",
          "Upload participant-level CSV (one row per participant)",
          accept = c(".csv")
        ),
        helpText("Required columns: id, locations, income/earnings/wages, agePerson1–12, married, numkids, etc."),
        numericInput("avg_te_dataset", "Average treatment effect on earnings", value = 7000, min = 0),
        numericInput("rule_year_dataset", "Rule year", value = 2024, min = 2000, max = 2100)
      ),
      
      actionButton("run", "Run calculator", class = "btn-primary")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.has_run == true",
        
        tabsetPanel(
          tabPanel(
            "Overview",
            h3("Fiscal gains"),
            tableOutput("overview_gains"),
            br(),
            
            h3("Net gains by government, by source (benefits vs taxes)"),
            radioButtons(
              "scale_net", label = NULL,
              choices = c("Per participant" = "pp", "Total" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            tableOutput("gov_type_gain_table"),
            br(),
            
            h3("Benefit payments by government (pre vs post)"),
            radioButtons(
              "scale_benefit", label = NULL,
              choices = c("Per participant" = "pp", "Total" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            tableOutput("benefit_payments_table"),
            br(),
            
            h3("Tax revenue by government (pre vs post)"),
            radioButtons(
              "scale_tax", label = NULL,
              choices = c("Per participant" = "pp", "Total" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            tableOutput("tax_payments_table"),
            br(),
            
            h3("Average marginal tax rate (tax gain ÷ earnings change)"),
            tableOutput("amtr_table"),
            br(),
            
            h3("Payments by government and source (pre vs post)"),
            radioButtons(
              "plot_scale", label = NULL,
              choices = c("Per participant" = "pp", "Total (millions)" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            plotOutput("plot_overall_gov_type")
          ),
          
          tabPanel(
            "Component gains",
            h3("Component-level gains (revenue / savings)"),
            radioButtons(
              "scale_component_gains", label = NULL,
              choices = c("Per participant" = "pp", "Total" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            tableOutput("component_gains_table"),
            br(),
            
            h3("Gains by component and government"),
            radioButtons(
              "comp_plot_scale", label = NULL,
              choices = c("Per participant" = "pp", "Total (millions)" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            plotOutput("plot_component_gains")
          ),
          
          tabPanel(
            "Component payments",
            h3("Payments by component (pre vs post)"),
            radioButtons(
              "scale_component_payments", label = NULL,
              choices = c("Per participant" = "pp", "Total" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            tableOutput("component_payments_table"),
            br(),
            
            h3("Payments by component (pre vs post)"),
            radioButtons(
              "comp_pay_plot_scale", label = NULL,
              choices = c("Per participant" = "pp", "Total (millions)" = "total"),
              selected = "pp", inline = TRUE
            ),
            br(),
            
            h4("State"),
            plotOutput("plot_component_payments_state"),
            br(),
            
            h4("Federal"),
            plotOutput("plot_component_payments_federal")
          ),
          
          tabPanel(
            "Assumption",
            div(
              class = "assumption-tab",
              h3("State–federal cost shares used in calculations"),
              tableOutput("funding_shares_used")
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = "output.has_run != true",
        div(style = "margin-top: 15px; color: #666;",
            "Click “Run calculator” to display results.")
      )
    )
  )
)

# ----------------- SERVER -----------------

server <- function(input, output, session) {
  
  has_run <- reactiveVal(FALSE)
  observeEvent(input$run, { has_run(TRUE) })
  
  output$has_run <- reactive({ has_run() })
  outputOptions(output, "has_run", suspendWhenHidden = FALSE)
  
  results <- eventReactive(input$run, {
    
    state_use  <- UT_STATE
    county_use <- if (LOCK_COUNTY) UT_COUNTY else input$county_simple
    
    res_full <- if (input$mode == "simple") {
      
      compute_fiscal_effect_simple(
        state_abbrev   = state_use,
        county_name    = county_use,
        avg_pre        = input$avg_pre_simple,
        avg_te         = input$avg_te_simple,
        avg_age        = input$avg_age_simple,
        n_participants = input$n_participants_simple,
        ruleYear       = input$rule_year_simple,
        funding_shares = funding_shares,
        hh_scenario    = input$hh_scenario_simple,
        child_age      = if (input$hh_scenario_simple == "single") NA_integer_ else as.integer(input$child_age_simple),
        spouse_age     = if (input$hh_scenario_simple == "two_parent_1kid") as.integer(input$spouse_age_simple) else NA_integer_
      )
      
    } else {
      
      req(input$data_file)
      df_pre <- readr::read_csv(input$data_file$datapath, show_col_types = FALSE)
      
      out <- compute_fiscal_effect_df(
        df_pre         = df_pre,
        avg_te         = input$avg_te_dataset,
        ruleYear       = input$rule_year_dataset,
        data_name      = NULL,
        funding_shares = funding_shares
      )
      
      rm(df_pre); gc()
      out
    }
    
    avg_te_use <- if (input$mode == "simple") input$avg_te_simple else input$avg_te_dataset
    
    res_keep <- list(
      n_participants                  = res_full$n_participants,
      avg_te                          = avg_te_use,
      delta_by_government             = res_full$delta_by_government,
      delta_by_component              = res_full$delta_by_component,
      payment_by_component_government = res_full$payment_by_component_government
    )
    
    rm(res_full); gc()
    res_keep
  })
  

  
  # ---------------- TAB: ASSUMPTION ----------------
  output$funding_shares_used <- renderTable({
    funding_shares %>%
      mutate(
        `State share`   = scales::percent(state_share, accuracy = 1),
        `Federal share` = scales::percent(federal_share, accuracy = 1)
      ) %>%
      select(
        Location  = countyortownName,
        State     = stateAbbrev,
        Component = component,
        `State share`,
        `Federal share`
      )
  })
  
  
  # ---------------- TAB 1: OVERVIEW ----------------
  
  output$overview_gains <- renderTable({
    req(results())
    res <- results()
    g   <- res$delta_by_government
    
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    state_total <- if (input$mode == "simple") g$state_total else g$state_gain_sum
    fed_total   <- if (input$mode == "simple") g$federal_total else g$federal_gain_sum
    
    state_per <- state_total / N
    fed_per   <- fed_total   / N
    
    tibble::tibble(
      Government = c("Federal","State"),
      `Gain per participant` = scales::dollar(c(fed_per, state_per), accuracy = 1),
      `Participants (N)`     = scales::comma(c(N, N)),
      `Total annual gain`    = scales::dollar(c(fed_total, state_total), accuracy = 1)
    )
  })
  
  output$gov_type_gain_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    payment_by_component_gov <- res$payment_by_component_government
    
    out <- payment_by_component_gov %>%
      pivot_longer(
        cols      = c(state_payment_total, federal_payment_total),
        names_to  = "gov_type",
        values_to = "dollar"
      ) %>%
      mutate(
        gov_type = case_when(
          gov_type == "state_payment_total"   ~ "State",
          gov_type == "federal_payment_total" ~ "Federal",
          TRUE ~ gov_type
        )
      ) %>%
      pivot_wider(
        id_cols     = c(type, component, component_label, gov_type),
        names_from  = period,
        values_from = dollar,
        values_fill = 0
      ) %>%
      mutate(
        diff = case_when(
          type == "benefit" ~ pre - post,
          type == "tax"     ~ post - pre,
          TRUE              ~ NA_real_
        )
      ) %>%
      group_by(gov_type, type) %>%
      summarise(total_gain = sum(diff, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        type = case_when(
          type == "benefit" ~ "Benefit gain",
          type == "tax"     ~ "Tax gain",
          TRUE              ~ type
        )
      ) %>%
      pivot_wider(
        id_cols     = gov_type,
        names_from  = type,
        values_from = total_gain,
        values_fill = 0
      ) %>%
      mutate(
        Net_gain = `Benefit gain` + `Tax gain`,
        Benefit_out = scale_money(`Benefit gain`, N, input$scale_net),
        Tax_out     = scale_money(`Tax gain`,     N, input$scale_net),
        Net_out     = scale_money(Net_gain,       N, input$scale_net)
      ) %>%
      transmute(
        Government = gov_type,
        `Benefit gain` = scales::dollar(Benefit_out, accuracy = 1),
        `Tax gain`     = scales::dollar(Tax_out, accuracy = 1),
        `Net gain`     = scales::dollar(Net_out, accuracy = 1)
      )
    
    out
  })
  
  output$benefit_payments_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    payment_by_component_gov <- res$payment_by_component_government
    
    out <- payment_by_component_gov %>%
      pivot_longer(
        cols      = c(state_payment_total, federal_payment_total),
        names_to  = "gov_type",
        values_to = "dollar"
      ) %>%
      mutate(
        gov_type = case_when(
          gov_type == "state_payment_total"   ~ "State",
          gov_type == "federal_payment_total" ~ "Federal",
          TRUE ~ gov_type
        )
      ) %>%
      filter(type == "benefit") %>%
      group_by(period, gov_type) %>%
      summarise(total = sum(dollar, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        id_cols     = gov_type,
        names_from  = period,
        values_from = total,
        values_fill = 0
      ) %>%
      mutate(
        gain = pre - post,
        pre_out  = scale_money(pre,  N, input$scale_benefit),
        post_out = scale_money(post, N, input$scale_benefit),
        gain_out = scale_money(gain, N, input$scale_benefit)
      ) %>%
      transmute(
        Government = gov_type,
        Pre  = scales::dollar(pre_out,  accuracy = 1),
        Post = scales::dollar(post_out, accuracy = 1),
        Gain = scales::dollar(gain_out, accuracy = 1)
      )
    
    out
  })
  
  output$tax_payments_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    payment_by_component_gov <- res$payment_by_component_government
    
    out <- payment_by_component_gov %>%
      pivot_longer(
        cols      = c(state_payment_total, federal_payment_total),
        names_to  = "gov_type",
        values_to = "dollar"
      ) %>%
      mutate(
        gov_type = case_when(
          gov_type == "state_payment_total"   ~ "State",
          gov_type == "federal_payment_total" ~ "Federal",
          TRUE ~ gov_type
        )
      ) %>%
      filter(type == "tax") %>%
      group_by(period, gov_type) %>%
      summarise(total = sum(dollar, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        id_cols     = gov_type,
        names_from  = period,
        values_from = total,
        values_fill = 0
      ) %>%
      mutate(
        gain = post - pre,
        pre_out  = scale_money(pre,  N, input$scale_tax),
        post_out = scale_money(post, N, input$scale_tax),
        gain_out = scale_money(gain, N, input$scale_tax)
      ) %>%
      transmute(
        Government = gov_type,
        Pre  = scales::dollar(pre_out,  accuracy = 1),
        Post = scales::dollar(post_out, accuracy = 1),
        Gain = scales::dollar(gain_out, accuracy = 1)
      )
    
    out
  })
  
  output$amtr_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    avg_te <- res$avg_te
    validate(need(is.finite(avg_te) && avg_te > 0, "avg_te must be > 0 to compute AMTR."))
    
    pay_tax <- res$payment_by_component_government %>%
      filter(type == "tax") %>%
      group_by(period) %>%
      summarise(
        state_total = sum(state_payment_total, na.rm = TRUE),
        fed_total   = sum(federal_payment_total, na.rm = TRUE),
        .groups = "drop"
      )
    
    get_val <- function(df, p, col) {
      v <- df %>% filter(period == p) %>% pull({{ col }})
      if (length(v) == 0) 0 else v[[1]]
    }
    
    # IMPORTANT: these must match whatever your PRD outputs.
    # If your periods are named differently, change these two strings.
    pre_label  <- "pre"
    post_label <- "post"
    
    state_pre  <- get_val(pay_tax, pre_label,  state_total)
    state_post <- get_val(pay_tax, post_label, state_total)
    fed_pre    <- get_val(pay_tax, pre_label,  fed_total)
    fed_post   <- get_val(pay_tax, post_label, fed_total)
    
    validate(need(
      any(pay_tax$period == pre_label) && any(pay_tax$period == post_label),
      paste0("AMTR needs periods '", pre_label, "' and '", post_label, "'. Found: ",
             paste(unique(pay_tax$period), collapse = ", "))
    ))
    
    tax_gain_raw <- c(
      fed_post - fed_pre,
      state_post - state_pre,
      (fed_post - fed_pre) + (state_post - state_pre)
    )
    
    scale_mode <- input$scale_tax %||% "pp"   # safe default if NULL
    earnings_change <- if (scale_mode == "pp") avg_te else avg_te * N
    tax_gain <- if (scale_mode == "pp") tax_gain_raw / N else tax_gain_raw
    amtr <- tax_gain / earnings_change
    
    tibble::tibble(
      Government = c("Federal", "State", "Total"),
      `Tax gain` = scales::dollar(tax_gain, accuracy = 1),
      `Earnings change` = scales::dollar(earnings_change, accuracy = 1),
      `Avg marginal tax rate` = scales::percent(amtr, accuracy = 0.1)
    )
  })
  
  
  output$plot_overall_gov_type <- renderPlot({
    req(results())
    res <- results()
    
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    payment_by_component_gov <- res$payment_by_component_government
    
    df_plot <- payment_by_component_gov %>%
      pivot_longer(
        cols      = c(state_payment_total, federal_payment_total),
        names_to  = "gov_type",
        values_to = "dollar"
      ) %>%
      mutate(
        gov_type = case_when(
          gov_type == "state_payment_total"   ~ "State",
          gov_type == "federal_payment_total" ~ "Federal",
          TRUE                                ~ gov_type
        ),
        type = case_when(
          type == "benefit" ~ "Benefit spending",
          type == "tax"     ~ "Tax revenue",
          TRUE              ~ type
        )
      ) %>%
      group_by(period, gov_type, type) %>%
      summarise(dollar = sum(dollar, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        period = factor(period, levels = c("pre", "post")),
        value  = if (input$plot_scale == "pp") dollar / N else dollar / 1e6
      )
    
    ylab <- if (input$plot_scale == "pp") "Dollars per participant" else "Total dollars (millions)"
    
    ggplot(df_plot, aes(x = gov_type, y = value, fill = period)) +
      geom_col(position = position_dodge(width = 0.6), width = 0.5) +
      facet_grid(. ~ type) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text.y    = element_text(angle = 0),
        strip.background = element_rect(fill = "#d9eae8", color = "#d9eae8", linewidth = 0.8)
      ) +
      scale_fill_manual(
        values = c(pre = "#FAA523", post = "#29B6A4"),
        breaks = c("pre", "post"),
        labels = c("Pre", "Post")
      ) +
      labs(x = NULL, y = ylab, fill = "Period")
  })
  
  # ---------------- TAB 2: COMPONENT GAINS ----------------
  
  output$component_gains_table <- renderTable({
    req(results())
    res <- results()
    
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    out <- res$delta_by_component %>%
      select(component_label, type, state_gain, federal_gain, diff) %>%
      group_by(component_label, type) %>%
      summarise(
        state_gain   = sum(state_gain,   na.rm = TRUE),
        federal_gain = sum(federal_gain, na.rm = TRUE),
        total_gain   = sum(diff,         na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        state_out   = if (input$scale_component_gains == "pp") state_gain / N else state_gain,
        federal_out = if (input$scale_component_gains == "pp") federal_gain / N else federal_gain,
        total_out   = if (input$scale_component_gains == "pp") total_gain / N else total_gain
      ) %>%
      transmute(
        Component = component_label,
        Type = case_when(
          type == "benefit" ~ "Benefit savings",
          type == "tax"     ~ "Tax revenue",
          TRUE              ~ type
        ),
        `State`   = scales::dollar(state_out,   accuracy = 1),
        `Federal` = scales::dollar(federal_out, accuracy = 1),
        `Total`   = scales::dollar(total_out,   accuracy = 1)
      ) %>%
      arrange(desc(Type), Component)
    
    out
  })
  
  output$plot_component_gains <- renderPlot({
    req(results())
    res <- results()
    
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    df_plot <- res$delta_by_component %>%
      select(component_label, type, state_gain, federal_gain, diff) %>%
      group_by(component_label, type) %>%
      summarise(
        state_gain   = sum(state_gain,   na.rm = TRUE),
        federal_gain = sum(federal_gain, na.rm = TRUE),
        total_gain   = sum(diff,         na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols      = c(state_gain, federal_gain),
        names_to  = "gov_type",
        values_to = "dollar"
      ) %>%
      mutate(
        gov_type = case_when(
          gov_type == "state_gain"   ~ "State",
          gov_type == "federal_gain" ~ "Federal",
          TRUE                       ~ gov_type
        ),
        type = case_when(
          type == "benefit" ~ "Benefit spending",
          type == "tax"     ~ "Tax revenue",
          TRUE              ~ type
        ),
        positive = ifelse(dollar >= 0, 1, 0),
        value = if (input$comp_plot_scale == "pp") dollar / N else dollar / 1e6
      )
    
    comp_levels <- df_plot %>%
      group_by(component_label) %>%
      summarise(tot = sum(dollar, na.rm = TRUE), .groups = "drop") %>%
      arrange(tot) %>%
      pull(component_label)
    
    df_plot <- df_plot %>%
      mutate(component_label = factor(component_label, levels = comp_levels))
    
    ylab <- if (input$comp_plot_scale == "pp") "Dollars per participant" else "Total dollars (millions)"
    
    ggplot(df_plot, aes(x = component_label, y = value, fill = as.factor(positive))) +
      geom_col(width = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
      facet_wrap(~ gov_type) +
      coord_flip() +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text.y    = element_text(angle = 0),
        strip.background = element_rect(fill = "#d9eae8", color = "#d9eae8", linewidth = 0.8)
      ) +
      labs(x = NULL, y = ylab, fill = NULL) +
      scale_fill_manual(
        values = c(`0` = "#E54060", `1` = "#0073A2"),
        labels = c("Loss", "Gain")
      )
  })
  
  # ---------------- TAB 3: COMPONENT PAYMENTS ----------------
  
  output$component_payments_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    out <- res$payment_by_component_government %>%
      group_by(component_label, type, period) %>%
      summarise(
        state_payment   = sum(state_payment_total,   na.rm = TRUE),
        federal_payment = sum(federal_payment_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols      = c(state_payment, federal_payment),
        names_to  = "gov_type",
        values_to = "dollar"
      ) %>%
      mutate(
        gov_type = case_when(
          gov_type == "state_payment"   ~ "State",
          gov_type == "federal_payment" ~ "Federal",
          TRUE                          ~ gov_type
        ),
        value = if (input$scale_component_payments == "pp") dollar / N else dollar
      ) %>%
      pivot_wider(
        id_cols     = c(component_label, type, gov_type),
        names_from  = period,
        values_from = value,
        values_fill = 0
      ) %>%
      mutate(
        gain = case_when(
          type == "benefit" ~ pre - post,
          type == "tax"     ~ post - pre,
          TRUE              ~ NA_real_
        )
      ) %>%
      arrange(desc(type), component_label, gov_type) %>%
      transmute(
        Component  = component_label,
        Type       = type,
        Government = gov_type,
        Pre        = scales::dollar(pre,  accuracy = 1),
        Post       = scales::dollar(post, accuracy = 1),
        Gain       = scales::dollar(gain, accuracy = 1)
      )
    
    out
  })
  
  plot_component_payments_gov <- function(gov_keep) {
    req(results())
    res <- results()
    N <- res$n_participants
    validate(need(!is.null(N) && !is.na(N) && N > 0, "Participants (N) missing or zero."))
    
    df_plot <- res$payment_by_component_government %>%
      pivot_longer(
        cols      = c(state_payment_total, federal_payment_total),
        names_to  = "gov_type",
        values_to = "dollar"
      ) %>%
      mutate(
        gov_type = case_when(
          gov_type == "state_payment_total"   ~ "State",
          gov_type == "federal_payment_total" ~ "Federal",
          TRUE                                ~ gov_type
        ),
        type = case_when(
          type == "benefit" ~ "Benefit spending",
          type == "tax"     ~ "Tax revenue",
          TRUE              ~ type
        ),
        period = factor(period, levels = c("pre", "post"))
      ) %>%
      group_by(component_label, type, period, gov_type) %>%
      summarise(dollar = sum(dollar, na.rm = TRUE), .groups = "drop") %>%
      filter(gov_type == gov_keep) %>%
      group_by(component_label, type) %>%
      filter(sum(abs(dollar), na.rm = TRUE) > 0) %>%
      ungroup() %>%
      mutate(
        value = if (input$comp_pay_plot_scale == "pp") dollar / N else dollar / 1e6
      )
    
    ylab <- if (input$comp_pay_plot_scale == "pp") "Dollars per participant" else "Dollars (millions)"
    
    ggplot(df_plot, aes(x = component_label, y = value, fill = period)) +
      geom_col(position = position_dodge(width = 0.6), width = 0.5) +
      theme_minimal() +
      theme(
        legend.position  = "bottom",
        strip.text.y     = element_text(angle = 0),
        strip.background = element_rect(fill = "#d9eae8", color = "#d9eae8", linewidth = 0.8)
      ) +
      scale_fill_manual(
        values = c(pre = "#FAA523", post = "#29B6A4"),
        breaks = c("pre", "post"),
        labels = c("Pre", "Post")
      ) +
      labs(x = NULL, y = ylab, fill = "Period")
  }
  
  output$plot_component_payments_state <- renderPlot({
    plot_component_payments_gov("State")
  })
  
  output$plot_component_payments_federal <- renderPlot({
    plot_component_payments_gov("Federal")
  })
}

shinyApp(ui = ui, server = server)
