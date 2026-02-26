# app.R  (optimized for ShinyApps memory)

# =========================================================== #
#                      1. SETUP                                #
# =========================================================== #

# --- Load necessary packages ---
current_directory <- getwd()

source(file.path(current_directory, "libraries_oi.R"),                       local = TRUE)
source(file.path(current_directory, "policy-rules-database", "libraries.R"),                       local = TRUE)

# --- Source PRD engine ---
source(file.path(current_directory, "policy-rules-database", "functions", "benefits_functions.R"),  local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "expense_functions.R"),   local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "BenefitsCalculator_functions.R"), local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "TANF.R"),               local = TRUE)
source(file.path(current_directory, "policy-rules-database", "functions", "CCDF.R"),               local = TRUE)

# --- Source our functions ---
source(file.path(current_directory, "functions_oi/nice_table_functions.R"),    local = TRUE)
source(file.path(current_directory, "functions_oi/run_prd_for_df_functions.R"),    local = TRUE)
source(file.path(current_directory, "functions_oi/run_prd_for_input_functions.R"), local = TRUE)

# --- Load all PRD parameter files into a dedicated environment ---
prd_env <- new.env(parent = emptyenv())
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "expenses.rdata"),            envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "benefit.parameters.rdata"),  envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "tables.rdata"),              envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "parameters.defaults.rdata"), envir = prd_env)
load(file.path(current_directory, "policy-rules-database", "prd_parameters", "funding.shares.rdata"),      envir = prd_env)

# Safe to re-source: detach previous attachment if it exists
if ("PRD_PARAMS_VALIDATE" %in% search()) detach("PRD_PARAMS_VALIDATE")
attach(prd_env, name = "PRD_PARAMS_VALIDATE", warn.conflicts = FALSE)

# --- PRD global switches (identical to app.R) ---
source(file.path(current_directory, "program_global_setting.R"),                       local = TRUE)

# --- Set up pre-initialized parallel workers for PRD runs ---
# Workers are initialized once with all PRD code and parameters so that
# subsequent future() calls only serialize the actual data (dataframes).
prd_worker_cl <- parallel::makeCluster(2)
parallel::clusterExport(prd_worker_cl, "current_directory", envir = environment())
parallel::clusterEvalQ(prd_worker_cl, {
  source(file.path(current_directory, "libraries_oi.R"))
  source(file.path(current_directory, "policy-rules-database", "libraries.R"))

  source(file.path(current_directory, "policy-rules-database", "functions", "benefits_functions.R"))
  source(file.path(current_directory, "policy-rules-database", "functions", "expense_functions.R"))
  source(file.path(current_directory, "policy-rules-database", "functions", "BenefitsCalculator_functions.R"))
  source(file.path(current_directory, "policy-rules-database", "functions", "TANF.R"))
  source(file.path(current_directory, "policy-rules-database", "functions", "CCDF.R"))

  source(file.path(current_directory, "functions_oi/nice_table_functions.R"))
  source(file.path(current_directory, "functions_oi/run_prd_for_df_functions.R"))
  source(file.path(current_directory, "functions_oi/run_prd_for_input_functions.R"))

  prd_env <- new.env(parent = emptyenv())
  load(file.path(current_directory, "policy-rules-database", "prd_parameters", "expenses.rdata"),            envir = prd_env)
  load(file.path(current_directory, "policy-rules-database", "prd_parameters", "benefit.parameters.rdata"),  envir = prd_env)
  load(file.path(current_directory, "policy-rules-database", "prd_parameters", "tables.rdata"),              envir = prd_env)
  load(file.path(current_directory, "policy-rules-database", "prd_parameters", "parameters.defaults.rdata"), envir = prd_env)
  load(file.path(current_directory, "policy-rules-database", "prd_parameters", "funding.shares.rdata"),      envir = prd_env)

  if ("PRD_PARAMS_VALIDATE" %in% search()) detach("PRD_PARAMS_VALIDATE")
  attach(prd_env, name = "PRD_PARAMS_VALIDATE", warn.conflicts = FALSE)

  source(file.path(current_directory, "program_global_setting.R"))
})
future::plan(future::cluster, workers = prd_worker_cl)

# --- Serve validation plots as static files ---
addResourcePath("val_plots", file.path(current_directory, "validation", "plots"))

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
        selectInput("state_simple", "State",
                    choices = sort(unique(table.countypop$stateAbbrev)),
                    selected = "UT"),
        selectInput("county_simple", "County",
                    choices = sort(unique(
                      table.countypop$countyortownName[table.countypop$stateAbbrev == "UT"]
                    )),
                    selected = "Salt Lake County"),
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
              tableOutput("funding_shares_used"),
              br(),
              h3("Validation plots (income sweep)"),
              uiOutput("validation_plots")
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

  # Update county choices when state changes
  observeEvent(input$state_simple, {
    counties <- table.countypop %>%
      dplyr::filter(stateAbbrev == input$state_simple) %>%
      dplyr::distinct(countyortownName) %>%
      dplyr::arrange(countyortownName) %>%
      dplyr::pull(countyortownName)
    updateSelectInput(session, "county_simple", choices = counties)
  })

  # --- Input-hash cache: skip recomputation when inputs haven't changed ---
  cache <- reactiveValues(hash = NULL, result = NULL)

  results <- eventReactive(input$run, {

    # Build a hash of all inputs that affect the computation
    if (input$mode == "simple") {
      current_hash <- digest::digest(list(
        input$mode, input$state_simple, input$county_simple,
        input$hh_scenario_simple, input$avg_pre_simple,
        input$avg_te_simple, input$avg_age_simple,
        input$n_participants_simple, input$rule_year_simple,
        input$child_age_simple, input$spouse_age_simple
      ))
    } else {
      req(input$data_file)
      current_hash <- digest::digest(list(
        input$mode,
        digest::digest(file = input$data_file$datapath),
        input$avg_te_dataset, input$rule_year_dataset
      ))
    }

    # Return cached result if inputs haven't changed
    if (identical(current_hash, cache$hash)) return(cache$result)

    # --- Compute from scratch with progress indicator ---
    withProgress(message = "Running calculator...", value = 0, {

      state_use  <- input$state_simple
      county_use <- input$county_simple

      if (input$mode == "simple") {

        incProgress(0.1, detail = "Preparing inputs")

        funding_shares_use <- fundingSharesData %>%
          dplyr::filter(stateAbbrev == state_use)

        incProgress(0.1, detail = "Computing fiscal effects")

        res_full <- compute_fiscal_effect_simple(
          state_abbrev   = state_use,
          county_name    = county_use,
          avg_pre        = input$avg_pre_simple,
          avg_te         = input$avg_te_simple,
          avg_age        = input$avg_age_simple,
          n_participants = input$n_participants_simple,
          ruleYear       = input$rule_year_simple,
          funding_shares = funding_shares_use,
          hh_scenario    = input$hh_scenario_simple,
          child_age      = if (input$hh_scenario_simple == "single") NA_integer_ else as.integer(input$child_age_simple),
          spouse_age     = if (input$hh_scenario_simple == "two_parent_1kid") as.integer(input$spouse_age_simple) else NA_integer_
        )

        avg_te_use <- input$avg_te_simple

      } else {

        incProgress(0.1, detail = "Reading dataset")

        df_pre <- readr::read_csv(input$data_file$datapath, show_col_types = FALSE)

        incProgress(0.1, detail = "Computing fiscal effects (pre & post in parallel)")

        res_full <- compute_fiscal_effect_df(
          df_pre         = df_pre,
          avg_te         = input$avg_te_dataset,
          ruleYear       = input$rule_year_dataset,
          data_name      = NULL,
          funding_shares = fundingSharesData
        )

        avg_te_use <- input$avg_te_dataset

        rm(df_pre); gc()
      }

      incProgress(0.7, detail = "Aggregating results")

      res_keep <- list(
        n_participants          = res_full$n_participants,
        avg_te                  = avg_te_use,
        delta_by_government     = res_full$delta_by_government,
        payment_long            = res_full$payment_long,
        summary_by_gov_type     = res_full$summary_by_gov_type,
        component_gains_summary = res_full$component_gains_summary
      )

      rm(res_full); gc()

      incProgress(0.1, detail = "Done")

      # Store in cache
      cache$hash   <- current_hash
      cache$result <- res_keep

      res_keep
    })
  })
  

  
  # ---------------- TAB: ASSUMPTION ----------------
  output$funding_shares_used <- renderTable({
    state_show <- input$state_simple %||% "UT"
    fundingSharesData %>%
      dplyr::filter(stateAbbrev == state_show) %>%
      dplyr::mutate(
        `State share`   = scales::percent(state_share, accuracy = 1),
        `Federal share` = scales::percent(federal_share, accuracy = 1)
      ) %>%
      dplyr::select(
        State     = stateAbbrev,
        Component = component,
        `State share`,
        `Federal share`
      )
  })
  
  
  # ---------------- VALIDATION PLOTS (Assumption tab) ----------------
  output$validation_plots <- renderUI({
    st <- input$state_simple %||% "UT"
    yr <- if (input$mode == "dataset") {
      input$rule_year_dataset %||% 2024
    } else {
      input$rule_year_simple %||% 2024
    }

    # In dataset mode show all three scenarios; in simple mode show the selected one
    if (input$mode == "dataset") {
      scenarios <- c(single = "Single (no kids)",
                     single_parent_1kid = "Single parent + 1 kid",
                     two_parent_1kid = "Two parents + 1 kid")
    } else {
      hh <- input$hh_scenario_simple %||% "single"
      scenarios <- setNames(hh, hh)
    }

    plot_types <- c("amtr", "benefits", "taxes")
    plot_labels <- c(
      amtr = "Average Marginal Tax Rate by Income",
      benefits = "Benefit Payments by Income",
      taxes = "Tax Payments by Income"
    )

    all_tags <- lapply(names(scenarios), function(hh) {
      img_tags <- lapply(plot_types, function(ptype) {
        fname <- paste0(ptype, "_", st, "_", hh, "_", yr, ".png")
        fpath <- file.path(current_directory, "validation", "plots", fname)
        if (file.exists(fpath)) {
          tags$div(
            style = "margin-bottom: 20px;",
            tags$img(src = paste0("val_plots/", fname), width = "100%")
          )
        }
      })
      img_tags <- Filter(Negate(is.null), img_tags)

      if (length(img_tags) > 0) {
        header <- if (length(scenarios) > 1) {
          tags$h4(style = "margin-top: 25px; border-bottom: 1px solid #ddd; padding-bottom: 5px;",
                  scenarios[[hh]])
        }
        tagList(header, img_tags)
      }
    })

    all_tags <- Filter(Negate(is.null), all_tags)

    if (length(all_tags) == 0) {
      tags$p(style = "color: #999; font-style: italic;",
             paste0("No validation plots available for ", st, " / ", yr, "."))
    } else {
      do.call(tagList, all_tags)
    }
  })

  # ---------------- TAB 1: OVERVIEW ----------------
  
  output$overview_gains <- renderTable({
    req(results())
    res <- results()
    g   <- res$delta_by_government
    N   <- res$n_participants
    validate_N(N)

    state_total <- g$state_gain_sum
    fed_total   <- g$federal_gain_sum

    tibble::tibble(
      Government = c("Federal", "State"),
      `Gain per participant` = scales::dollar(c(fed_total / N, state_total / N), accuracy = 1),
      `Participants (N)`     = scales::comma(c(N, N)),
      `Total annual gain`    = scales::dollar(c(fed_total, state_total), accuracy = 1)
    )
  })
  
  output$gov_type_gain_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    validate_N(N)

    res$summary_by_gov_type %>%
      pivot_wider(id_cols = c(gov_type, type), names_from = period, values_from = dollar, values_fill = 0) %>%
      mutate(gain = case_when(type == "benefit" ~ pre - post, type == "tax" ~ post - pre, TRUE ~ NA_real_)) %>%
      mutate(type = case_when(type == "benefit" ~ "Benefit gain", type == "tax" ~ "Tax gain", TRUE ~ type)) %>%
      pivot_wider(id_cols = gov_type, names_from = type, values_from = gain, values_fill = 0) %>%
      mutate(Net_gain = `Benefit gain` + `Tax gain`) %>%
      transmute(
        Government     = gov_type,
        `Benefit gain` = fmt_money_scaled(`Benefit gain`, N, input$scale_net),
        `Tax gain`     = fmt_money_scaled(`Tax gain`,     N, input$scale_net),
        `Net gain`     = fmt_money_scaled(Net_gain,       N, input$scale_net)
      )
  })
  
  output$benefit_payments_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    validate_N(N)

    res$summary_by_gov_type %>%
      filter(type == "benefit") %>%
      pivot_wider(id_cols = gov_type, names_from = period, values_from = dollar, values_fill = 0) %>%
      mutate(gain = pre - post) %>%
      transmute(
        Government = gov_type,
        Pre  = fmt_money_scaled(pre,  N, input$scale_benefit),
        Post = fmt_money_scaled(post, N, input$scale_benefit),
        Gain = fmt_money_scaled(gain, N, input$scale_benefit)
      )
  })
  
  output$tax_payments_table <- renderTable({
    req(results())
    res <- results()
    N <- res$n_participants
    validate_N(N)

    res$summary_by_gov_type %>%
      filter(type == "tax") %>%
      pivot_wider(id_cols = gov_type, names_from = period, values_from = dollar, values_fill = 0) %>%
      mutate(gain = post - pre) %>%
      transmute(
        Government = gov_type,
        Pre  = fmt_money_scaled(pre,  N, input$scale_tax),
        Post = fmt_money_scaled(post, N, input$scale_tax),
        Gain = fmt_money_scaled(gain, N, input$scale_tax)
      )
  })
  
  output$amtr_table <- renderTable({
    req(results())
    res <- results()
    N      <- res$n_participants
    avg_te <- res$avg_te
    validate(need(is.finite(avg_te) && avg_te > 0, "avg_te must be > 0 to compute AMTR."))

    tax_wide <- res$summary_by_gov_type %>%
      filter(type == "tax") %>%
      pivot_wider(id_cols = gov_type, names_from = period, values_from = dollar, values_fill = 0) %>%
      mutate(gain = post - pre)

    d_fed   <- tax_wide$gain[tax_wide$gov_type == "Federal"]
    d_state <- tax_wide$gain[tax_wide$gov_type == "State"]
    if (length(d_fed) == 0) d_fed <- 0
    if (length(d_state) == 0) d_state <- 0
    d_total <- d_fed + d_state
    tax_gain_raw <- c(d_fed, d_state, d_total)

    scale_mode <- input$scale_tax %||% "pp"
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
    validate_N(N)

    df_plot <- res$summary_by_gov_type %>%
      mutate(
        type   = rename_type(type, "spending"),
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
    validate_N(N)

    sc <- input$scale_component_gains
    res$component_gains_summary %>%
      transmute(
        Component = component_label,
        Type      = rename_type(type, "savings"),
        `State`   = fmt_money_scaled(state_gain,   N, sc),
        `Federal` = fmt_money_scaled(federal_gain, N, sc),
        `Total`   = fmt_money_scaled(total_gain,   N, sc)
      ) %>%
      arrange(desc(Type), Component)
  })
  
  output$plot_component_gains <- renderPlot({
    req(results())
    res <- results()
    N <- res$n_participants
    validate_N(N)

    df_plot <- res$component_gains_summary %>%
      pivot_longer(cols = c(state_gain, federal_gain), names_to = "gov_type", values_to = "dollar") %>%
      mutate(
        gov_type = ifelse(gov_type == "state_gain", "State", "Federal"),
        type     = rename_type(type, "spending"),
        positive = ifelse(dollar >= 0, 1, 0),
        value    = if (input$comp_plot_scale == "pp") dollar / N else dollar / 1e6
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
    validate_N(N)

    sc <- input$scale_component_payments
    res$payment_long %>%
      group_by(component_label, type, period, gov_type) %>%
      summarise(dollar = sum(dollar, na.rm = TRUE), .groups = "drop") %>%
      mutate(dollar = scale_money(dollar, N, sc)) %>%
      pivot_wider(id_cols = c(component_label, type, gov_type), names_from = period, values_from = dollar, values_fill = 0) %>%
      mutate(gain = case_when(type == "benefit" ~ pre - post, type == "tax" ~ post - pre, TRUE ~ NA_real_)) %>%
      arrange(desc(type), component_label, gov_type) %>%
      transmute(
        Component  = component_label,
        Type       = type,
        Government = gov_type,
        Pre        = scales::dollar(pre,  accuracy = 1),
        Post       = scales::dollar(post, accuracy = 1),
        Gain       = scales::dollar(gain, accuracy = 1)
      )
  })
  
  plot_component_payments_gov <- function(gov_keep) {
    req(results())
    res <- results()
    N <- res$n_participants
    validate_N(N)

    df_plot <- res$payment_long %>%
      mutate(type = rename_type(type, "spending"), period = factor(period, levels = c("pre", "post"))) %>%
      filter(gov_type == gov_keep) %>%
      group_by(component_label, type, period) %>%
      summarise(dollar = sum(dollar, na.rm = TRUE), .groups = "drop") %>%
      group_by(component_label, type) %>%
      filter(sum(abs(dollar), na.rm = TRUE) > 0) %>%
      ungroup() %>%
      mutate(value = if (input$comp_pay_plot_scale == "pp") dollar / N else dollar / 1e6)

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
