# TIME SERIES PREDICTIONS BASED ON DATA FROM CBS (DUTCH STATISTICAL OFFICE) ----
# Inspiratie:
# - https://gavandrewj.shinyapps.io/gtWeatherPrediction/
#   code: https://github.com/gavandrewj/gtClimatePrediction
# - https://business-science.shinyapps.io/nostradamus/
#   code: learning labs 46: forecasting at scale
# Voor data tussen modules uitwisselen, zie:
# https://gist.github.com/kota8/a0fbf401e143920a64e70d3341ea5e54
# https://shiny.rstudio.com/articles/modules.html

# LIBRARIES ----

# Plotting
library(plotly)

# Shiny
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(DT)
library(reactable)

# Modeling
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

library(rules)
# engines
library(Cubist)  # cubist
library(glmnet)  # glm
library(kknn)    # Nearest Neighbors
library(earth)   # MARS
library(ranger)  # random forest
library(kernlab) # SVM
library(xgboost) # xgboost

# Data
# library(tidyquant)
library(cbsodataR)
library(tidyquant)

# # Data Wrangling
library(janitor)

# Time Series
library(lubridate)
library(timetk)

# Core
library(tidyverse)


# SETTINGS ---------------------------------------------------------------------
# set language to English
Sys.setlocale("LC_ALL","English")

# * file names ----
fn_pump_prices    <- "00_local_data/cbs_daily_pump_prices.RDS"
fn_housing_prices <-
    "00_local_data/cbs_quarterly_housing_prices.RDS"
fn_air_passengers <- "00_local_data/cbs_monthly_air_passengers.RDS"
fn_dow <- "00_local_data/dow.RDS"

# set start date for time series with too many observations for shinyapps to keep in memory
ts_start <- str_c(year(Sys.Date() %-time% "2 years 6 months"), "-", month(Sys.Date() %-time% "5 years 1 month"))


# # LOAD "OLD" DATA --------------------------------------------------------------
# cbs_pump_prices_tbl    <- readRDS(fn_pump_prices)
# cbs_housing_prices_tbl <- readRDS(fn_housing_prices)
# cbs_air_passengers_tbl <- readRDS(fn_air_passengers)
# dow_tbl                <- readRDS(fn_dow)

# SOURCE FUNCTIONS -------------------------------------------------------------
source("autoforecast/data_functions.R")
source("autoforecast/plot_functions.R")
source("autoforecast/ui_functions.R")
# source("autoforecast/setup.R")

# GET DATA ---------------------------------------------------------------------
# * daily fuel prices ----
tryCatch(
    (
        cbs_get_data("80416ENG") %>%
            cbs_add_label_columns() %>%
            clean_names() %>%
            mutate(date = ymd(periods)) %>%
            select(
                date,
                petrol = euro95_1,
                diesel = diesel_2,
                lpg = lpg_3
            ) %>% 
            filter_by_time(.date_var   = date,
                           .start_date = ts_start,
                           .end_date   = "end") %>%
            saveRDS(file = fn_pump_prices)
    ),
    finally = cbs_pump_prices_tbl <<- 
        read_rds(fn_pump_prices)
)

# * quarterly housing prices ----
tryCatch(
    (
        cbs_get_data("83910ENG") %>%
            cbs_add_label_columns() %>%
            cbs_add_date_column() %>%
            clean_names() %>%
            filter(periods_freq == "Q") %>%
            select(
                date                   = periods_date,
                type_of_dwelling       = type_of_dwelling_label,
                average_purchase_price = average_purchase_price_7
            )  %>%
            mutate(across(where(is.integer), as.numeric)) %>%
            pivot_wider(names_from  = type_of_dwelling,
                        values_from = average_purchase_price) %>%
            saveRDS(file = fn_housing_prices)
    ),
    finally = cbs_housing_prices_tbl <<- read_rds(fn_housing_prices)
)

# * monthly aviation figures ----
tryCatch(
    (
        cbs_get_data("37478ENG") %>%
            cbs_add_label_columns() %>%
            cbs_add_date_column() %>%
            clean_names() %>%
            filter(periods_freq == "M") %>%
            select(date = periods_date,
                   airports_label,
                   contains("total_passengers")) %>%
            rename_all(list(
                ~ stringr::str_replace_all(., "total_", "")
            )) %>%
            rename_all(list(
                ~ stringr::str_replace_all(., "_[0-9]{1,2}$", "")
            )) %>%
            mutate(across(where(is.integer), as.numeric)) %>%
            pivot_wider(names_from = 2, values_from = 3) %>%
            saveRDS(file = fn_air_passengers)
    ),
    finally = cbs_air_passengers_tbl <<- read_rds(fn_air_passengers)
)

# * DOW stocks ----
tryCatch(
    (
        get_dow_data() %>% 
            filter_by_time(.date_var   = date,
                           .start_date = ts_start,
                           .end_date   = "end") %>% 
            saveRDS(file = fn_dow)
    ),
    finally = dow_tbl <<- read_rds(fn_dow)
)

# DATA DESCRIPTIONS ----
description_tbl <-
    tribble(
        ~ dataset_id, ~ desc,
        "cbs_pump_prices_tbl", "Daily prices for different fuel types. Data source: https://opendata.cbs.nl/statline/#/CBS/en/dataset/80416ENG.",
        "cbs_housing_prices_tbl", "Housing prices in the Netherlands. Quarterly average housing prices per type. Date source: https://opendata.cbs.nl/statline/#/CBS/en/dataset/83910ENG.",
        "cbs_air_passengers_tbl", "Montly total passenger data (departures, arrivals & transit) for all Dutch Airports. Date source: https://opendata.cbs.nl/statline/#/CBS/en/dataset/37478ENG.",
        "dow_tbl", "(Adjusted) stock prices for DOW-Jones stocks."
    )

# COMPOSE DATASETS TIBBLE----
datasets_tbl <- list(
    cbs_pump_prices_tbl    = cbs_pump_prices_tbl,
    cbs_housing_prices_tbl = cbs_housing_prices_tbl,
    cbs_air_passengers_tbl = cbs_air_passengers_tbl,
    dow_tbl                = dow_tbl) %>%
    enframe(
        name  = "dataset_id",
        value = "data"
    ) %>%
    left_join(description_tbl, by = "dataset_id")


# COMPOSE CHOICES ----
# * dataset choice
choices_vec <- datasets_tbl$dataset_id
# # * transformation choice ----
# choices_trans_vec <- c("no transformation", "log1p")
# # * scaling choice ----
# choices_scale_vec <- c("no scaling", "center & scale", "center", "scale")


# SOURCE MODULES ---------------------------------------------------------------
# source("modules/module_pick_dataset.R")
# source("modules/module_transformations.R")

# SHINY APP --------------------------------------------------------------------
# UI ---------------------------------------------------------------------------
ui <- navbarPage(
    id = "time_series",
    
    # * SET TITLE + THEME ----
    title       = div(img(src="Leo.jpg"),"Time Series Forecasting"),
    windowTitle = "shiny - time series forecasting",
    theme       = shinytheme("flatly"),
    selected    = "Pick & Explore Dataset",
    # useShinyjs(),
    
    # * SET CSS ----
    tags$head(
        tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = "styles-default.css"
        )
    ),
    
    
    # 1. SELECT DATASET & SHOW SUMMARY ----
    # # (nog?) niet werken met modules
    # mod_pick_dataset_ui(id = "pick_dataset", title = "Pick dataset"),
    # 1.1 user inputs ----
    tabPanel(
        title = "Pick & Explore Dataset",
        id = "dataset",
        
        # load message
        load_message(),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        
        fluidRow(
            column(width = 4,
                   wellPanel(
                       h4("User Inputs:"),
                       pickerInput(
                           inputId  = "sel_dataset",
                           label    = "Load a dataset",
                           choices  = choices_vec,
                           selected = "cbs_pump_prices_tbl",
                           multiple = FALSE
                       ),
                       
                       pickerInput(
                           inputId  = "sel_vars_load",
                           label    = "Select time series to explore",
                           choices  = names(cbs_pump_prices_tbl)[!names(cbs_pump_prices_tbl) %in% c("date")],
                           selected = names(cbs_pump_prices_tbl)[!names(cbs_pump_prices_tbl) %in% c("date")],
                           multiple = TRUE,
                           options = list(`actions-box` = TRUE)
                       )
                   )),
            # 1.2 dataset summary ----
            column(
                width = 8,
                fluidRow(
                    h3("Time Series Information"),
                    hr(),
                    strong("Description"),
                    textOutput("description"),
                    br(),
                    br()
                ),
                fluidRow(
                    column(width = 4,
                           strong("Dataset"),
                           textOutput("dataset")),
                    column(width = 2,
                           strong("Time Scale"),
                           textOutput("time_scale")),
                    column(width = 2,
                           strong("Frequency"),
                           textOutput("time_frequency")),
                    column(width = 2,
                           strong("# Time Series"),
                           textOutput("time_series_count")),
                    column(width = 2,
                           strong("# Observations"),
                           textOutput("nobs"))
                )
            )
        ),
        fluidRow(# 1.3 explore time series ----
                 column(
                     width = 6,
                     h4("Time Series Plot & Raw Data Table"),
                     tabsetPanel(
                         id = "tab_raw_data",
                         tabPanel(
                             title = "Time Series Plot",
                             div(
                                 plotlyOutput(outputId = "time_plot",
                                              height   = "600px")
                             )
                         ),
                         tabPanel(
                             title = "Time Series Data Table",
                             div(
                                 DTOutput(outputId = "raw_data_tbl",
                                          height   = "600px")
                                 )
                             )
                     )
                 ),
                 # 1.4 time series seasonality ----
                 column(
                     width = 6,
                     h4("Seasonal Diagnostics"),
                     uiOutput(outputId = "seasonality_plots")
                     )
                 ),
        fluidRow(
            column(# 1.5 Anomoly Plot ----
                width = 6,
                h4("Anomoly Plot"),
                tabsetPanel(
                    id = "tab_anomoly",
                    tabPanel(
                        title = "Selected Time Series",
                        plotlyOutput("anomoly_plot",
                                     height   = "600px")
                        )
                    )
                ),
            column(# 1.6 Autocorrelation Plots ----
                width = 6,
                h4("Autocorrelation Plots"),
                uiOutput("acf_plots")
            )
        )
    )
    ,
    
    # 9. FORECAST ----
    tabPanel(title = "Forecasting",
             id = "forecasting",

             # load message
             load_message(),
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$div("Running & selecting forecasts... This will take a few minutes.",id="loadmessage")),

             # 9.1 user inputs ----
             column(width = 4,
                    wellPanel(
                        # h4("Usefull information:"),
                        # textOutput("info_freq"),
                        # textOutput("info_scale"),
                        # textOutput("info_rec_horizon"),
                        h4("Forecast choices:"),
                        uiOutput("forecast_choices"),
                        br(), br(),
                        actionButton(inputId = "forecast",
                                     label   = "Run Forecast",
                                     icon    = icon("angle-double-right")
                                     )
                    ),
                    wellPanel(
                        h4("Accuracy Table -  Best In Class Models"),
                        # DTOutput("accuracy_tbl")
                        reactableOutput("accuracy_tbl")
                        )
                    ),
             # 9.2 result plots ----
             column(width = 8,
                    h4("Forecast Results - Best Global Model")
                    ,
                    plotlyOutput("forecast_plot",
                                 height   = "800px")
                    )
             )
    )
    
    
    # SERVER -------------------------------------------------------------------
    server <- function(session, input, output) {
        
        # * Setup Reactive Values ----
        rv <- reactiveValues()
        
        # * OBSERVER ----
        observeEvent(input$sel_dataset, {
            
            rv$current_selection <- input$sel_dataset
            rv$data              <-
                datasets_tbl %>% select_dataset(choice = rv$current_selection)
            rv$data_long <- rv$data %>%
                pivot_longer(cols     = -date,
                             names_to = "id")
            rv$desc              <-
                datasets_tbl %>% get_dataset_description(choice = rv$current_selection)
            rv$data_colnames     <- colnames(rv$data)
            rv$time_series_count <- length(rv$data_colnames)-1
            
            # Summary information
            rv$ts_summary_tbl <- rv$data %>%
                tk_summary_diagnostics()
            
            rv$ts_scale <- rv$ts_summary_tbl$scale[[1]]
            
            print(rv$ts_scale)
            
            rv$median_nobs <- rv$ts_summary_tbl %>%
                pull(n.obs) %>%
                median()
            
            rv$ts_frequency <- rv$data %>% pull(date) %>% tk_get_frequency()
            
            rv$lag_limit <- rv$median_nobs %>%
                `*`(0.4) %>%
                round()
            
            rv$horizon_recommended <-
                round(0.18 * rv$median_nobs) - (round(0.18 * rv$median_nobs) %% rv$ts_frequency)
            
            rv$value_name <- names(rv$data)[!names(rv$data) %in% c("date")] %>% 
                sort()
            
            updatePickerInput(
                session = session,
                inputId = "sel_vars_load",
                choices = rv$value_name,
                selected = rv$value_name[1:min(c(6, length(rv$value_name)))]
            ) 
            
            # INITIALIZE FORECAST TBLS
            rv$future_forecast_tbl <- NULL
            rv$accuracy_tbl        <- NULL
            
        },
        ignoreNULL = FALSE)
        
        # 1. SELECT DATA SET & SHOW SUMMARY ----------------------------------------
        # 1.1 user inputs ----
        # no server code

        # 1.2 summary ----
        output$description       <-
            renderText(stringr::str_glue("{rv$desc}"))
        output$dataset           <-
            renderText(stringr::str_glue("{rv$current_selection}"))
        output$time_scale        <-
            renderText(stringr::str_glue("{rv$ts_scale}"))
        output$time_frequency        <-
            renderText(stringr::str_glue("{rv$ts_frequency}"))
        output$time_series_count <-
            renderText(stringr::str_glue("{rv$time_series_count}"))
        output$nobs              <-
            renderText(format(rv$median_nobs, big.mark = ","))

        # 1.3 raw data ----
        # 1.3.1 time series plot ----
        output$time_plot <-
            renderPlotly({
                req(rv$data)
                
                rv$data %>% 
                    select(date, input$sel_vars_load) %>%
                    plot_raw_data(plot_title = "")
            })

        # 1.3.1 raw data table ----
        output$raw_data_tbl <-
            renderDT(
                rv$data %>% 
                    select(date, input$sel_vars_load) %>%
                    datatable(
                              # select(input$sel_vars_load),
                          rownames = FALSE,
                          options = list(order = list(list(0, 'desc')))) %>%
                    formatCurrency(
                        columns  = 2:length(c(date, input$sel_vars_load)),
                        mark     = ",",
                        dec.mark = ".",
                        currency = ""
                    )
            )
        
        # 1.4 seasonality plots ----
        output$seasonality_plots <- renderUI({
            req(input$sel_vars_load)
            
            # First Tab Panel
            tab_panel_1 <- tabPanel(
                title = input$sel_vars_load[1],
                rv$data %>%
                    pivot_longer(cols = -date, names_to = "id") %>% 
                    filter(id == input$sel_vars_load[1])  %>%
                    group_by(id) %>%
                    plot_seasonal_diagnostics(.date_var    = date,
                                              .value       = value,
                                              .interactive = FALSE,
                                              .title       = "") %>%
                    ggplotly(height = 600)
            )
            
            # Seasonality remaining Tab Panels
            seasonality_tab_panels <- NULL
            
            if(length(input$sel_vars_load[2:length(input$sel_vars_load)])>0) {
                seasonality_tab_panels <- 
                    input$sel_vars_load[2:length(input$sel_vars_load)] %>%
                    map(.f = function(x) {
                        tabPanel(
                            title = x,
                            # dataset() %>%
                            rv$data %>%
                                pivot_longer(cols = -date, names_to = "id") %>% 
                                filter(id == x)  %>%
                                group_by(id) %>%
                                plot_seasonal_diagnostics(.date_var    = date,
                                                          .value       = value,
                                                          .interactive = FALSE,
                                                          .title       = "") %>%
                                ggplotly(height = 600)
                        )
                    })
            }
            
            # Building the tabset panel
            do.call(
                what = tabsetPanel,
                args = list(tab_panel_1) %>% 
                    append(seasonality_tab_panels) %>%
                    append(list(id   = "tab_panel_seasonality",
                                type = "tabs"))
            )
        })
        
        # 1.5 Anomoly Plot ----
        output$anomoly_plot <-
            renderPlotly(rv$data %>%
                             pivot_longer(cols = -date, names_to = "id") %>%
                             filter(id %in% input$sel_vars_load)  %>%
                             group_by(id) %>%
                             plot_anomaly_diagnostics(.date_var   = date,
                                                      .value      = value,
                                                      .facet_ncol = 2,
                                                      .title = "")
                         )
        
        # 1.6 Autocorrelation Plots ----
        output$acf_plots <- renderUI({

            # First Tab Panel
            tab_panel_1 <- tabPanel(
                title = input$sel_vars_load[1],
                rv$data %>%
                    pivot_longer(cols = -date, names_to = "id") %>%
                    filter(id == input$sel_vars_load[1])  %>%
                    group_by(id) %>%
                    plot_acf_diagnostics(.date_var    = date,
                                              .value       = value,
                                              .interactive = FALSE) %>%
                    ggplotly(height = 600)
            )

            # Autocorrelation remaining Tab Panels
            seasonality_tab_panels <- NULL

            if(length(input$sel_vars_load[2:length(input$sel_vars_load)])>0) {
                acf_tab_panels <-
                    input$sel_vars_load[2:length(input$sel_vars_load)] %>%
                    map(.f = function(x) {
                        tabPanel(
                            title = x,
                            rv$data %>%
                                pivot_longer(cols = -date, names_to = "id") %>%
                                filter(id == x)  %>%
                                group_by(id) %>%
                                plot_acf_diagnostics(.date_var   = date,
                                                          .value      = value,
                                                          .interactive = FALSE) %>%
                                ggplotly(height = 600)
                        )
                    })
            }

            # Building the tabset panel
            do.call(
                what = tabsetPanel,
                args = list(tab_panel_1) %>%
                    append(acf_tab_panels) %>%
                    append(list(id   = "tab_panel_acf",
                                type = "tabs"))
            )
        })


        # 9 FORECAST ----
        # * OBSERVER - Forecast ----
        observeEvent(input$forecast, {

            req(input$horizon)

            # ** SETUP ----
            run_parallel  <- FALSE

            # ** TRANSFORMATIONS (+ INVERSIONS) ----
            # * log transformer ----
            trans_fun     <- if(input$sel_log1p) {
                log1p
            } else {
                function(x) {x}
                }
            trans_fun_inv <- if(input$sel_log1p) {
                expm1
            } else {
                function(x) {x}
                }

            # * outlier cleaning ----
            # outlier cleaning, no inversion needed
            clean_fun <- if(input$sel_cleaning) {
                ts_clean_vec
            } else {
                function(x) {x}
                }    

            # * forecast horizon ----
            rv$horizon <- input$horizon

            # ** FEATURE ENGINEERING ----
            # * grouped data transformations + extending data ----
            message("Feature Engineering")
            rv$full_data_tbl <- rv$data %>%
                pivot_longer(cols = -date, names_to = "id") %>%

                # Remove missing values
                drop_na() %>%

                # Apply transformation
                mutate(value = ifelse(value < 0, 0, value)) %>% # no negative values in log-transformations
                mutate(value = trans_fun(value)) %>%

                # Apply Group-wise Time Series Manipulations
                group_by(id) %>%
                pad_by_time(.date_var = date, .pad_value = 0) %>%

                # extend data
                future_frame(
                    .date_var   = date,
                    .length_out = rv$horizon,
                    .bind_data  = TRUE
                ) %>%
                ungroup() %>%

                # Lags & Rolling Features / Fourier
                mutate(id = as_factor(id)) %>%
                group_by(id) %>%
                group_split() %>%
                map(.f = function(df) {
                    df %>%
                        arrange(date) %>%
                        tk_augment_fourier(date, .periods = c(rv$ts_frequency, 2*rv$ts_frequency))  %>%
                        tk_augment_lags(.value = value, .lags = rv$horizon) %>%
                        tk_augment_slidify(
                            paste0("value_lag", rv$horizon),
                            .f       = ~ mean(.x, na.rm = TRUE),
                            .period  = c(rv$ts_frequency,
                                         2*rv$ts_frequency,
                                         4*rv$ts_frequency,
                                         8*rv$ts_frequency),
                            .partial = TRUE,
                            .align   = "center"
                        )
                }) %>%
                bind_rows() %>%
                # NaN to NA
                mutate(across(where(is.double), ~replace(., is.nan(.), NA))) %>%

                rowid_to_column(var = "rowid")

            print(rv$full_data_tbl)

            # ** SPLITTING DATA ----
            # * data prepared ----
            # remove future data
            rv$actual_data <- rv$full_data_tbl %>%
                filter(!is.na(value))

            # remove rows without features
            rv$data_prepared_tbl <- rv$actual_data %>%
                drop_na()

            # * future data ----
            rv$future_tbl <- rv$full_data_tbl %>%
                filter(is.na(value))

            # * splitting ----
            rv$splits <- rv$data_prepared_tbl %>%
                time_series_split(
                    date_var   = date,
                    assess     = rv$horizon,
                    cumulative = TRUE
                )

            # * cleaned training data ----
            rv$train_cleaned <- training(rv$splits) %>%
                group_by(id) %>%
                mutate(value = clean_fun(value, period = rv$ts_frequency)) %>%
                ungroup()

            # ** RECIPES ----
            # * base recipe: recipe_spec_1 ----
            recipe_spec_1 <-
                recipe(value ~ ., data = rv$train_cleaned) %>%
                update_role(rowid, new_role = "indicator") %>%
                step_timeseries_signature(date) %>%
                step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")) %>%
                step_normalize(date_index.num, date_year) %>%
                step_other(id) %>%
                step_dummy(all_nominal(), one_hot = TRUE)
            
            # adjust recipe for frequency
            recipe_spec_1 <- if(rv$ts_scale == "week") {
                recipe_spec_1 %>% 
                    step_rm(matches("(day)"))
            } else if(rv$ts_scale == "month") {
                recipe_spec_1 %>% 
                    step_rm(matches("(day)|(week)"))
            } else if(rv$ts_scale == "quarter") {
                recipe_spec_1 %>% 
                    step_rm(matches("(day)|(week)|(month)"))
            } else {
                recipe_spec_1
            }
            recipe_spec_1 %>% prep() %>% juice() %>% glimpse()
            
            # * without lags: recipe_spec_2: ----
            recipe_spec_2 <- 
                recipe_spec_1 %>% 
                step_rm(matches("_lag"))
            recipe_spec_2 %>% prep() %>% juice() %>% glimpse()
            
            # * without fourier: recipe_spec_2: ----
            recipe_spec_3 <- 
                recipe_spec_1 %>% 
                step_rm(matches("(date_sin)|(date_cos)"))
            recipe_spec_3 %>% prep() %>% juice() %>% glimpse()
            
            # * without lags and fourier: recipe_spec_4: ----
            recipe_spec_4 <-
                recipe_spec_1 %>%
                step_rm(matches("(_lag)|(date_sin)|(date_cos)"))
            recipe_spec_4 %>% prep() %>% juice() %>% glimpse()
            
            # * recipes excl date var ----
            recipe_spec_1_alt <- recipe_spec_1 %>%
                update_role(date, new_role = "ID")
            
            recipe_spec_2_alt <- recipe_spec_2 %>%
                update_role(date, new_role = "ID")
            
            recipe_spec_3_alt <- recipe_spec_3 %>%
                update_role(date, new_role = "ID")
            
            recipe_spec_4_alt <- recipe_spec_4 %>%
                update_role(date, new_role = "ID")

            # ** MODELLING ----
            # * MODEL SPECS ----
            # + ARIMA 1 ----
            wflw_spec_arima_1 <- workflow() %>%
                add_model(
                    arima_reg() %>% set_engine("auto_arima")
                ) %>%
                add_recipe(recipe = recipe(value ~ date, rv$train_cleaned))
            
            # + ARIMA 2 ----
            wflw_spec_arima_2 <- workflow() %>%
                add_model(
                    modeltime::arima_boost() %>% set_engine('auto_arima_xgboost')
                ) %>%
                add_recipe(recipe = recipe(value ~ date, rv$train_cleaned))
            
            # # + Cubist 1 ----
            # wflw_spec_cubist_1 <- workflow() %>%
            #     add_model(
            #         cubist_rules() %>% set_engine("Cubist")
            #     ) %>%
            #     add_recipe(recipe_spec_1_alt)
            # 
            # # + Cubist 2 ----
            # wflw_spec_cubist_2 <- workflow() %>%
            #     add_model(
            #         cubist_rules() %>% set_engine("Cubist")
            #     ) %>%
            #     add_recipe(recipe_spec_2_alt)
            # 
            # # + Cubist 3 ----
            # wflw_spec_cubist_3 <- workflow() %>%
            #     add_model(
            #         cubist_rules() %>% set_engine("Cubist")
            #     ) %>%
            #     add_recipe(recipe_spec_3_alt)
            # 
            # # + Cubist 4 ----
            # wflw_spec_cubist_4 <- workflow() %>%
            #     add_model(
            #         cubist_rules() %>% set_engine("Cubist")
            #     ) %>%
            #     add_recipe(recipe_spec_4_alt)
            
            # + Cubist - best tuned for air passengers ----
            wflw_spec_cubist_airpassengers <- workflow() %>%
                add_model(
                    rules::cubist_rules(
                        committees = 49,
                        neighbors  = 6,
                        max_rules  = 363
                    ) %>%
                        set_engine('Cubist')
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + Cubist - best tuned for housing ----
            wflw_spec_cubist_housing <- workflow() %>%
                add_model(
                    rules::cubist_rules(
                        committees = 19,
                        neighbors  = 5,
                        max_rules  = 63
                    ) %>%
                        set_engine('Cubist')
                ) %>%
                add_recipe(recipe_spec_1_alt)
            
            # + Cubist - best tuned for pump prices ----
            wflw_spec_cubist_pumpprices <- workflow() %>%
                add_model(
                    rules::cubist_rules(
                        committees = 45,
                        neighbors  = 8,
                        max_rules  = 449
                    ) %>%
                        set_engine('Cubist')
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + Cubist - best tuned for stocks ----
            wflw_spec_cubist_stocks <- workflow() %>%
                add_model(
                    rules::cubist_rules(
                        committees = 68,
                        neighbors  = 5,
                        max_rules  = 493
                    ) %>%
                        set_engine('Cubist')
                ) %>%
                add_recipe(recipe_spec_2_alt)
            
            # # + GLMNET 1 ----
            # wflw_spec_glm_1 <- workflow() %>%
            #     add_model(
            #         spec = linear_reg(mode = "regression",
            #                           penalty = 0.01,
            #                           mixture = 0.5) %>% set_engine("glmnet")
            #     ) %>%
            #     add_recipe(recipe_spec_1_alt)
            # 
            # # + GLMNET 2 ----
            # wflw_spec_glm_2 <- workflow() %>%
            #     add_model(
            #         spec = linear_reg(mode = "regression",
            #                           penalty = 0.01,
            #                           mixture = 0.5) %>% set_engine("glmnet")
            #     ) %>%
            #     add_recipe(recipe_spec_2_alt)
            # 
            # # + GLMNET 3 ----
            # wflw_spec_glm_3 <- workflow() %>%
            #     add_model(
            #         spec = linear_reg(mode = "regression",
            #                           penalty = 0.01,
            #                           mixture = 0.5) %>% set_engine("glmnet")
            #     ) %>%
            #     add_recipe(recipe_spec_3_alt)
            # 
            # # + GLMNET 4 ----
            # wflw_spec_glm_4 <- workflow() %>%
            #     add_model(
            #         spec = linear_reg(mode = "regression",
            #                           penalty = 0.01,
            #                           mixture = 0.5) %>% set_engine("glmnet")
            #     ) %>%
            #     add_recipe(recipe_spec_4_alt)
            
            # + GLMNET - best tuned for airpassengers ----
            wflw_spec_glm_airpassengers <- workflow() %>%
                add_model(
                    spec = linear_reg(mode = "regression",
                                      penalty = 0.000000882,
                                      mixture = 0.875) %>% set_engine("glmnet")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # + GLMNET - best tuned for housing ----
            wflw_spec_glm_housing <- workflow() %>%
                add_model(
                    spec = linear_reg(mode = "regression",
                                      penalty = 0.0000000942,
                                      mixture = 0.112) %>% set_engine("glmnet")
                ) %>%
                add_recipe(recipe_spec_2_alt)
            
            # + GLMNET - best tuned for pump prices ----
            wflw_spec_glm_pumpprices <- workflow() %>%
                add_model(
                    spec = linear_reg(mode = "regression",
                                      penalty = 0.0000000240,
                                      mixture = 0.932) %>% set_engine("glmnet")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + GLMNET - best tuned for stocks ----
            # hetzelfde als voor de pump prices
            
            # # + KNN 1 ----
            # wflw_spec_knn_1 <- workflow() %>%
            #     add_model(
            #         nearest_neighbor() %>% set_engine("kknn")
            #     ) %>%
            #     add_recipe(recipe_spec_1_alt)
            # 
            # # + KNN 2 ----
            # wflw_spec_knn_2 <- workflow() %>%
            #     add_model(
            #         nearest_neighbor() %>% set_engine("kknn")
            #     ) %>%
            #     add_recipe(recipe_spec_2_alt)
            # 
            # # + KNN 3 ----
            # wflw_spec_knn_3 <- workflow() %>%
            #     add_model(
            #         nearest_neighbor() %>% set_engine("kknn")
            #     ) %>%
            #     add_recipe(recipe_spec_3_alt)
            # 
            # # + KNN 4 ----
            # wflw_spec_knn_4 <- workflow() %>%
            #     add_model(
            #         nearest_neighbor() %>% set_engine("kknn")
            #     ) %>%
            #     add_recipe(recipe_spec_4_alt)
            
            # + KNN - best tuned for airpassengers ----
            wflw_spec_knn_airpassengers <- workflow() %>%
                add_model(
                    nearest_neighbor(
                        mode        = "regression",
                        neighbors   = 4,
                        weight_func = "cos",
                        dist_power  = 0.336
                    ) %>% set_engine("kknn")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + KNN - best tuned for housing ----
            wflw_spec_knn_housing <- workflow() %>%
                add_model(
                    nearest_neighbor(
                        mode        = "regression",
                        neighbors   = 2,
                        weight_func = "epanechnikov",
                        dist_power  = 0.598
                    ) %>% set_engine("kknn")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + KNN - best tuned for pump prices ----
            wflw_spec_knn_pumpprices <- workflow() %>%
                add_model(
                    nearest_neighbor(
                        mode        = "regression",
                        neighbors   = 14,
                        weight_func = "rank",
                        dist_power  = 0.670
                    ) %>% set_engine("kknn")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # + KNN - best tuned for stocks ----
            wflw_spec_knn_stocks <- workflow() %>%
                add_model(
                    nearest_neighbor(
                        mode        = "regression",
                        neighbors   = 3,
                        weight_func = "inv",
                        dist_power  = 0.993
                    ) %>% set_engine("kknn")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # # + MARS 1 ----
            # wflw_spec_mars_1 <- workflow() %>%
            #     add_model(
            #         spec = mars() %>% set_engine("earth")
            #     ) %>%
            #     add_recipe(recipe_spec_1_alt)
            # 
            # # + MARS 2 ----
            # wflw_spec_mars_2 <- workflow() %>%
            #     add_model(
            #         spec = mars() %>% set_engine("earth")
            #     ) %>%
            #     add_recipe(recipe_spec_2_alt)
            # 
            # # + MARS 3 ----
            # wflw_spec_mars_3 <- workflow() %>%
            #     add_model(
            #         spec = mars() %>% set_engine("earth")
            #     ) %>%
            #     add_recipe(recipe_spec_3_alt)
            # 
            # # + MARS 4 ----
            # wflw_spec_mars_4 <- workflow() %>%
            #     add_model(
            #         spec = mars() %>% set_engine("earth")
            #     ) %>%
            #     add_recipe(recipe_spec_4_alt)
            
            # + MARS - best tuned for airpassengers ----
            wflw_spec_mars_airpassengers <- workflow() %>%
                add_model(
                    spec = mars(prod_degree = 1) %>% set_engine("earth")
                ) %>%
                add_recipe(recipe_spec_1_alt)
            
            # + MARS - best tuned for housing ----
            wflw_spec_mars_housing <- workflow() %>%
                add_model(
                    spec = mars(prod_degree = 2) %>% set_engine("earth")
                ) %>%
                add_recipe(recipe_spec_1_alt)
            
            # + MARS - best tuned for pump prices ----
            # gelijk aan tuned for housing
            
            # + MARS - best tuned for stocks ----
            # gelijk aan tuned for housing
            
            # # + Prophet Boost 1 ----
            # wflw_spec_prophet_boost_1 <- workflow() %>%
            #     add_model(
            #         prophet_boost(
            #             seasonality_daily  = FALSE,
            #             seasonality_weekly = FALSE,
            #             seasonality_yearly = FALSE
            #         ) %>%
            #             set_engine("prophet_xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_1)
            # 
            # # + Prophet Boost 2 ----
            # wflw_spec_prophet_boost_2 <- workflow() %>%
            #     add_model(
            #         prophet_boost(
            #             seasonality_daily  = FALSE,
            #             seasonality_weekly = FALSE,
            #             seasonality_yearly = FALSE
            #         ) %>%
            #             set_engine("prophet_xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_2)
            # 
            # # + Prophet Boost 3 ----
            # wflw_spec_prophet_boost_3 <- workflow() %>%
            #     add_model(
            #         prophet_boost(
            #             seasonality_daily  = FALSE,
            #             seasonality_weekly = FALSE,
            #             seasonality_yearly = FALSE
            #         ) %>%
            #             set_engine("prophet_xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_3)
            # 
            # # + Prophet Boost 4 ----
            # wflw_spec_prophet_boost_4 <- workflow() %>%
            #     add_model(
            #         prophet_boost(
            #             seasonality_daily  = FALSE,
            #             seasonality_weekly = FALSE,
            #             seasonality_yearly = FALSE
            #         ) %>%
            #             set_engine("prophet_xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_4)
            
            # + Prophet Boost - best tuned for airpassengers ----
            wflw_spec_prophet_boost_airpassengers <- workflow() %>%
                add_model(
                    modeltime::prophet_boost(
                        seasonality_daily        = FALSE,
                        seasonality_weekly       = FALSE,
                        seasonality_yearly       = FALSE,
                        growth                   = "linear",
                        season                   = "multiplicative",
                        prior_scale_changepoints = 13.9,
                        prior_scale_seasonality  = 0.00248,
                        prior_scale_holidays     = 88.7,
                        tree_depth               = 9,
                        trees                    = 757,
                        learn_rate               = 0.0337,
                        mtry                     = 20,
                        min_n                    = 3,
                        loss_reduction           = 0.00750,
                        sample_size              = 1,
                        stop_iter                = 14
                    ) %>%
                        set_engine("prophet_xgboost")
                ) %>%
                add_recipe(recipe_spec_4)
            
            # + Prophet Boost - best tuned for housing ----
            wflw_spec_prophet_boost_housing <- workflow() %>%
                add_model(
                    modeltime::prophet_boost(
                        seasonality_daily        = FALSE,
                        seasonality_weekly       = FALSE,
                        seasonality_yearly       = FALSE,
                        growth                   = "linear",
                        season                   = "multiplicative",
                        prior_scale_changepoints = 13.9,
                        prior_scale_seasonality  = 0.00248,
                        prior_scale_holidays     = 88.7,
                        tree_depth               = 9,
                        trees                    = 757,
                        learn_rate               = 0.0337,
                        mtry                     = 14,
                        min_n                    = 3,
                        loss_reduction           = 0.00750,
                        sample_size              = 1,
                        stop_iter                = 14
                    ) %>%
                        set_engine("prophet_xgboost")
                ) %>%
                add_recipe(recipe_spec_2)
            
            # + Prophet Boost - best tuned for pump prices ----
            wflw_spec_prophet_boost_pumpprices <- workflow() %>%
                add_model(
                    modeltime::prophet_boost(
                        seasonality_daily        = FALSE,
                        seasonality_weekly       = FALSE,
                        seasonality_yearly       = FALSE,
                        growth                   = "linear",
                        season                   = "additive",
                        prior_scale_changepoints = 2.60,
                        prior_scale_seasonality  = 0.0168,
                        prior_scale_holidays     = 0.0559,
                        tree_depth               = 12,
                        trees                    = 987,
                        learn_rate               = 0.00569,
                        mtry                     = 16,
                        min_n                    = 11,
                        loss_reduction           = 0.0000000950,
                        sample_size              = 1,
                        stop_iter                = 17
                    ) %>%
                        set_engine("prophet_xgboost")
                ) %>%
                add_recipe(recipe_spec_1)
            
            # + Prophet Boost - best tuned for stocks ----
            wflw_spec_prophet_boost_stocks <- workflow() %>%
                add_model(
                    modeltime::prophet_boost(
                        seasonality_daily        = FALSE,
                        seasonality_weekly       = FALSE,
                        seasonality_yearly       = FALSE,
                        growth                   = "linear",
                        season                   = "additive",
                        prior_scale_changepoints = 2.60,
                        prior_scale_seasonality  = 0.0168,
                        prior_scale_holidays     = 0.0559,
                        tree_depth               = 12,
                        trees                    = 987,
                        learn_rate               = 0.00569,
                        mtry                     = 10,
                        min_n                    = 11,
                        loss_reduction           = 0.0000000950,
                        sample_size              = 1,
                        stop_iter                = 17
                    ) %>%
                        set_engine("prophet_xgboost")
                ) %>%
                add_recipe(recipe_spec_3)
            
            # # + Random Forest - Ranger 1 ----
            # wflw_spec_rf_1 <- workflow() %>%
            #     add_model(
            #         rand_forest() %>% set_engine("ranger")
            #     ) %>%
            #     add_recipe(recipe_spec_1_alt)
            # 
            # # + Random Forest - Ranger 2 ----
            # wflw_spec_rf_2 <- workflow() %>%
            #     add_model(
            #         rand_forest() %>% set_engine("ranger")
            #     ) %>%
            #     add_recipe(recipe_spec_2_alt)
            # 
            # # + Random Forest - Ranger 3 ----
            # wflw_spec_rf_3 <- workflow() %>%
            #     add_model(
            #         rand_forest() %>% set_engine("ranger")
            #     ) %>%
            #     add_recipe(recipe_spec_3_alt)
            # 
            # # + Random Forest - Ranger 4 ----
            # wflw_spec_rf_4 <- workflow() %>%
            #     add_model(
            #         rand_forest() %>% set_engine("ranger")
            #     ) %>%
            #     add_recipe(recipe_spec_4_alt)
            
            # + Random Forest - best tuned for airpassengers ----
            wflw_spec_rf_airpassengers <- workflow() %>%
                add_model(
                    spec = rand_forest(mtry = 20,
                                       min_n = 3) %>% set_engine("ranger")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + Random Forest - best tuned for housing ----
            wflw_spec_rf_housing <- workflow() %>%
                add_model(
                    spec = rand_forest(mtry = 11,
                                       min_n = 3) %>% set_engine("ranger")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + Random Forest - best tuned for pump prices ----
            wflw_spec_rf_pumpprices <- workflow() %>%
                add_model(
                    spec = rand_forest(mtry = 33,
                                       min_n = 3) %>% set_engine("ranger")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + Random Forest - best tuned for stocks ----
            wflw_spec_rf_stocks <- workflow() %>%
                add_model(
                    spec = rand_forest(mtry = 25,
                                       min_n = 3) %>% set_engine("ranger")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # # + SVM 1 ----
            # wflw_spec_svm_1 <- workflow() %>%
            #     add_model(
            #         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
            #     ) %>%
            #     add_recipe(recipe_spec_1_alt)
            # 
            # # + SVM 2 ----
            # wflw_spec_svm_2 <- workflow() %>%
            #     add_model(
            #         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
            #     ) %>%
            #     add_recipe(recipe_spec_2_alt)
            # 
            # # + SVM 3 ----
            # wflw_spec_svm_3 <- workflow() %>%
            #     add_model(
            #         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
            #     ) %>%
            #     add_recipe(recipe_spec_3_alt)
            # 
            # # + SVM 4 ----
            # wflw_spec_svm_4 <- workflow() %>%
            #     add_model(
            #         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
            #     ) %>%
            #     add_recipe(recipe_spec_4_alt)
            
            # + SVM RBF - best tuned for airpassengers ----
            wflw_spec_svm_airpassengers <- workflow() %>%
                add_model(
                    spec = svm_rbf(
                        mode      = "regression",
                        cost      = 10.5,
                        rbf_sigma = 0.0824,
                        margin    = 0.0600
                    ) %>% 
                        set_engine("kernlab")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # + SVM RBF - best tuned for housing ----
            wflw_spec_svm_housing <- workflow() %>%
                add_model(
                    spec = svm_rbf(
                        mode      = "regression",
                        cost      = 3.78,
                        rbf_sigma = 0.00771,
                        margin    = 0.0797
                    ) %>% 
                        set_engine("kernlab")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + SVM RBF - best tuned for pump prices ----
            wflw_spec_svm_pumpprices <- workflow() %>%
                add_model(
                    spec = svm_rbf(
                        mode      = "regression",
                        cost      = 3.78,
                        rbf_sigma = 0.00771,
                        margin    = 0.0797
                    ) %>% 
                        set_engine("kernlab")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # + SVM RBF - best tuned for stocks ----
            wflw_spec_svm_stocks <- workflow() %>%
                add_model(
                    spec = svm_rbf(
                        mode      = "regression",
                        cost      = 4.29,
                        rbf_sigma = 0.210,
                        margin    = 0.0161
                    ) %>% 
                        set_engine("kernlab")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # # + XGBoost 1 ----
            # wflw_spec_xgboost_1 <- workflow() %>%
            #     add_model(
            #         boost_tree() %>% set_engine("xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_1_alt)
            # 
            # # + XGBoost 2 ----
            # wflw_spec_xgboost_2 <- workflow() %>%
            #     add_model(
            #         boost_tree() %>% set_engine("xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_2_alt)
            # 
            # # + XGBoost 3 ----
            # wflw_spec_xgboost_3 <- workflow() %>%
            #     add_model(
            #         boost_tree() %>% set_engine("xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_3_alt)
            # 
            # # + XGBoost 4 ----
            # wflw_spec_xgboost_4 <- workflow() %>%
            #     add_model(
            #         boost_tree() %>% set_engine("xgboost")
            #     ) %>%
            #     add_recipe(recipe_spec_4_alt)
            
            # + XGBoost - best tuned for airpassengers ----
            wflw_spec_xgboost_airpassengers <- workflow() %>%
                add_model(
                    boost_tree(
                        trees          = 530,
                        min_n          = 10,
                        tree_depth     = 15,
                        learn_rate     = 0.0446,
                        loss_reduction = 0.000373,
                        sample_size    = 0.311,
                        stop_iter      = 18
                    ) %>% set_engine("xgboost")
                ) %>%
                add_recipe(recipe_spec_4_alt)
            
            # + XGBoost - best tuned for housing ----
            wflw_spec_xgboost_housing <- workflow() %>%
                add_model(
                        boost_tree(
                            trees          = 530,
                            min_n          = 10,
                            tree_depth     = 15,
                            learn_rate     = 0.0446,
                            loss_reduction = 0.000373,
                            sample_size    = 0.311,
                            stop_iter      = 18
                    ) %>% set_engine("xgboost")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # + XGBoost - best tuned for pump prices ----
            wflw_spec_xgboost_pumpprices <- workflow() %>%
                add_model(
                    boost_tree(
                        trees          = 889,
                        min_n          = 13,
                        tree_depth     = 9,
                        learn_rate     = 0.0390,
                        loss_reduction = 0.000000850,
                        sample_size    = 0.260,
                        stop_iter      = 13
                    ) %>% set_engine("xgboost")
                ) %>%
                add_recipe(recipe_spec_3_alt)
            
            # + XGBoost - best tuned for stocks ----
            wflw_spec_xgboost_stocks <- workflow() %>%
                add_model(
                    boost_tree(
                        trees          = 889,
                        min_n          = 13,
                        tree_depth     = 9,
                        learn_rate     = 0.0390,
                        loss_reduction = 0.000000850,
                        sample_size    = 0.260,
                        stop_iter      = 13
                    ) %>% set_engine("xgboost")
                ) %>%
                add_recipe(recipe_spec_1_alt)
            
            
            # * SUBMODEL SPEC SELECTION ----
            spec_table <- tibble(
                wflw_spec = list(
                    wflw_spec_cubist_airpassengers,
                    wflw_spec_cubist_housing,
                    wflw_spec_cubist_pumpprices,
                    wflw_spec_cubist_stocks,
                    wflw_spec_glm_airpassengers,
                    wflw_spec_glm_housing,
                    wflw_spec_glm_pumpprices,
                    wflw_spec_knn_airpassengers,
                    wflw_spec_knn_housing,
                    wflw_spec_knn_pumpprices,
                    wflw_spec_knn_stocks,
                    wflw_spec_mars_airpassengers,
                    wflw_spec_mars_housing,
                    wflw_spec_prophet_boost_airpassengers,
                    wflw_spec_prophet_boost_housing,
                    wflw_spec_prophet_boost_pumpprices,
                    wflw_spec_prophet_boost_stocks,
                    wflw_spec_rf_airpassengers,
                    wflw_spec_rf_housing,
                    wflw_spec_rf_pumpprices,
                    wflw_spec_rf_stocks,
                    wflw_spec_svm_airpassengers,
                    wflw_spec_svm_housing,
                    wflw_spec_svm_pumpprices,
                    wflw_spec_svm_stocks,
                    wflw_spec_xgboost_airpassengers,
                    wflw_spec_xgboost_housing,
                    wflw_spec_xgboost_pumpprices,
                    wflw_spec_xgboost_stocks
                )
            )

            # * SAFE FITTING ----
            # - Models can fail in production. Don't include them if they fail.
            message("Fitting Sub-Models")

            tictoc::tic()
            fitted_wflw_list <- map(spec_table$wflw_spec, .f = function(wflw) {
                res <- wflw %>% fit(rv$train_cleaned)
                return(res)
            })
            tictoc::toc()

            spec_fit_tbl <- spec_table %>%
                mutate(wflw_fit = fitted_wflw_list)

            # * SUBMODELS ----
            # - Add models that passed to Modeltime Table

            rv$submodels_tbl <- as_modeltime_table(spec_fit_tbl$wflw_fit)

            rv$submodels_calib_tbl <- rv$submodels_tbl %>%
                modeltime_calibrate(testing(rv$splits))

            rv$submodels_tbl <- NULL

            rv$submodels_accuracy_tbl <- rv$submodels_calib_tbl %>%
                mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
                    tbl %>%
                        mutate(
                            .actual     = .actual %>% trans_fun_inv(),
                            .prediction = .prediction %>% trans_fun_inv(),
                            .residuals  = .actual - .prediction
                        )
                })) %>%
                modeltime_accuracy(metric_set = metric_set(mae, smape, rmse, rsq))

            print(rv$submodels_accuracy_tbl %>% arrange(rmse))

            # * ENSEMBLE ----
            # - Apply Exponential & Average Strategy
            message("Selecting Best Ensemble")
            
            rv$bestinclass_models <- rv$submodels_accuracy_tbl %>% 
                group_by(.model_desc) %>% 
                summarise(rmse = min(rmse)) %>% 
                ungroup() %>%
                left_join(rv$submodels_accuracy_tbl) %>%
                group_by(.model_desc) %>% 
                summarise(mae = min(mae)) %>% 
                ungroup() %>%
                left_join(rv$submodels_accuracy_tbl) %>%
                group_by(.model_desc) %>% 
                summarise(rsq = max(rsq)) %>% 
                ungroup() %>%
                left_join(rv$submodels_accuracy_tbl) %>%
                group_by(.model_desc) %>% 
                summarise(.model_id = min(.model_id)) %>% 
                ungroup() %>%
                left_join(rv$submodels_accuracy_tbl) %>% 
                pull(.model_id)
            
            rv$submodels_for_ensemble <- rv$submodels_accuracy_tbl %>% 
                filter(.model_id %in% rv$bestinclass_models) %>% 
                filter(rmse < 1.5*mean(rmse)) %>% # remove extreme values
                filter(rmse < mean(rmse)) %>%
                # filter(rmse < 0.9*mean(rv$submodels_accuracy_tbl$rmse)) %>% 
                # filter(rmse < 1.3*min(rv$submodels_accuracy_tbl$rmse)) %>%
                pull(.model_id)
            
            rv$submodels_calib_tbl <- rv$submodels_calib_tbl %>% 
                filter(.model_id %in% rv$bestinclass_models)
            
            # average ensemble
            rv$mean_ensemble_calib_tbl <- modeltime_table(
                ensemble_average(rv$submodels_calib_tbl %>% filter(.model_id %in% rv$submodels_for_ensemble),
                                 type = "mean")
            ) %>%
                modeltime_calibrate(testing(rv$splits))
            
            rv$mean_ensemble_accuracy_tbl <- rv$mean_ensemble_calib_tbl %>%
                mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
                    tbl %>%
                        mutate(
                            .actual     = .actual %>% trans_fun_inv(),
                            .prediction = .prediction %>% trans_fun_inv(),
                            .residuals  = .actual - .prediction
                        )
                })) %>%
                modeltime_accuracy() %>%
                select(.model_id, .model_desc, mae, rmse, rsq)
            
            print(rv$mean_ensemble_accuracy_tbl)
            
            # loadings table
            rv$loadings_tbl <- rv$submodels_accuracy_tbl %>% 
                filter(.model_id %in% rv$submodels_for_ensemble) %>% 
                arrange(desc(rmse)) %>% 
                rowid_to_column() %>% 
                mutate(loadings = exp(rowid),
                       loadings_2 = 2^rowid) %>% 
                mutate(loadings = loadings/sum(loadings),
                       loadings_2 = loadings_2/sum(loadings_2)) %>% 
                select(.model_id, loadings, everything())
            
            # weighted ensemble
            rv$weighted_ensemble_calib_tbl <- modeltime_table(
                ensemble_weighted(rv$submodels_calib_tbl %>% filter(.model_id %in% rv$submodels_for_ensemble),
                                  loadings = rv$loadings_tbl$loadings)
            ) %>%
                modeltime_calibrate(testing(rv$splits))
            
            rv$weighted_ensemble_accuracy_tbl <- rv$weighted_ensemble_calib_tbl %>%
                mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
                    tbl %>%
                        mutate(
                            .actual     = .actual %>% trans_fun_inv(),
                            .prediction = .prediction %>% trans_fun_inv(),
                            .residuals  = .actual - .prediction
                        )
                })) %>%
                modeltime_accuracy() %>%
                select(.model_id, .model_desc, mae, rmse, rsq)
            
            print(rv$weighted_ensemble_accuracy_tbl)
            
            # weighted ensemble 2
            rv$weighted_2_ensemble_calib_tbl <- modeltime_table(
                ensemble_weighted(rv$submodels_calib_tbl %>% filter(.model_id %in% rv$submodels_for_ensemble),
                                  loadings = rv$loadings_tbl$loadings_2)
            ) %>%
                modeltime_calibrate(testing(rv$splits))

            rv$ensemble_accuracy_tbl <- rv$weighted_2_ensemble_calib_tbl %>%
                mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
                    tbl %>%
                        mutate(
                            .actual     = .actual %>% trans_fun_inv(),
                            .prediction = .prediction %>% trans_fun_inv(),
                            .residuals  = .actual - .prediction
                        )
                })) %>%
                modeltime_accuracy() %>%
                select(.model_id, .model_desc, mae, rmse, rsq)

            print(rv$ensemble_accuracy_tbl)
            

            # final calibration
            rv$calibration_tbl <-
                combine_modeltime_tables(rv$submodels_calib_tbl %>% filter(.model_id %in% rv$submodels_for_ensemble), 
                                         rv$mean_ensemble_calib_tbl,
                                         rv$weighted_ensemble_calib_tbl,
                                         rv$weighted_2_ensemble_calib_tbl) %>%
                modeltime_calibrate(testing(rv$splits), id = "id")
            rv$accuracy_tbl <- rv$calibration_tbl %>%
                mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
                    tbl %>%
                        mutate(
                            .actual     = .actual %>% trans_fun_inv(),
                            .prediction = .prediction %>% trans_fun_inv(),
                            .residuals  = .actual - .prediction
                        )
                })) %>%
                modeltime_accuracy(acc_by_id = TRUE) %>%
                select(id, .model_id, .model_desc, mae, rmse, rsq) %>%
                arrange(rmse)

            rv$best_model <- rv$accuracy_tbl %>% 
                filter(grepl(pattern = "ENSEMBLE", x = .model_desc)) %>% 
                dplyr::slice(1) %>% 
                pull(.model_id)

            # ** REFIT ----
            message("Refitting on Full Dataset")

            rv$refit_tbl <- rv$calibration_tbl %>%
                # filter(.model_id == rv$best_model) %>%
                modeltime_refit(rv$data_prepared_tbl)

            rv$future_forecast_tbl <- rv$refit_tbl %>%
                modeltime_forecast(
                    new_data    = rv$future_tbl,
                    actual_data = rv$actual_data,
                    keep_data   = TRUE
                ) %>%
                mutate(
                    across(.cols = c(.value, .conf_lo, .conf_hi),
                           .fns  = function(x) trans_fun_inv(x)
                           # .fns  = function(x) scale_fun_inv(trans_fun_inv(x))
                           )
                ) %>%
                group_by(id)

            message("\nDone!")

        })

        # 9.1 user inputs ----
        # # 9.1.1 info for user ----
        # output$info_freq <- renderText(str_c("Frequency: ", rv$ts_frequency))
        # output$info_scale <- renderText(str_c("scale: ", rv$ts_scale))
        # output$info_rec_horizon <- renderText(str_c("Recommended forecast horizon: ", rv$horizon_recommended))
        # output$info_log1p <- renderText(str_c("Recommended forecast horizon: ", rv$horizon_recommended))
        # output$info_centered <- renderText(str_c("Recommended forecast horizon: ", rv$horizon_recommended))
        # output$scaled <- renderText(str_c("Recommended forecast horizon: ", rv$horizon_recommended))
        # output$cleaned <- renderText(str_c("Recommended forecast horizon: ", rv$horizon_recommended))

        # 9.1.2 forcast horizon, lags & fourier input ----
        output$forecast_choices <- renderUI({
            div(
                numericInputIcon(
                    inputId = "horizon",
                    value   = rv$horizon_recommended,
                    min     = 2,
                    label   = "Enter a Forecast Horizon",
                    icon    = icon("chart-line")
                    ),
                p(str_glue("We recommend forecasting {rv$horizon_recommended} periods.")),
                br(),
                strong("Transformation & Cleaning"),
                materialSwitch(
                    inputId  = "sel_log1p",
                    label    = "Log transformation OFF/ON",
                    value    = TRUE,
                    status   = "succes"
                    ),
                br(),
                materialSwitch(
                    inputId  = "sel_cleaning",
                    label    = "Clean outliers OFF/ON",
                    value    = TRUE,
                    status   = "succes"
                    )
                )
        })
        
        output$accuracy_tbl <- 
            # renderDT({
            #     req(rv$accuracy_tbl)
            #     datatable(rv$accuracy_tbl %>%
            #                   # left_join(rv$loadings_tbl %>% select(-.model_id)) %>%
            #                   select(id, .model_id, .model_desc, mae, rmse, rsq) %>% 
            #                   group_by(id) %>% 
            #                   arrange(id, rmse),
            #               rownames = FALSE,
            #               options  = list(dom = 't')
            #               ) %>%
            #         formatRound(columns = 4:6,
            #                     digits  = 2)
            # })
            renderReactable({
                req(rv$accuracy_tbl)
                
                rv$accuracy_tbl %>%
                    group_by(id) %>%
                    # select(id, .model_id, .model_desc, mae, rmse, rsq) %>%
                    table_modeltime_accuracy(
                          # rownames = FALSE,
                          # options  = list(dom = 't')
                )
            })

        # 9.2 forecast plot ----
        output$forecast_plot <- renderPlotly({
            req(rv$future_forecast_tbl)

            rv$future_forecast_tbl %>%
                filter(id %in% input$sel_vars_load) %>% 
                plot_modeltime_forecast(.facet_ncol = 2,
                                        # .color_var = .model_id,
                                        .conf_interval_show = FALSE)
        })
            
    }
    
    # Run the application
    shinyApp(ui = ui, server = server)