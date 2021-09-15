# module for dataset selection

# UI

mod_pick_dataset_ui <- function(id, title = "Pick Dataset") {
    
    # create namespace to prevent namespace collisions
    ns <- NS(id)
    
    # 1. SELECT DATASET & SHOW SUMMARY ----
    # 1.1 user inputs ----
    tabPanel(title = title,
             id = ns("pick_dataset"),
             fluidRow(
                 column(width = 4,
                        wellPanel(
                            pickerInput(inputId  = ns("sel_dataset"),
                                        label    = "Load a dataset",
                                        choices  = choices_vec,
                                        selected = "cbs_pump_prices_tbl",
                                        multiple = FALSE),
                            
                            actionButton(inputId = ns("sel_dataset_btn"),
                                         icon    = icon("database"),
                                         label   = "Load dataset")  
                        )
                 ),
                 # 1.2 dataset summary ----             
                 column(width = 8,
                        fluidRow(
                            h3("Time Series Information"),
                            hr(),
                            strong("Description"),
                            textOutput(ns("description")),
                            br(),
                            br()
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                strong("Dataset"),
                                textOutput(ns("dataset"))
                            ),
                            column(
                                width = 4,
                                strong("Time Scale"),
                                textOutput(ns("time_scale"))
                            ),
                            column(
                                width = 4,
                                strong("Number observations"),
                                textOutput(ns("nobs"))
                            )
                        )
                 )
             ),
             fluidRow(
                 # 1.3 time series plot ----
                 column(width = 6,
                        div(
                            h3("Time series plot:"),
                            wellPanel(plotlyOutput(ns("time_plot")))
                        )
                 ),
                 # 1.4 raw data table ----
                 column(width = 6,
                        div(
                            h3("Raw data:"),
                            wellPanel(DTOutput(ns("raw_data_tbl")))
                        )
                 )
             )
    )
}

# SERVER
mod_pick_dataset_server <- function(id) {
    moduleServer(id, function(input, output, session){
        # * Setup Reactive Values ----
        rv <- reactiveValues()
        
        # * OBSERVER ----
        observeEvent(eventExpr = input$sel_dataset_btn, {
            rv$current_selection <- input$sel_dataset
            rv$data              <- datasets_tbl %>% select_dataset(choice = input$sel_dataset)
            rv$desc              <- datasets_tbl %>% get_dataset_description(choice = input$sel_dataset)
            rv$data_colnames     <- colnames(rv$data)
            
            rv$group_count <- 1
            if (length(rv$data_colnames) == 3) {
                rv$group_count <- rv$data %>%
                    ungroup() %>%
                    pull(rv$data_colnames[1]) %>%
                    unique() %>%
                    length()
            }
            
            # Group, Date, Value ID's
            rv$group_name      <- NULL
            rv$group_name_expr <- NULL
            rv$groups_vec      <- NULL
            if (rv$group_count > 1) {
                rv$group_name      <- rv$data_colnames[1]
                rv$group_name_expr <- sym(rv$group_name)
                rv$groups_vec      <- rv$data %>% pull(!! sym(rv$group_name)) %>% unique() %>% as.character()
            }
            rv$date_name       <- rv$data %>% timetk::tk_get_timeseries_variables()
            rv$value_name      <- rv$data_colnames[!rv$data_colnames %in% c(rv$date_name, rv$group_name)]
            
            # Summary information
            rv$ts_summary_tbl <- rv$data %>%
                group_by(!! rv$group_name_expr) %>%
                tk_summary_diagnostics()
            
            rv$ts_scale <- rv$ts_summary_tbl$scale[[1]]
            
            print(rv$ts_scale)
            
            rv$median_nobs <- rv$ts_summary_tbl %>%
                pull(n.obs) %>%
                median()
            
            rv$lag_limit <- rv$median_nobs %>%
                `*`(0.4) %>%
                round()
            
            rv$horizon_recommended <- round(0.18 * rv$median_nobs)
            
        }, ignoreNULL = FALSE)
        
        # * REACTIVE ----
        dataset <- 
            reactive({
                datasets_tbl %>% 
                    select_dataset(choice = input$sel_dataset)
            })
    
        # 1.1 user inputs ----
        # no server code
        
        # 1.2 summary ----
        output$description <- renderText(rv$desc)
        output$dataset     <- renderText(rv$current_selection)
        output$time_scale  <- renderText(rv$ts_scale)
        output$nobs        <- renderText(format(rv$median_nobs,big.mark = ","))
        
        # 1.3 time series plot ----
        output$time_plot <- renderPlotly(
            rv$data %>% plot_raw_data()
        )
        
        # 1.3 raw data table ----
        output$raw_data_tbl <- 
            renderDT(
                datatable(data = rv$data,
                          rownames = FALSE ) %>% 
                    formatCurrency(columns  = 2:length(rv$data),
                                   mark     = ",",
                                   dec.mark = ".",
                                   currency = "")
            )
        
        return(dataset = reactive({dataset()}))
    })
    
}