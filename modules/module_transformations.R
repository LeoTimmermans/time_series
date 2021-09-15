# module for transforming data

# UI
mod_transform_dataset_ui <- function(id, title = "Transformations") {
    
    # create namespace to prevent namespace collisions
    ns <- NS(id)
    
    # 2. TRANSFORMATIONS ----
    # mod_pick_dataset_ui(id = "transformations", title = "Transformations"),
    tabPanel(title = "Transformations",
             id = ns("transformations"),
             fluidRow(
                 # 2.1 user inputs ----
                 column(width = 4,
                        wellPanel(
                            pickerInput(
                                inputId  = "sel_transformation",
                                label    = "Select a transformation option",
                                choices  = choices_trans_vec,
                                selected = "no transformation",
                                multiple = FALSE
                            ),
                            pickerInput(
                                inputId  = "sel_scaling",
                                label    = "Select a scaling option",
                                choices  = choices_scale_vec,
                                selected = "no scaling",
                                multiple = FALSE
                            ),
                            
                            actionButton(
                                inputId = "save_transformations_btn",
                                icon    = icon("cloud-upload-alt"),
                                label   = "Save Transformations"
                            )
                        )),
                 # 2.2 directions for user ----
                 column(width = 8,
                        h3("How to pick transformation and scaling options"),
                        p("text here"),
                        p("Log transformation can be done on the whole dataset. Centering and scaling must be done by group.
                          Centering sets the mean to 0 and scaling divides the values by the standard deviation. 
                          This should be done for each group, because the groups have different means and standard deviations."),
                        p(HTML(str_c("Advise on transformation and scaling, based on models to be used, is found ", 
                                     a("here", href = "https://www.tmwr.org/pre-proc-table.html"), ".",
                                     br(),
                                     "Most model types do not need transformation or scaling.")))
                 )
             ),
             fluidRow(
                 # 2.3 no transformation and scaling ----
                 column(width = 6,
                        wellPanel(
                            h4("No transformation and scaling"),
                            plotlyOutput("no_transformation_plot")
                        )
                 ),
                 # 2.4 transformation and scaling ----
                 column(width = 6,
                        wellPanel(
                            h4("Including transformation and scaling"),
                            plotlyOutput("transformed_plot")
                        )
                 )
             )
    )
}


# SERVER
mod_transformations_server <- function(input, 
                                      output, 
                                      session, 
                                      dataset) {
    # 2. TRANSFORMATIONS ----
    # * REACTIVE VALUES
    observe(
        rv$temp <- rv$data %>%
            pivot_longer(cols     = -date,
                         names_to = "id")
    )
    observe(
        rv$data_long <- rv$data %>%
            pivot_longer(cols     = -date,
                         names_to = "id"))
    # * OBSERVER ----
    observeEvent(eventExpr = input$save_transformations_btn,
                 {
                     rv$transform <- input$sel_transformation
                     rv$scale     <- input$sel_scaling
                 },
                 ignoreNULL = FALSE)
    
    # * REACTIVE ----
    dataset_trans <- 
        reactive({
            dataset() %>% 
                pivot_longer(cols     = -date,
                             names_to = "id") %>% 
                mutate(value = case_when(
                    input$sel_transformation == "log1p" ~ log1p(value),
                    TRUE ~ value
                )) %>%
                group_by(id) %>%
                mutate(value = case_when(
                    input$sel_scaling == "center & scale" ~ scale(value, center = TRUE, scale = TRUE),
                    input$sel_scaling == "center" ~ scale(value, center = TRUE, scale = FALSE),
                    input$sel_scaling == "scale" ~ scale(value, center = FALSE, scale = TRUE),
                    TRUE ~ value
                ))  %>%
                ungroup()
        })
    
    # 2.1 user inputs ----
    # no server code
    
    # 2.2 directions for user ----
    # no server code
    
    # 2.3 no transformation and scaling ----
    output$no_transformation_plot <-
        renderPlotly(rv$data %>% plot_raw_data())
    
    # 2.4 transformation and scaling ----
    output$transformed_plot <-
        renderPlotly(dataset_trans() %>% plot_data_long())
}