plot_raw_data <- function(data, nfacets = 2, smoother = FALSE, plot_title = "time series plot") {
    data  %>%  
        pivot_longer(cols = -date,names_to = "id") %>% 
        group_by(id) %>% 
        timetk::plot_time_series(.date_var    = date,
                                 .value       = value,
                                 .facet_ncol  = nfacets,
                                 .smooth      = smoother,
                                 .title       = plot_title,
                                 .interactive = FALSE
        ) %>% 
        ggplotly(height = 600)
}

plot_data_long <- function(data, nfacets = 2, smoother = FALSE, plot_title = "time series plot") {
    data  %>%
        group_by(id) %>% 
        timetk::plot_time_series(.date_var   = date,
                                 .value      = value,
                                 .facet_ncol = nfacets,
                                 .smooth     = smoother,
                                 .title      = plot_title,
                                 .interactive = FALSE
        ) %>% 
        ggplotly(height = 600)
}

    