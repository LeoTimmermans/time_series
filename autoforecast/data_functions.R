# DATA FUNCTIONS ----
# 
# library(timetk)
# library(tidyquant)
# library(tidyverse)

# FUNCTIONS -----
get_dow_data <- function() {
    dow_raw_tbl <- NULL
    symbols <- tq_index("DOW") %>% pull(symbol) %>% sort()
    
            for (i in symbols) {
                stock_tbl <- tq_get(i)
                dow_raw_tbl <- dow_raw_tbl %>% bind_rows(stock_tbl)
            }
    
        selected_stocks <- dow_raw_tbl %>% 
            group_by(symbol) %>% 
            summarise(date = last(date),
                      volume = last(volume),
                      adjusted = last(adjusted)) %>% 
            ungroup() %>% 
            mutate(trade_value = volume*adjusted) %>% 
            arrange(desc(trade_value)) %>% 
            dplyr::slice(1:6) %>% 
            pull(symbol)

        dow_raw_tbl %>% 
            dplyr::filter(symbol %in% selected_stocks) %>% 
            select(date, symbol, adjusted) %>% 
            distinct() %>% 
                group_by(symbol) %>% 
                summarise_by_time(.date_var = date,
                                  adjusted  = last(adjusted),
                                  .type     = "ceiling",
                                  .by       = "week") %>% 
                ungroup() %>% 
            pivot_wider(names_from  = 1, 
                        values_from = 3, 
                        values_fn  = {mean})
}


select_dataset <- function(data, choice = "cbs_pump_prices_tbl", show_message = TRUE) {
    
    data_filtered <- data %>%
        filter(dataset_id == choice)
    
    if (show_message) {
        message(stringr::str_glue(
            "Selected: {data_filtered$dataset_id}
        Description: {data_filtered$desc}"
        ))
    }
    
    return(data_filtered$data[[1]])
    
}

get_dataset_description <- function(data, choice = "cbs_pump_prices_tbl") {
    data_filtered <- data %>%
        filter(dataset_id == choice)
    
    return(data_filtered$desc[[1]])
}

get_dataset_column_names <- function(data) {
    data %>% colnames()
}