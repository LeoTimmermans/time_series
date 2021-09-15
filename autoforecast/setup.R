# SETUP ----

library(tidyverse)
library(lubridate)


# DATA DESCRIPTIONS ----
description_tbl <-
    tribble(
        ~ dataset_id, ~ desc,
        "cbs_pump_prices_tbl", "Daily prices for different fuel types. Data source: https://opendata.cbs.nl/statline/#/CBS/en/dataset/80416ENG.",
        "cbs_housing_prices_tbl", "Housing prices in the Netherlands. Quarterly average housing prices per type. Date source: https://opendata.cbs.nl/statline/#/CBS/en/dataset/83910ENG.",
        "cbs_air_passengers_tbl", "Montly total passenger data (departures, arrivals & transit) for all Dutch Airports. Date source: https://opendata.cbs.nl/statline/#/CBS/en/dataset/37478ENG.",
        "dow_tbl", "(Adjusted) stock prices for DOW-Jones stocks. Last adjusted stock price per week to keep dataset small."
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
# * transformation choice ----
choices_trans_vec <- c("no transformation", "log1p")
# * scaling choice ----
choices_scale_vec <- c("no scaling", "center & scale", "center", "scale")