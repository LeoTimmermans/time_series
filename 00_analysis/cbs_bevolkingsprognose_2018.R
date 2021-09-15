# 0. LIBRARIES -----------------------------------------------------------------
# 0.1 retrieve data ----
library(cbsodataR)

# 0.2 EDA & data cleaning ----
library(DataExplorer)
library(timetk)
library(janitor)

# 0.3 GIS ----
library(geojsonio)
library(sf)

# 0.4 base packages ----
library(tidyverse)

# 0.5 productivity ----
library(targets)


# 1. SETTINGS ------------------------------------------------------------------
cat_tbl <- cbs_get_catalogs()
res_tbl <- cbs_search("Regionale prognose 2020-2050; bevolking, intervallen, regio-indeling 2018", 
                      language="nl", 
                      catalog = "CBS",
                      format = "datasets")
res_arb <- cbs_search("Sociale zekerheid; kerncijfers, uitkeringen naar uitkeringssoort", 
                      language="nl", 
                      catalog = "CBS",
                      format = "datasets")
res_vac <- cbs_search("Openstaande vacatures; SBI 2008, regio", 
                      language="nl", 
                      catalog = "CBS",
                      format = "datasets")


# 2. GET DATA ------------------------------------------------------------------
# 2.1 CBS bevolkinsprognose ----
# cbs_bevolkings_prognose_raw_tbl <- 
#     cbs_get_data("84527NED") %>% 
#     cbs_add_label_columns() %>% 
#     cbs_add_date_column() %>% 
#     clean_names()
# # * save raw data ----
# saveRDS(cbs_bevolkings_prognose_raw_tbl, 
#         "time_series/00_local_data/cbs_bevolkings_prognose_raw_tbl.RDS")
cbs_bevolkings_prognose_raw_tbl <- 
    readRDS("time_series/00_local_data/cbs_bevolkings_prognose_raw_tbl.RDS")

# 2.2 CBS regionale kerncijfers ----
# cbs_regionale_kerncijfers_raw_tbl <-
#     cbs_get_data("70072NED") %>%
#     cbs_add_label_columns() %>%
#     cbs_add_date_column() %>%
#     clean_names()
# # * save raw data ----
# saveRDS(cbs_regionale_kerncijfers_raw_tbl,
#         "time_series/00_local_data/cbs_regionale_kerncijfers_raw_tbl.RDS")
cbs_regionale_kerncijfers_raw_tbl <- 
    readRDS("time_series/00_local_data/cbs_regionale_kerncijfers_raw_tbl.RDS")

# 2.3 CBS tijdreeksen sociale zekerheid ----
cbs_tijdreeks_sz_raw_tbl <-
    cbs_get_data("03763") %>%
    cbs_add_label_columns() %>%
    cbs_add_date_column() %>%
    clean_names()

# 2.4 CBS vacatures SBI 2008 regio ----
cbs_regionale_vacatures_raw_tbl <-
  cbs_get_data("83599NED") %>%
  cbs_add_label_columns() %>%
  cbs_add_date_column() %>%
  clean_names()

# 2.3 gebiedsindelingen ----
# 2.3.1 codes en namen ----
# cbs_gebieden_2018_raw_tbl <- 
#     cbs_get_data("83859NED") %>% 
#     cbs_add_label_columns() %>% 
#     clean_names()
# saveRDS(cbs_gebieden_2018_raw_tbl, 
#         "time_series/00_local_data/cbs_gebieden_2018_raw_tbl.RDS")
cbs_gebieden_2018_raw_tbl <- 
    readRDS("time_series/00_local_data/cbs_gebieden_2018_raw_tbl.RDS")


# 2.3.2 geojson bestanden voor gebieden ----
# opzoeken via https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetCapabilities 
base_url <- "https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName="
download_format <- "&outputFormat=json"

# * corop-grenzen ----
# geoUrl <- paste0(base_url, "cbs_coropgebied_2018_gegeneraliseerd", download_format)
# fileName <- "gebiedsindelingen.geojson"
# download.file(geoUrl, fileName)
# coropgrenzen_2018 <- geojson_sf(fileName)
# saveRDS(coropgrenzen_2018, 
#         "time_series/00_local_data/coropgrenzen_2018.RDS")
coropgrenzen_2018 <- 
    readRDS("time_series/00_local_data/coropgrenzen_2018.RDS")

# * provinciegrenzen ----
# geoUrl <- paste0(base_url, "cbs_provincie_2018_gegeneraliseerd", download_format)
# fileName <- "gebiedsindelingen.geojson"
# download.file(geoUrl, fileName)
# provinciegrenzen_2018 <- geojson_sf(fileName)
# saveRDS(provinciegrenzen_2018, 
#         "time_series/00_local_data/provinciegrenzen_2018.RDS")
provinciegrenzen_2018 <- 
    readRDS("time_series/00_local_data/provinciegrenzen_2018.RDS")

# 3. EDA + DATA WRANGLING ------------------------------------------------------
# 3.1 CBS bevolkingsprognose ----
# 3.1.1 EDA ----
cbs_bevolkings_prognose_raw_tbl %>% glimpse()
cbs_bevolkings_prognose_raw_tbl %>% pull(regio_indeling2018_label) %>% unique()
DataExplorer::create_report(cbs_bevolkings_prognose_raw_tbl)
skimr::skim(cbs_bevolkings_prognose_raw_tbl)

# 3.1.2 wrangle dataset ----
# alleen corop, provincie, land behouden
cbs_bevolkings_prognose_tbl <- cbs_bevolkings_prognose_raw_tbl %>% 
    # alleen Nederland, provincies en corop-gbieden
    dplyr::filter(grepl("Nederland|(PV)|(CR)", regio_indeling2018_label)) %>% 
    # alleen de prognose en niet het betrouwbaarheidsinterval
    dplyr::filter(prognose_interval_label == "Prognose") %>% 
    # selecteer variabelen
    select(leeftijd_label, regio_indeling2018, regio_indeling2018_label,
           bevolking = totale_bevolking_1, perioden_date) %>% 
    # opschonen regio-code
    mutate(regio_indeling2018 = gsub(" ", "", regio_indeling2018))

# plot time series ----
cbs_bevolkings_prognose_tbl %>% 
    dplyr::filter(str_detect(regio_indeling2018, "CR")) %>% 
    group_by(leeftijd_label) %>% 
    # dplyr::filter(str_detect(leeftijd_label, "Totaal")) %>% 
    plot_time_series(
        .date_var = perioden_date,
        .value = bevolking,
        .color_var = regio_indeling2018_label,
        .smooth = FALSE,
        .facet_ncol = 2
    )
  
# 3.2 CBS regionale kerncijfers ----
# 3.2.1 EDA ----
cbs_regionale_kerncijfers_raw_tbl %>% glimpse()
cbs_regionale_kerncijfers_raw_tbl %>% pull(regio_s_label) %>% unique()
# DataExplorer::create_report(cbs_regionale_kerncijfers_raw_tbl)
skimr::skim(cbs_regionale_kerncijfers_raw_tbl)  

# 3.2.2 wrangle dataset ----
# alleen corop, provincie, landsdeel en land behouden
cbs_regionale_kerncijfers_tbl <- 
  cbs_regionale_kerncijfers_raw_tbl %>% 
  # alleen Nederland, landsdelen, provincies en corop-gbieden
  dplyr::filter(grepl("^NL|^LD|^PV", regio_s)) %>% 
  # variabelen selecteren
  select(regio_s, region = regio_s_label, date = perioden_date,
         inhabitants = totale_bevolking_1) %>% 
  # opschonen regio-code
  mutate(region_code = gsub(" ", "", regio_s) %>% factor()) %>%
  mutate(region_type = substr(region_code, 1, 2) %>% factor()) %>% 
  select(region_code, region, region_type, date, inhabitants)

# 3.2.3 plot time series ----
cbs_regionale_kerncijfers_tbl %>%
  dplyr::filter(region_type == "LD") %>% 
  select(date,
         region,
         inhabitants) %>% 
  plot_time_series(
    .date_var = date,
    .value = inhabitants,
    .facet_vars = region,
    .smooth = FALSE
  )    

# 3.3 CBS vacatures ----
# 3.3.1 EDA ----
cbs_regionale_vacatures_raw_tbl %>% glimpse()
cbs_regionale_vacatures_raw_tbl %>% pull(regio_s_label) %>% unique()
# DataExplorer::create_report(cbs_regionale_kerncijfers_raw_tbl)
skimr::skim(cbs_regionale_vacatures_raw_tbl) 

# 3.2.2 wrangle dataset ----
cbs_regionale_vacatures_tbl <- 
  cbs_regionale_vacatures_raw_tbl %>% 
  mutate(level = case_when(str_detect(bedrijfstakken_branches_sbi2008_label, "(^A-U*)") ~ "Total",
                           str_detect(bedrijfstakken_branches_sbi2008_label, "(^[A-Z]-[A-Z]*)") ~ "Grouped",
                           TRUE ~ "Detailed")
         ) %>% 
  select(level, industry = bedrijfstakken_branches_sbi2008_label, regio_s,
         region_label = regio_s_label, date = perioden_date,
         value = openstaande_vacatures_1) %>% 
  # opschonen regio-code
  mutate(region_code = gsub(" ", "", regio_s) %>% factor()) %>%
  mutate(region_type = substr(region_code, 1, 2) %>% factor()) %>% 
  select(region_type, region_code, region_label, level, id = industry, date, value)

# 3.2.3 plot time series ----
cbs_regionale_vacatures_tbl %>%
  dplyr::filter(region_type == "NL" & level == "Grouped") %>% 
  select(date,
         id,
         value) %>% 
  plot_time_series(
    .date_var = date,
    .value = value,
    .facet_vars = id,
    .smooth = FALSE
  ) 

