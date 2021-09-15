# 0. LIBRARIES -----------------------------------------------------------------
# 0.1 retrieve data ----
library(cbsodataR)

# 0.2 EDA & data cleaning ----
library(DataExplorer)
library(janitor)

# 0.3 GIS ----
library(geojsonio)
library(sf)

# 0.4 base packages ----
library(tidymodels)
library(tidyverse)

# 0.5 timeseries & modelling ----
library(lubridate)
library(timetk)
library(modeltime)
library(modeltime.resample)
library(modeltime.gluonts)
library(modeltime.ensemble)
library(baguette)
library(rules)

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

# 2.5 CBS PUMP PRICES MOTOR FUELS ----
cbs_pump_prices_raw_tbl <-
  cbs_get_data("80416ENG") %>%
  cbs_add_label_columns() %>%
  clean_names() %>% 
  mutate(date = ymd(periods)) %>% 
  select(date, petrol = euro95_1, diesel = diesel_2, lpg = lpg_3)

cbs_pump_prices_raw_tbl %>% 
  pivot_longer(cols = petrol:lpg,names_to = "id") %>% 
  group_by(id) %>% 
  plot_time_series(.date_var = date,.value = value)
  
# 2.6 HOUSING PRICES ----
cbs_dwelling_prices_raw_tbl <-
  cbs_get_data("83910ENG") %>%
  cbs_add_label_columns() %>%
  cbs_add_date_column() %>%
  clean_names() %>% 
  filter(periods_freq == "Q") %>% 
  select(date                   = periods_date, 
         type_of_dwelling       = type_of_dwelling_label, 
         average_purchase_price = average_purchase_price_7) %>% 
  pivot_wider(names_from  = type_of_dwelling,
              values_from = average_purchase_price)
  glimpse()

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
# DataExplorer::create_report(cbs_bevolkings_prognose_raw_tbl)
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
  mutate(region_code = gsub(" ", "", regio_s)) %>%
  mutate(region_type = substr(region_code, 1, 2)) %>% 
  select(region_type, region_code, region_label, level, id = industry, date, value) %>% 
  mutate(region_label = as.character(region_label),
         id = as.character(id))

# 3.2.3 plot time series ----
cbs_regionale_vacatures_tbl %>%
  dplyr::filter(region_type == "NL" & (level == "Grouped" | level == "Total")) %>% 
  select(date,
         id,
         value) %>% 
  plot_time_series(
    .date_var = date,
    .value = value,
    .facet_vars = id,
    .smooth = FALSE
  ) 

# 4. TIME SERIES ---------------------------------------------------------------
# 4.1 prepare dataset ----
ds <- cbs_regionale_vacatures_tbl %>%
  dplyr::filter(region_type == "NL" & (level == "Grouped" | level == "Total")) %>% 
  select(id, date, value)

ts_summary_tbl <- ds %>% 
  group_by(id) %>%
  tk_summary_diagnostics()

# * settings ----
median_nobs <- ts_summary_tbl %>%
  pull(n.obs) %>%
  median()

horizon_recommended <- round(0.18 * median_nobs)
horizon <- horizon_recommended

# trans_fun     <- log1p
# trans_fun_inv <- expm1

# * datasets ----
full_data_table <- ds %>% 
  group_by(id) %>% 
  future_frame(.date_var = date,.length_out = horizon,.bind_data = TRUE) %>% 
  ungroup()

data_prepared_tbl <- 
  full_data_table %>% 
  filter(!is.na(value))

future_tbl <- 
  full_data_table %>% 
  filter(is.na(value))


# * splits ----
splits = time_series_split(
  data       = data_prepared_tbl,
  assess     = horizon,
  cumulative = TRUE
)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value)

# 4.2 recipe(s) ----
recipe_spec_1 <- recipe(value ~ ., training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(matches("(.iso$)|(.xts$)|(month)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(date_index.num, date_year) %>%
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec_1 %>% prep() %>% juice() %>% glimpse()

recipe_spec_2 <- recipe_spec_1 %>%
  update_role(date, new_role = "ID")

recipe_spec_1 %>% prep() %>% summary()
recipe_spec_2 %>% prep() %>% summary()

# 5. MODELS --------------------------------------------------------------------
# 5.1 model specs ----
# get model_db from parsnip
model_db <- parsnip::model_db %>% 
  filter(mode == "regression") %>% 
  mutate(model_spec_name = str_c(model, "_", engine, "_spec"),
         model_spec = str_c(package, "::", model, "() %>% ",  
                            "set_engine('", engine, "')"))

arima_boost_arima_xgboost_spec <-
  modeltime::arima_boost() %>%
  set_engine('arima_xgboost')

arima_boost_auto_arima_xgboost_spec <-
  modeltime::arima_boost() %>%
  set_engine('auto_arima_xgboost')

# arima_reg_arima_spec <-
#   modeltime::arima_reg() %>%
#   set_engine('arima')

# arima_reg_auto_arima_spec <-
#   modeltime::arima_reg() %>%
#   set_engine('auto_arima')

bag_mars_earth_spec <-
  baguette::bag_mars() %>%
  set_engine('earth') %>%
  set_mode('regression')

bag_tree_rpart_spec <-
  baguette::bag_tree() %>%
  set_engine('rpart') %>%
  set_mode('regression')

boost_tree_xgboost_spec <-
  boost_tree() %>%
  set_engine('xgboost') %>%
  set_mode('regression')

cubist_rules_Cubist_spec <-
  rules::cubist_rules() %>%
  set_engine('Cubist')

decision_tree_rpart_spec <-
  decision_tree() %>%
  set_engine('rpart') %>%
  set_mode('regression')

exp_smoothing_ets_spec <-
  modeltime::exp_smoothing() %>%
  set_engine('ets')

# linear_reg_glmnet_spec <-
#   linear_reg(penalty = tune()) %>%
#   set_engine('glmnet')

# linear_reg_keras_spec <-
#   linear_reg() %>%
#   set_engine('keras')

linear_reg_lm_spec <-
  linear_reg() %>%
  set_engine('lm')

linear_reg_stan_spec <-
  linear_reg() %>%
  set_engine('stan')

mars_earth_spec <-
  mars() %>%
  set_engine('earth') %>%
  set_mode('regression')

mlp_keras_spec <-
  mlp() %>%
  set_engine('keras') %>%
  set_mode('regression')

mlp_nnet_spec <-
  mlp() %>%
  set_engine('nnet') %>%
  set_mode('regression')

nearest_neighbor_kknn_spec <-
  nearest_neighbor() %>%
  set_engine('kknn') %>%
  set_mode('regression')

poisson_reg_glm_spec <-
  poissonreg::poisson_reg() %>%
  set_engine('glm')

# poisson_reg_glmnet_spec <-
#   poissonreg::poisson_reg() %>%
#   set_engine('glmnet')

poisson_reg_hurdle_spec <-
  poissonreg::poisson_reg() %>%
  set_engine('hurdle')

poisson_reg_stan_spec <-
  poissonreg::poisson_reg() %>%
  set_engine('stan')

poisson_reg_zeroinfl_spec <-
  poissonreg::poisson_reg() %>%
  set_engine('zeroinfl')

prophet_boost_prophet_xgboost_spec <-
  modeltime::prophet_boost() %>%
  set_engine('prophet_xgboost')

prophet_reg_prophet_spec <-
  modeltime::prophet_reg() %>%
  set_engine('prophet')

rand_forest_randomForest_spec <-
  rand_forest() %>%
  set_engine('randomForest') %>%
  set_mode('regression')

rand_forest_ranger_spec <-
  rand_forest() %>%
  set_engine('ranger') %>%
  set_mode('regression')

rule_fit_xrf_spec <-
  rules::rule_fit() %>%
  set_engine('xrf') %>%
  set_mode('regression')

surv_reg_flexsurv_spec <-
  survival_reg() %>%
  set_engine('flexsurv')

surv_reg_survival_spec <-
  survival_reg() %>%
  set_engine('survival')

svm_poly_kernlab_spec <-
  svm_poly() %>%
  set_engine('kernlab') %>%
  set_mode('regression')

svm_rbf_kernlab_spec <-
  svm_rbf() %>%
  set_engine('kernlab') %>%
  set_mode('regression')


# 5.2 workflow fits ----
# * arima_xgboost ----
wflw_fit_arima_boost_arima_xgboost <- 
  workflow() %>% 
  add_model(arima_boost_arima_xgboost_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * auto_arima_xgboost ----  
wflw_fit_arima_boost_auto_arima_xgboost <- 
  workflow() %>% 
  add_model(arima_boost_auto_arima_xgboost_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# # * arima_reg_arima ----  
# wflw_fit_arima_reg_arima <- 
#   workflow() %>% 
#   add_model(arima_reg_arima_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# # * arima_reg_auto_arima ----  
# wflw_fit_arima_reg_auto_arima <- 
#   workflow() %>% 
#   add_model(arima_reg_auto_arima_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# * bag_mars_earth ----  
wflw_fit_bag_mars_earth <- 
  workflow() %>% 
  add_model(bag_mars_earth_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * bag_tree_rpart ----  
wflw_fit_bag_tree_rpart <- 
  workflow() %>% 
  add_model(bag_tree_rpart_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * boost_tree_xgboost ----  
wflw_fit_boost_tree_xgboost <- 
  workflow() %>% 
  add_model(boost_tree_xgboost_spec) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

# # * cubist_rules_Cubist ----  
# wflw_fit_cubist_rules_Cubist <- 
#   workflow() %>% 
#   add_model(cubist_rules_Cubist_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# * decision_tree_rpart ----  
wflw_fit_decision_tree_rpart <- 
  workflow() %>% 
  add_model(decision_tree_rpart_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * exp_smoothing_ets ----  
wflw_fit_exp_smoothing_ets <- 
  workflow() %>% 
  add_model(exp_smoothing_ets_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# # * linear_reg_glmnet ----  
# wflw_fit_linear_reg_glmnet <- 
#   workflow() %>% 
#   add_model(linear_reg_glmnet_spec) %>%
#   add_recipe(recipe_spec_2) %>%
#   fit(training(splits))

# # * linear_reg_keras ----  
# wflw_fit_linear_reg_keras <- 
#   workflow() %>% 
#   add_model(linear_reg_keras_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# * linear_reg_lm ----  
wflw_fit_linear_reg_lm <- 
  workflow() %>% 
  add_model(linear_reg_lm_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# # * linear_reg_stan ----  
# wflw_fit_linear_reg_stan <- 
#   workflow() %>% 
#   add_model(linear_reg_stan_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# * mars_earth ----  
wflw_fit_mars_earth <- 
  workflow() %>% 
  add_model(mars_earth_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# # * mlp_keras ----  
# wflw_fit_mlp_keras <- 
#   workflow() %>% 
#   add_model(mlp_keras_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# * mlp_nnet ----  
wflw_fit_mlp_nnet <- 
  workflow() %>% 
  add_model(mlp_nnet_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * nearest_neighbor_kknn ----  
wflw_fit_nearest_neighbor_kknn <- 
  workflow() %>% 
  add_model(nearest_neighbor_kknn_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * poisson_reg_glm ----  
wflw_fit_poisson_reg_glm <- 
  workflow() %>% 
  add_model(poisson_reg_glm_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# # * poisson_reg_glmnet ----  
# wflw_fit_poisson_reg_glmnet <- 
#   workflow() %>% 
#   add_model(poisson_reg_glmnet_spec) %>%
#   add_recipe(recipe_spec_2) %>%
#   fit(training(splits))

# # * poisson_reg_hurdle ----  
# wflw_fit_poisson_reg_hurdle <- 
#   workflow() %>% 
#   add_model(poisson_reg_hurdle_spec) %>%
#   add_recipe(recipe_spec_2) %>%
#   fit(training(splits))

# # * poisson_reg_stan ----  
# wflw_fit_poisson_reg_stan <- 
#   workflow() %>% 
#   add_model(poisson_reg_stan_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# # * poisson_reg_zeroinfl ----  
# wflw_fit_poisson_reg_zeroinfl <- 
#   workflow() %>% 
#   add_model(poisson_reg_zeroinfl_spec) %>%
#   add_recipe(recipe_spec_1) %>%
#   fit(training(splits))

# * prophet_boost_prophet_xgboost ----  
wflw_fit_prophet_boost_prophet_xgboost <- 
  workflow() %>% 
  add_model(prophet_boost_prophet_xgboost_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * prophet_reg_prophet_spec ----  
wflw_fit_prophet_reg_prophet <- 
  workflow() %>% 
  add_model(prophet_reg_prophet_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * rand_forest_randomForest ----  
wflw_fit_rand_forest_randomForest <- 
  workflow() %>% 
  add_model(rand_forest_randomForest_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * rand_forest_ranger ----  
wflw_fit_rand_forest_ranger <- 
  workflow() %>% 
  add_model(rand_forest_ranger_spec) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * rule_fit_xrf ----  
wflw_fit_rule_fit_xrf <- 
  workflow() %>% 
  add_model(rule_fit_xrf_spec) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

# # * surv_reg_flexsurv ----  
# wflw_fit_surv_reg_flexsurv <- 
#   workflow() %>% 
#   add_model(surv_reg_flexsurv_spec) %>%
#   add_recipe(recipe_spec_2) %>%
#   fit(training(splits))

# * svm_poly_kernlab ----
wflw_fit_svm_poly_kernlab <-
  workflow() %>%
  add_model(svm_poly_kernlab_spec) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

# * svm_rbf_kernlab ----
wflw_fit_svm_rbf_kernlab <-
  workflow() %>%
  add_model(svm_rbf_kernlab_spec) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

# DEEP LEARNING ----
# * DeepAR GluonTS ----
fit_deepar_gluonts <- deep_ar(
  id = "id",
  freq = "Q",
  prediction_length = horizon,
  lookback_length   = horizon*3,
  epochs = 5
) %>%
  set_engine("gluonts_deepar") %>%
  fit(value ~ date + id, data = training(splits))

# * DeepAR NBeats ----
fit_nbeats_gluonts <- nbeats(
  id = "id",
  freq = "Q",
  prediction_length = horizon,
  lookback_length   = horizon*3,
  epochs = 5
) %>%
  set_engine("gluonts_nbeats") %>%
  fit(value ~ date + id, data = training(splits))

# * DeepAR Torch -----
fit_deepar_torch <- deep_ar(
  id = "id",
  freq = "Q",
  prediction_length = horizon,
  lookback_length   = horizon*3,
  epochs = 5*2
) %>%
  set_engine("torch") %>%
  fit(value ~ date + id, data = training(splits))

# * GP Forecaster ----
fit_gp_forecaster <- gp_forecaster(
  id = "id",
  freq = "Q",
  prediction_length = horizon,
  # lookback_length   = 6*3,
  epochs = 5*3
) %>%
  set_engine("gluonts_gp_forecaster") %>%
  fit(value ~ date + id, data = training(splits))

# * NEW: Deep State ----
fit_deep_state <- deep_state(
  id = "id",
  freq = "M",
  prediction_length = horizon,
  lookback_length   = horizon*3,
  epochs = 5*4
) %>%
  set_engine("gluonts_deepstate") %>%
  fit(value ~ date + id, data = training(splits))

# BASELINE ----
# * Median by ID ----
model_median_fit <- 
  window_reg(id = "id", window_size = horizon) %>%
  set_engine("window_function", window_function = median) %>%
  fit(value ~ ., data = training(splits))

# * Mean by ID ----
model_mean_fit <- 
  window_reg(id = "id", window_size = horizon) %>%
  set_engine("window_function", window_function = mean) %>%
  fit(value ~ ., data = training(splits))

# * Seasonal NAIVE by ID ----
model_snaive_fit = 
  naive_reg(seasonal_period = 4, id = "id") %>%
  set_engine("snaive") %>%
  fit(value ~ ., data = training(splits))

# 6. RESULTS -------------------------------------------------------------------
# 6.1 Modeltime Table ----
submodels_tbl <- modeltime_table(
  wflw_fit_arima_boost_arima_xgboost,
  wflw_fit_arima_boost_auto_arima_xgboost,
  wflw_fit_bag_mars_earth,
  wflw_fit_bag_tree_rpart,
  wflw_fit_boost_tree_xgboost,
  wflw_fit_decision_tree_rpart,
  wflw_fit_exp_smoothing_ets,
  wflw_fit_linear_reg_lm,
  wflw_fit_mars_earth,
  wflw_fit_mlp_nnet,
  wflw_fit_nearest_neighbor_kknn,
  wflw_fit_poisson_reg_glm,
  wflw_fit_prophet_boost_prophet_xgboost,
  wflw_fit_prophet_reg_prophet,
  wflw_fit_rand_forest_randomForest,
  wflw_fit_rand_forest_ranger,
  wflw_fit_rule_fit_xrf,
  wflw_fit_svm_poly_kernlab,
  wflw_fit_svm_rbf_kernlab,
  fit_deep_state,
  fit_deepar_gluonts,
  fit_nbeats_gluonts,
  fit_deepar_torch,
  fit_gp_forecaster,
  model_mean_fit,
  model_median_fit,
  model_snaive_fit
) %>%
  update_model_description(1, "arima_boost_arima_xgboost") %>%
  update_model_description(2, "arima_boost_auto_arima_xgboost") %>%
  update_model_description(3, "bag_mars_earth") %>% 
  update_model_description(4, "bag_tree_rpart") %>%
  update_model_description(5, "boost_tree_xgboost") %>%
  update_model_description(6, "decision_tree_rpart") %>% 
  update_model_description(7, "exp_smoothing_ets") %>%
  update_model_description(8, "linear_reg_lm") %>% 
  update_model_description(9, "mars_earth") %>%
  update_model_description(10, "mlp_nnet") %>%
  update_model_description(11, "nearest_neighbor_kknn") %>%
  update_model_description(12, "poisson_reg_glm") %>%
  update_model_description(13, "prophet_boost_prophet_xgboost") %>%
  update_model_description(14, "prophet_reg_prophet") %>%
  update_model_description(15, "rand_forest_randomForest") %>% 
  update_model_description(16, "rand_forest_range") %>%
  update_model_description(17, "rule_fit_xrf") %>%
  update_model_description(18, "svm_poly_kernlab") %>% 
  update_model_description(19, "svm_rbf_kernlab") %>%
  update_model_description(20, "deep_state") %>% 
  update_model_description(21, "deepar_gluonts") %>%
  update_model_description(22, "nbeats_gluonts") %>%
  update_model_description(23, "deepar_torch") %>% 
  update_model_description(24, "gp_forecaster") %>%
  update_model_description(25, "baseline_median") %>%
  update_model_description(26, "baseline_mean") %>% 
  update_model_description(27, "baseline_snaive")

# 6.2 Calibrate Testing Data ----
submodels_calibrated_tbl <- submodels_tbl %>%
  modeltime_calibrate(testing(splits), id = "id")

# 6.3 Measure Test Accuracy ----
# * global accuracy ----
submodels_calibrated_tbl %>% 
  modeltime_accuracy() %>% 
  arrange(rmse) 

# * Accuracy by ID ----
submodels_acc_by_id_tbl <- 
  submodels_calibrated_tbl %>% 
  modeltime_accuracy(acc_by_id = TRUE) 

submodels_acc_by_id_tbl %>% 
  group_by(id) %>% 
  table_modeltime_accuracy()

submodels_acc_by_id_tbl %>%
  group_by(id) %>%
  slice_min(rmse, n = 1)

# * Visualize Test Forecast ----
submodels_calibrated_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    conf_by_id  = TRUE
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.facet_ncol = 2, .plotly_slider = FALSE)

