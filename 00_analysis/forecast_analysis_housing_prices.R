# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# * Parallel Processing ----
registerDoFuture()
n_cores <- parallel::detectCores()
plan(
    strategy = cluster,
    workers  = parallel::makeCluster(n_cores)
)

# 1. SETTINGS ----
# 1.1 data ----
data <- cbs_housing_prices_tbl %>%
    pivot_longer(cols = -date, names_to = "id")

# 1.2 periods ----
horizon <- 19 - (19 %% 4) # recommended horizon = 19, frequency = 4
ts_frequency <- data %>% 
    pivot_wider(names_from  = 2, values_from = 3) %>% 
    pull(date) %>% 
    tk_get_frequency()
ts_scale <- data %>%
    pivot_wider(names_from  = 2, values_from = 3) %>% 
    tk_summary_diagnostics() %>% 
    pull(scale)

# 1.3 transformation settings ----
# * transformations ----
trans_fun     <- log1p           # for log1p transformation
# trans_fun <- function(x) {x}     # for no transformation
trans_fun_inv <- expm1           # for log1p transformation
# trans_fun_inv <- function(x) {x} # for no transformation

# * cleaning outliers ----
clean_fun <- ts_clean_vec    # outlier cleaning, no inversion needed
# clean_fun <- function(x) {x} # no outlier cleaning, no inversion needed

# 2. FEATURE ENGINEERING ----
# 2.1 grouped data transformations + extending data ----
full_data_tbl <- data %>% 
    
    # Remove missing values
    drop_na() %>%
    
    # Apply transformation
    mutate(value = ifelse(value < 0, 0, value)) %>% # no negative values in log-transformations
    mutate(value = trans_fun(value)) %>% 
    
    # Apply Group-wise Time Series Manipulations
    group_by(id) %>%
    pad_by_time(.date_var = date, .pad_value = 0) %>%
    # # center and/or scale 
    # mutate(value = scale_fun(value)) %>% 
    # extend data
    future_frame(
        .date_var   = date,
        .length_out = horizon,
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
            tk_augment_fourier(date, .periods = c(ts_frequency, 2*ts_frequency))  %>%
            tk_augment_lags(.value = value, .lags = horizon) %>%
            tk_augment_slidify(
                paste0("value_lag", horizon),
                .f       = ~ mean(.x, na.rm = TRUE),
                .period  = c(ts_frequency, 4*ts_frequency, 8*ts_frequency),
                .partial = TRUE,
                .align   = "center"
            )
    }) %>%
    bind_rows() %>%
    # NaN to NA
    mutate(across(where(is.double), ~replace(., is.nan(.), NA))) %>% 
    
    rowid_to_column(var = "rowid")

print(full_data_tbl)

# 2.2 SPLITTING DATA ----
# * data prepared ----
# remove future data
actual_data <- full_data_tbl %>%
    filter(!is.na(value))

# remove rows without features
data_prepared_tbl <- actual_data %>%
    drop_na()

# * future data ----
future_tbl <- full_data_tbl %>%
    filter(is.na(value))

# ** splitting ----
splits <- data_prepared_tbl %>%
    time_series_split(
        date_var   = date,
        assess     = horizon,
        cumulative = TRUE
    )

train_cleaned <- training(splits) %>%
    group_by(id) %>%
    mutate(value = clean_fun(value, period = ts_frequency)) %>%
    ungroup()

# 2.3 RECIPES ----
# 2.3.1 base recipe ----
recipe_spec_1 <-
    recipe(value ~ ., data = train_cleaned) %>%
    update_role(rowid, new_role = "indicator") %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")) %>%
    step_normalize(date_index.num, date_year) %>%
    step_other(id) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

# adjust recipe for frequency
recipe_spec_1 <- if(ts_scale == "week") {
    recipe_spec_1 %>% 
        step_rm(matches("(day)"))
    } else if(ts_scale == "month") {
        recipe_spec_1 %>% 
            step_rm(matches("(day)|(week)"))
        } else if(ts_scale == "quarter") {
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

# 2.4 CROSS VALIDATION ----
set.seed(123)
resamples_kfold <- vfold_cv(
    training(splits),
    v = 25
)

# 3. MODELLING ----
# * MODEL SPECS ----

# + ARIMA ----
wflw_spec_arima_1 <- workflow() %>%
    add_model(
        arima_reg() %>% set_engine("auto_arima")
    ) %>%
    add_recipe(recipe = recipe(value ~ date, train_cleaned))

# + Cubist 1 ----
# wflw_spec_cubist_1 <- workflow() %>%
#     add_model(
#         cubist_rules() %>% set_engine("Cubist")
#     ) %>%
#     add_recipe(recipe_spec_1_alt)

cubist_tune_spec <-
    rules::cubist_rules(
        committees = tune(),
        neighbors = tune(),
        max_rules = tune()
    ) %>%
    set_engine('Cubist')

wflw_spec_cubist_tune_1 <- workflow() %>%
    add_model(
        cubist_tune_spec
    ) %>%
    add_recipe(recipe_spec_1_alt)

tic()
set.seed(123)
tune_results_cubist_1 <- wflw_spec_cubist_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_cubist_tune_1),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_cubist_1 <- wflw_spec_cubist_tune_1 %>%
    finalize_workflow(select_best(tune_results_cubist_1, "rmse"))

# + Cubist 2 ----
# wflw_spec_cubist_2 <- workflow() %>%
#     add_model(
#         cubist_rules() %>% set_engine("Cubist")
#     ) %>%
#     add_recipe(recipe_spec_2_alt)

wflw_spec_cubist_tune_2 <- workflow() %>%
    add_model(
        cubist_tune_spec
    ) %>%
    add_recipe(recipe_spec_2_alt)

tic()
set.seed(123)
tune_results_cubist_2 <- wflw_spec_cubist_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_cubist_tune_2),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_cubist_2 <- wflw_spec_cubist_tune_2 %>%
    finalize_workflow(select_best(tune_results_cubist_2, "rmse"))

# + Cubist 3 ----
# wflw_spec_cubist_3 <- workflow() %>%
#     add_model(
#         cubist_rules() %>% set_engine("Cubist")
#     ) %>%
#     add_recipe(recipe_spec_3_alt)

wflw_spec_cubist_tune_3 <- workflow() %>%
    add_model(
        cubist_tune_spec
    ) %>%
    add_recipe(recipe_spec_3_alt)

tic()
set.seed(123)
tune_results_cubist_3 <- wflw_spec_cubist_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_cubist_tune_3),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_cubist_3 <- wflw_spec_cubist_tune_3 %>%
    finalize_workflow(select_best(tune_results_cubist_3, "rmse"))

# + Cubist 4 ----
# wflw_spec_cubist_4 <- workflow() %>%
#     add_model(
#         cubist_rules() %>% set_engine("Cubist")
#     ) %>%
#     add_recipe(recipe_spec_4_alt)

wflw_spec_cubist_tune_4 <- workflow() %>%
    add_model(
        cubist_tune_spec
    ) %>%
    add_recipe(recipe_spec_4_alt)

tic()
set.seed(123)
tune_results_cubist_4 <- wflw_spec_cubist_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_cubist_tune_4),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_cubist_4 <- wflw_spec_cubist_tune_4 %>%
    finalize_workflow(select_best(tune_results_cubist_4, "rmse"))

# ** Results tuned ----
tune_results_cubist_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_cubist_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_cubist_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_cubist_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)


# + GLMNET 1 ----
# wflw_spec_glm_1 <- workflow() %>%
#     add_model(
#         spec = linear_reg(mode = "regression",
#                           penalty = 0.01,
#                           mixture = 0.5) %>% set_engine("glmnet")
#     ) %>%
#     add_recipe(recipe_spec_1_alt)
glmnet_tune_spec <-
    linear_reg(penalty = tune(), 
               mixture = tune()) %>%
    set_engine('glmnet')

wflw_spec_glmnet_tune_1 <- workflow() %>%
    add_model(
        glmnet_tune_spec
    ) %>%
    add_recipe(recipe_spec_1_alt)

tic()
set.seed(123)
tune_results_glmnet_1 <- wflw_spec_glmnet_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_glmnet_tune_1),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_glm_1 <- wflw_spec_glmnet_tune_1 %>%
    finalize_workflow(select_best(tune_results_glmnet_1, "rmse"))

# + GLMNET 2 ----
# wflw_spec_glm_2 <- workflow() %>%
#     add_model(
#         spec = linear_reg(mode = "regression",
#                           penalty = 0.01,
#                           mixture = 0.5) %>% set_engine("glmnet")
#     ) %>%
#     add_recipe(recipe_spec_2_alt)
wflw_spec_glmnet_tune_2 <- workflow() %>%
    add_model(
        glmnet_tune_spec
    ) %>%
    add_recipe(recipe_spec_2_alt)

tic()
set.seed(123)
tune_results_glmnet_2 <- wflw_spec_glmnet_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_glmnet_tune_2),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_glm_2 <- wflw_spec_glmnet_tune_2 %>%
    finalize_workflow(select_best(tune_results_glmnet_2, "rmse"))

# + GLMNET 3 ----
# wflw_spec_glm_3 <- workflow() %>%
#     add_model(
#         spec = linear_reg(mode = "regression",
#                           penalty = 0.01,
#                           mixture = 0.5) %>% set_engine("glmnet")
#     ) %>%
#     add_recipe(recipe_spec_3_alt)
wflw_spec_glmnet_tune_3 <- workflow() %>%
    add_model(
        glmnet_tune_spec
    ) %>%
    add_recipe(recipe_spec_3_alt)

tic()
set.seed(123)
tune_results_glmnet_3 <- wflw_spec_glmnet_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_glmnet_tune_3),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_glm_3 <- wflw_spec_glmnet_tune_3 %>%
    finalize_workflow(select_best(tune_results_glmnet_3, "rmse"))

# + GLMNET 4 ----
# wflw_spec_glm_4 <- workflow() %>%
#     add_model(
#         spec = linear_reg(mode = "regression",
#                           penalty = 0.01,
#                           mixture = 0.5) %>% set_engine("glmnet")
#     ) %>%
#     add_recipe(recipe_spec_4_alt)
wflw_spec_glmnet_tune_4 <- workflow() %>%
    add_model(
        glmnet_tune_spec
    ) %>%
    add_recipe(recipe_spec_4_alt)

tic()
set.seed(123)
tune_results_glmnet_4 <- wflw_spec_glmnet_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_glmnet_tune_4),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_glm_4 <- wflw_spec_glmnet_tune_4 %>%
    finalize_workflow(select_best(tune_results_glmnet_1, "rmse"))

# ** Results tuned ----
tune_results_glmnet_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_glmnet_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_glmnet_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_glmnet_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)

# + KNN 1 ----
# wflw_spec_knn_1 <- workflow() %>%
#     add_model(
#         nearest_neighbor() %>% set_engine("kknn")
#     ) %>%
#     add_recipe(recipe_spec_1_alt)

kknn_tune_spec <-
    nearest_neighbor(
        neighbors = tune(),
        weight_func = tune(),
        dist_power = tune()
    ) %>%
    set_engine('kknn') %>%
    set_mode('regression')

wflw_spec_kknn_tune_1 <- workflow() %>%
    add_model(
        kknn_tune_spec %>% set_engine("kknn")
    ) %>%
    add_recipe(recipe_spec_1_alt)

tic()
set.seed(123)
tune_results_kknn_1 <- wflw_spec_kknn_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_kknn_tune_1),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_kknn_tune_1 <- wflw_spec_kknn_tune_1 %>%
    finalize_workflow(select_best(tune_results_kknn_1, "rmse"))

# + KNN 2 ----
# wflw_spec_knn_2 <- workflow() %>%
#     add_model(
#         nearest_neighbor() %>% set_engine("kknn")
#     ) %>%
#     add_recipe(recipe_spec_2_alt)

wflw_spec_kknn_tune_2 <- workflow() %>%
    add_model(
        kknn_tune_spec %>% set_engine("kknn")
    ) %>%
    add_recipe(recipe_spec_2_alt)

tic()
set.seed(123)
tune_results_kknn_2 <- wflw_spec_kknn_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_kknn_tune_2),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_kknn_tune_2 <- wflw_spec_kknn_tune_2 %>%
    finalize_workflow(select_best(tune_results_kknn_2, "rmse"))

# + KNN 3 ----
# wflw_spec_knn_3 <- workflow() %>%
#     add_model(
#         nearest_neighbor() %>% set_engine("kknn")
#     ) %>%
#     add_recipe(recipe_spec_3_alt)

wflw_spec_kknn_tune_3 <- workflow() %>%
    add_model(
        kknn_tune_spec %>% set_engine("kknn")
    ) %>%
    add_recipe(recipe_spec_3_alt)

tic()
set.seed(123)
tune_results_kknn_3 <- wflw_spec_kknn_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_kknn_tune_3),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_kknn_tune_3 <- wflw_spec_kknn_tune_3 %>%
    finalize_workflow(select_best(tune_results_kknn_3, "rmse"))

# + KNN 4 ----
# wflw_spec_knn_4 <- workflow() %>%
#     add_model(
#         nearest_neighbor() %>% set_engine("kknn")
#     ) %>%
#     add_recipe(recipe_spec_4_alt)

wflw_spec_kknn_tune_4 <- workflow() %>%
    add_model(
        kknn_tune_spec %>% set_engine("kknn")
    ) %>%
    add_recipe(recipe_spec_4_alt)

tic()
set.seed(123)
tune_results_kknn_4 <- wflw_spec_kknn_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_kknn_tune_4),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_kknn_tune_4 <- wflw_spec_kknn_tune_4 %>%
    finalize_workflow(select_best(tune_results_kknn_4, "rmse"))

# ** Results tuned ----
tune_results_kknn_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_kknn_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_kknn_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_kknn_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)

# + MARS 1 ----
# wflw_spec_mars_1 <- workflow() %>%
#     add_model(
#         spec = mars() %>% set_engine("earth")
#     ) %>%
#     add_recipe(recipe_spec_1_alt)

mars_tune_spec <-
    mars(prod_degree = tune()) %>%
    set_engine('earth') %>%
    set_mode('regression')

wflw_spec_mars_tune_1 <- workflow() %>%
    add_model(
        spec = mars_tune_spec %>% set_engine("earth")
    ) %>%
    add_recipe(recipe_spec_1_alt)

tic()
set.seed(123)
tune_results_mars_1 <- wflw_spec_mars_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_mars_tune_1),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_mars_1 <- wflw_spec_mars_tune_1 %>%
    finalize_workflow(select_best(tune_results_mars_1, "rmse"))

# + MARS 2 ----
# wflw_spec_mars_2 <- workflow() %>%
#     add_model(
#         spec = mars() %>% set_engine("earth")
#     ) %>%
#     add_recipe(recipe_spec_2_alt)

wflw_spec_mars_tune_2 <- workflow() %>%
    add_model(
        spec = mars_tune_spec %>% set_engine("earth")
    ) %>%
    add_recipe(recipe_spec_2_alt)

tic()
set.seed(123)
tune_results_mars_2 <- wflw_spec_mars_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_mars_tune_2),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_mars_2 <- wflw_spec_mars_tune_2 %>%
    finalize_workflow(select_best(tune_results_mars_2, "rmse"))

# + MARS 3 ----
# wflw_spec_mars_3 <- workflow() %>%
#     add_model(
#         spec = mars() %>% set_engine("earth")
#     ) %>%
#     add_recipe(recipe_spec_3_alt)

wflw_spec_mars_tune_3 <- workflow() %>%
    add_model(
        spec = mars_tune_spec %>% set_engine("earth")
    ) %>%
    add_recipe(recipe_spec_3_alt)

tic()
set.seed(123)
tune_results_mars_3 <- wflw_spec_mars_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_mars_tune_3),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_mars_3 <- wflw_spec_mars_tune_3 %>%
    finalize_workflow(select_best(tune_results_mars_3, "rmse"))

# + MARS 4 ----
# wflw_spec_mars_4 <- workflow() %>%
#     add_model(
#         spec = mars() %>% set_engine("earth")
#     ) %>%
#     add_recipe(recipe_spec_4_alt)

wflw_spec_mars_tune_4 <- workflow() %>%
    add_model(
        spec = mars_tune_spec %>% set_engine("earth")
    ) %>%
    add_recipe(recipe_spec_4_alt)

tic()
set.seed(123)
tune_results_mars_4 <- wflw_spec_mars_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_mars_tune_4),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_mars_4 <- wflw_spec_mars_tune_4 %>%
    finalize_workflow(select_best(tune_results_mars_4, "rmse"))

# ** Results tuned ----
tune_results_mars_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_mars_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_mars_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_mars_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)

# + Prophet Boost 1 ----
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

prophet_boost_tune_spec <-
    modeltime::prophet_boost(
        seasonality_daily  = FALSE,
        seasonality_weekly = FALSE,
        seasonality_yearly = FALSE,
        growth = "linear",
        num_changepoints = tune(),
        season = tune(),
        prior_scale_changepoints = tune(),
        prior_scale_seasonality = tune(),
        prior_scale_holidays = tune(),
        tree_depth = tune(),
        trees = tune(),
        learn_rate = tune(),
        mtry = tune(),
        min_n = tune(),
        loss_reduction = tune(),
        sample_size = 1,
        stop_iter = tune()
    ) %>%
    set_engine('prophet_xgboost')

wflw_spec_prophet_boost_tune_1 <- workflow() %>%
    add_model(prophet_boost_tune_spec) %>%
    add_recipe(recipe_spec_1)

tic()
set.seed(123)
tune_results_prophet_boost_1 <- wflw_spec_prophet_boost_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_prophet_boost_tune_1),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_prophet_boost_1 <- wflw_spec_prophet_boost_tune_1 %>%
    finalize_workflow(select_best(tune_results_prophet_boost_1, "rmse"))

# + Prophet Boost 2 ----
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

wflw_spec_prophet_boost_tune_2 <- workflow() %>%
    add_model(prophet_boost_tune_spec) %>%
    add_recipe(recipe_spec_2)

tic()
set.seed(123)
tune_results_prophet_boost_2 <- wflw_spec_prophet_boost_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_prophet_boost_tune_2),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_prophet_boost_2 <- wflw_spec_prophet_boost_tune_2 %>%
    finalize_workflow(select_best(tune_results_prophet_boost_2, "rmse"))

# + Prophet Boost 3 ----
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

wflw_spec_prophet_boost_tune_3 <- workflow() %>%
    add_model(prophet_boost_tune_spec) %>%
    add_recipe(recipe_spec_3)

tic()
set.seed(123)
tune_results_prophet_boost_3 <- wflw_spec_prophet_boost_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_prophet_boost_tune_3),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_prophet_boost_3 <- wflw_spec_prophet_boost_tune_3 %>%
    finalize_workflow(select_best(tune_results_prophet_boost_3, "rmse"))

# + Prophet Boost 4 ----
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

wflw_spec_prophet_boost_tune_4 <- workflow() %>%
    add_model(prophet_boost_tune_spec) %>%
    add_recipe(recipe_spec_4)

tic()
set.seed(123)
tune_results_prophet_boost_4 <- wflw_spec_prophet_boost_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_prophet_boost_tune_4),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_prophet_boost_4 <- wflw_spec_prophet_boost_tune_4 %>%
    finalize_workflow(select_best(tune_results_prophet_boost_4, "rmse"))

# ** Results tuned ----
tune_results_prophet_boost_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_prophet_boost_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_prophet_boost_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_prophet_boost_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)

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

# + Random Forest - Ranger 1 ----
# wflw_spec_rf_1 <- workflow() %>%
#     add_model(
#         rand_forest() %>% set_engine("ranger")
#     ) %>%
#     add_recipe(recipe_spec_1_alt)
rand_forest_tune_spec <-
    rand_forest(mtry  = tune(), 
                min_n = tune()) %>%
    set_engine('ranger') %>%
    set_mode('regression')

wflw_spec_rand_forest_tune_1 <- workflow() %>%
    add_model(
        rand_forest_tune_spec
    ) %>%
    add_recipe(recipe_spec_1_alt)

tic()
set.seed(123)
tune_results_rand_forest_1 <- wflw_spec_rand_forest_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_rand_forest_tune_1),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_rf_1 <- wflw_spec_rand_forest_tune_1 %>%
    finalize_workflow(select_best(tune_results_rand_forest_1, "rmse"))

# + Random Forest - Ranger 2 ----
# wflw_spec_rf_2 <- workflow() %>%
#     add_model(
#         rand_forest() %>% set_engine("ranger")
#     ) %>%
#     add_recipe(recipe_spec_2_alt)
wflw_spec_rand_forest_tune_2 <- workflow() %>%
    add_model(
        rand_forest_tune_spec
    ) %>%
    add_recipe(recipe_spec_2_alt)

tic()
set.seed(123)
tune_results_rand_forest_2 <- wflw_spec_rand_forest_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_rand_forest_tune_2),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_rf_2 <- wflw_spec_rand_forest_tune_2 %>%
    finalize_workflow(select_best(tune_results_rand_forest_2, "rmse"))

# + Random Forest - Ranger 3 ----
# wflw_spec_rf_3 <- workflow() %>%
#     add_model(
#         rand_forest() %>% set_engine("ranger")
#     ) %>%
#     add_recipe(recipe_spec_3_alt)
wflw_spec_rand_forest_tune_3 <- workflow() %>%
    add_model(
        rand_forest_tune_spec
    ) %>%
    add_recipe(recipe_spec_3_alt)

tic()
set.seed(123)
tune_results_rand_forest_3 <- wflw_spec_rand_forest_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_rand_forest_tune_3),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_rf_3 <- wflw_spec_rand_forest_tune_3 %>%
    finalize_workflow(select_best(tune_results_rand_forest_3, "rmse"))

# + Random Forest - Ranger 4 ----
# wflw_spec_rf_4 <- workflow() %>%
#     add_model(
#         rand_forest() %>% set_engine("ranger")
#     ) %>%
#     add_recipe(recipe_spec_4_alt)
wflw_spec_rand_forest_tune_4 <- workflow() %>%
    add_model(
        rand_forest_tune_spec
    ) %>%
    add_recipe(recipe_spec_4_alt)

tic()
set.seed(123)
tune_results_rand_forest_4 <- wflw_spec_rand_forest_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_rand_forest_tune_4),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_rf_4 <- wflw_spec_rand_forest_tune_4 %>%
    finalize_workflow(select_best(tune_results_rand_forest_4, "rmse"))

# ** Results tuned ----
tune_results_rand_forest_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_rand_forest_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_rand_forest_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_rand_forest_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)

# + SVM 1 ----
# wflw_spec_svm_1 <- workflow() %>%
#     add_model(
#         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
#     ) %>%
#     add_recipe(recipe_spec_1_alt)

svm_rbf_tune_spec <-
    svm_rbf(cost = tune(),
            rbf_sigma = tune(),
            margin = tune()) %>%
    set_engine('kernlab') %>%
    set_mode('regression')

wflw_spec_svm_tune_1 <- workflow() %>%
    add_model(
        svm_rbf_tune_spec
    ) %>%
    add_recipe(recipe_spec_1_alt)

tic()
set.seed(123)
tune_results_svm_rbf_1 <- wflw_spec_svm_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_svm_tune_1),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_svm_rbf_1 <- wflw_spec_svm_tune_1 %>%
    finalize_workflow(select_best(tune_results_svm_rbf_1, "rmse"))

# + SVM 2 ----
# wflw_spec_svm_2 <- workflow() %>%
#     add_model(
#         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
#     ) %>%
#     add_recipe(recipe_spec_2_alt)

wflw_spec_svm_tune_2 <- workflow() %>%
    add_model(
        svm_rbf_tune_spec
    ) %>%
    add_recipe(recipe_spec_2_alt)

tic()
set.seed(123)
tune_results_svm_rbf_2 <- wflw_spec_svm_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_svm_tune_2),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_svm_rbf_2 <- wflw_spec_svm_tune_2 %>%
    finalize_workflow(select_best(tune_results_svm_rbf_2, "rmse"))

# + SVM 3 ----
# wflw_spec_svm_3 <- workflow() %>%
#     add_model(
#         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
#     ) %>%
#     add_recipe(recipe_spec_3_alt)

wflw_spec_svm_tune_3 <- workflow() %>%
    add_model(
        svm_rbf_tune_spec
    ) %>%
    add_recipe(recipe_spec_3_alt)

tic()
set.seed(123)
tune_results_svm_rbf_3 <- wflw_spec_svm_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_svm_tune_3),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_svm_rbf_3 <- wflw_spec_svm_tune_3 %>%
    finalize_workflow(select_best(tune_results_svm_rbf_3, "rmse"))

# + SVM 4 ----
# wflw_spec_svm_4 <- workflow() %>%
#     add_model(
#         spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
#     ) %>%
#     add_recipe(recipe_spec_4_alt)

wflw_spec_svm_tune_4 <- workflow() %>%
    add_model(
        svm_rbf_tune_spec
    ) %>%
    add_recipe(recipe_spec_4_alt)

tic()
set.seed(123)
tune_results_svm_rbf_4 <- wflw_spec_svm_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_svm_tune_4),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_svm_rbf_4 <- wflw_spec_svm_tune_4 %>%
    finalize_workflow(select_best(tune_results_svm_rbf_4, "rmse"))

# ** Results tuned ----
tune_results_svm_rbf_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_svm_rbf_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_svm_rbf_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_svm_rbf_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)


# + XGBoost 1 ----
# wflw_spec_xgboost_1 <- workflow() %>%
#     add_model(
#         boost_tree() %>% set_engine("xgboost")
#     ) %>%
#     add_recipe(recipe_spec_1_alt)
boost_tree_xgboost_tune_spec <-
    boost_tree(
        tree_depth = tune(),
        trees = tune(),
        learn_rate = tune(),
        min_n = tune(),
        loss_reduction = tune(),
        sample_size = tune(),
        stop_iter = tune()
    ) %>%
    set_engine('xgboost') %>%
    set_mode('regression')

wflw_spec_xgboost_tune_1 <- workflow() %>%
    add_model(
        boost_tree_xgboost_tune_spec
    ) %>%
    add_recipe(recipe_spec_1_alt)

tic()
set.seed(123)
tune_results_xgboost_1 <- wflw_spec_xgboost_tune_1 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(boost_tree_xgboost_tune_spec),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_xgboost_1 <- wflw_spec_xgboost_tune_1 %>%
    finalize_workflow(select_best(tune_results_xgboost_1, "rmse"))

# + XGBoost 2 ----
# wflw_spec_xgboost_2 <- workflow() %>%
#     add_model(
#         boost_tree() %>% set_engine("xgboost")
#     ) %>%
#     add_recipe(recipe_spec_2_alt)

wflw_spec_xgboost_tune_2 <- workflow() %>%
    add_model(
        boost_tree_xgboost_tune_spec
    ) %>%
    add_recipe(recipe_spec_2_alt)

tic()
set.seed(123)
tune_results_xgboost_2 <- wflw_spec_xgboost_tune_2 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(boost_tree_xgboost_tune_spec),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_xgboost_2 <- wflw_spec_xgboost_tune_2 %>%
    finalize_workflow(select_best(tune_results_xgboost_2, "rmse"))

# + XGBoost 3 ----
# wflw_spec_xgboost_3 <- workflow() %>%
#     add_model(
#         boost_tree() %>% set_engine("xgboost")
#     ) %>%
#     add_recipe(recipe_spec_3_alt)

wflw_spec_xgboost_tune_3 <- workflow() %>%
    add_model(
        boost_tree_xgboost_tune_spec
    ) %>%
    add_recipe(recipe_spec_3_alt)

tic()
set.seed(123)
tune_results_xgboost_3 <- wflw_spec_xgboost_tune_3 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(boost_tree_xgboost_tune_spec),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_xgboost_3 <- wflw_spec_xgboost_tune_3 %>%
    finalize_workflow(select_best(tune_results_xgboost_3, "rmse"))

# + XGBoost 4 ----
# wflw_spec_xgboost_4 <- workflow() %>%
#     add_model(
#         boost_tree() %>% set_engine("xgboost")
#     ) %>%
#     add_recipe(recipe_spec_4_alt)
wflw_spec_xgboost_tune_4 <- workflow() %>%
    add_model(
        boost_tree_xgboost_tune_spec
    ) %>%
    add_recipe(recipe_spec_4_alt)

tic()
set.seed(123)
tune_results_xgboost_4 <- wflw_spec_xgboost_tune_4 %>%
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(boost_tree_xgboost_tune_spec),
        grid = 25,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

wflw_spec_xgboost_4 <- wflw_spec_xgboost_tune_4 %>%
    finalize_workflow(select_best(tune_results_xgboost_4, "rmse"))

# ** Results tuned ----
tune_results_xgboost_1 %>% show_best("rmse", n = 1) %>%
    bind_rows(tune_results_xgboost_2 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_xgboost_3 %>% show_best("rmse", n = 1)) %>% 
    bind_rows(tune_results_xgboost_4 %>% show_best("rmse", n = 1)) %>% 
    rowid_to_column() %>% 
    arrange(mean)


# * SUBMODEL SPEC SELECTION ----
spec_table <- tibble(
    wflw_spec = list(
        # wflw_spec_arima_1,
        # wflw_spec_xgboost_1,
        # wflw_spec_xgboost_2,
        # wflw_spec_xgboost_3,
        # wflw_spec_xgboost_4,
        # wflw_spec_rf_1,
        # wflw_spec_rf_2,
        # wflw_spec_rf_3,
        # wflw_spec_rf_4,
        # wflw_spec_svm_1,
        # wflw_spec_svm_2,
        # wflw_spec_svm_3,
        # wflw_spec_svm_4,
        wflw_spec_cubist_1,
        wflw_spec_cubist_2,
        wflw_spec_cubist_3,
        wflw_spec_cubist_4,
        # wflw_spec_glm_1,
        # wflw_spec_glm_2,
        # wflw_spec_glm_3,
        # wflw_spec_glm_4,
        # wflw_spec_mars_1,
        # wflw_spec_mars_2,
        # wflw_spec_mars_3,
        # wflw_spec_mars_4,
        # wflw_spec_prophet_boost_1,
        # wflw_spec_prophet_boost_2,
        # wflw_spec_prophet_boost_3,
        # wflw_spec_prophet_boost_4,
        wflw_spec_prophet_boost_housing
    )
)

# ** SAFE FITTING ----
# - Models can fail in production. Don't include them if they fail.
message("Fitting Sub-Models")

tictoc::tic()
fitted_wflw_list <- map(spec_table$wflw_spec, .f = function(wflw) {
    res <- wflw %>% fit(train_cleaned)
    return(res)
})
tictoc::toc()

spec_fit_tbl <- spec_table %>%
    mutate(wflw_fit = fitted_wflw_list)

# ** SUBMODELS ----
# - Add models that passed to Modeltime Table

submodels_tbl <- as_modeltime_table(spec_fit_tbl$wflw_fit)

submodels_calib_tbl <- submodels_tbl %>%
    modeltime_calibrate(testing(splits), id = "id")

submodels_tbl <- NULL

submodels_accuracy_tbl <- submodels_calib_tbl %>%
    mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
        tbl %>%
            mutate(
                .actual     = .actual %>% trans_fun_inv(),
                .prediction = trans_fun_inv(.prediction),
                .residuals  = .actual - .prediction
            )
    })) %>%
    modeltime_accuracy(metric_set = metric_set(mae, smape, rmse, rsq),
                       acc_by_id = TRUE) 

submodels_accuracy_tbl %>% 
    group_by(id) %>% 
    table_modeltime_accuracy()

# print(submodels_accuracy_tbl %>% arrange(rmse))

bestinclass_models <- submodels_accuracy_tbl %>% 
    group_by(id, .model_desc) %>% 
    summarise(rmse = min(rmse)) %>% 
    ungroup() %>%
    left_join(submodels_accuracy_tbl) %>%
    group_by(id, .model_desc) %>% 
    summarise(mae = min(mae)) %>% 
    ungroup() %>%
    left_join(submodels_accuracy_tbl) %>%
    group_by(id, .model_desc) %>% 
    summarise(rsq = max(rsq)) %>% 
    ungroup() %>%
    left_join(submodels_accuracy_tbl) %>%
    group_by(id, .model_desc) %>% 
    summarise(.model_id = min(.model_id)) %>% 
    ungroup() %>%
    left_join(submodels_accuracy_tbl) %>% 
    select(id, .model_id)

submodels_for_ensemble <- bestinclass_models %>% 
    left_join(submodels_accuracy_tbl) %>% 
    group_by(id) %>% 
    mutate(avg = mean(rmse),
           grens = 1.5*mean(rmse)) %>% 
    filter(rmse < grens) %>% 
    filter(rmse < mean(rmse)) %>% 
    ungroup() %>% 
    select(id, .model_id)

# submodels_calib_tbl <- submodels_calib_tbl %>% 
    # filter(.model_id %in% bestinclass_models)

loadings_tbl <- submodels_for_ensemble %>% 
    left_join(submodels_accuracy_tbl) %>% 
    group_by(id) %>% 
    arrange(id, desc(rmse)) %>% 
    mutate(rowid = row_number()) %>% 
    mutate(loadings = 2^rowid) %>% 
    mutate(loadings = loadings/sum(loadings)) %>% 
    select(.model_id, loadings) %>% 
    ungroup()

# ** ENSEMBLE ----
# - Apply Exponential Average Strategy
message("Selecting Best Ensemble")

# test
submodels_calib_tbl %>% 
    unnest(.calibration_data) %>% 
    right_join(submodels_for_ensemble)

# werkt niet
modeltime_table(
    ensemble_weighted(submodels_calib_tbl %>% unnest(.calibration_data) %>% 
                          right_join(submodels_for_ensemble) %>% 
                          left_join(loadings_tbl) %>% 
                          nest_by(.model_id, .model_desc, .type, .key = ".calibration_data") %>% 
                          modeltime_table()
                      )
)

# werkt ook niet
submodels_for_ensemble %>% 
    left_join(submodels_calib_tbl) %>% 
    filter(id == "Total dwellings") %>% 
    select(-id) %>% 
    as_modeltime_table()
    modeltime_table()

submodels_tbl %>%
    modeltime_calibrate(testing(splits), id = "id") %>% 
    filter(.calibration_data$id == "Total dwellings")

# voorbeeld voor 1 id
# generate temporary ensemble tibble
temp_ens_tbl <- modeltime_table(
    ensemble_average(submodels_calib_tbl %>% 
                         filter(.model_id %in% 
                                    (submodels_for_ensemble %>% 
                                    filter(id == "Total dwellings") %>% 
                                    pull(.model_id))) %>% 
                         mutate(.calibration_data =
                                    map(.calibration_data, 
                                        ~ filter(., id == "Total dwellings")))
    )
)
# update ensemble description, to include the id
temp_ens_tbl <- temp_ens_tbl %>% 
    update_modeltime_description(.model_id       = 1, 
                                 .new_model_desc = str_c("Total dwellings - ", temp_ens_tbl$.model_desc))

temp_ens_cal_tbl <- temp_ens_tbl %>% 
    modeltime_calibrate(testing(splits) %>% filter(id == "Total dwellings"), id = "id") %>%
    mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
        tbl %>%
            mutate(
                .actual     = .actual %>% trans_fun_inv(),
                .prediction = .prediction %>% trans_fun_inv(),
                .residuals  = .actual - .prediction
            )
    })) 
temp_ens_cal_tbl %>%
    modeltime_accuracy() %>%
    select(.model_id, .model_desc, mae, rmse, rsq) 

ens_refit_tbl <- temp_ens_cal_tbl %>% 
    modeltime_refit(data_prepared_tbl)

ens_future_forecast_tbl <- ens_refit_tbl %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = actual_data,
        keep_data   = TRUE
    ) %>%
    mutate(
        across(.cols = c(.value, .conf_lo, .conf_hi),
               .fns  = function(x) trans_fun_inv(x))
    ) %>%
    group_by(id)

ens_future_forecast_tbl %>% 
    plot_modeltime_forecast(.facet_ncol         = 2,
                            .conf_interval_show = FALSE)



submodels_tbl %>% 
    filter(.model_id %in% submodels_for_ensemble$.model_id) %>% 
    ensemble_average() %>% 
    # left_join(submodels_calib_tbl %>% unnest(.calibration_data)) %>% 
    modeltime_calibrate(testing(splits), id = "id") %>% 
    modeltime_accuracy(acc_by_id = TRUE) %>% 
    table_modeltime_accuracy()
    
    ensemble_weighted()

modeltime_table(
    ensemble_average())
) %>%
    modeltime_calibrate(testing(splits))

weighted_ensemble_calib_tbl <- modeltime_table(
    ensemble_weighted(submodels_calib_tbl %>% filter(.model_id %in% submodels_for_ensemble$.model_id),
                      loadings = loadings_tbl$loadings)
) %>%
    modeltime_calibrate(testing(splits))

ensemble_accuracy_tbl <- modeltime_ensemble_calib_tbl %>%
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

print(ensemble_accuracy_tbl)

weighted_ensemble_accuracy_tbl <- weighted_ensemble_calib_tbl %>%
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

print(weighted_ensemble_accuracy_tbl)

calibration_tbl <- 
    combine_modeltime_tables(submodels_calib_tbl, 
                             modeltime_ensemble_calib_tbl,
                             weighted_ensemble_calib_tbl) %>%
    modeltime_calibrate(testing(splits)) 
accuracy_tbl <- calibration_tbl %>%
    mutate(.calibration_data = map(.calibration_data, .f = function(tbl) {
        tbl %>%
            mutate(
                .actual     = .actual %>% trans_fun_inv(),
                .prediction = .prediction %>% trans_fun_inv(),
                .residuals  = .actual - .prediction
            )
    })) %>%
    modeltime_accuracy() %>%
    select(.model_id, .model_desc, mae, rmse, rsq) %>%
    arrange(rmse)

best_model <- accuracy_tbl %>% dplyr::slice(1) %>% pull(.model_id)

# ** REFIT ----
message("Refitting Ensemble on Full Dataset")

refit_tbl <- calibration_tbl %>% 
    # filter(.model_id == best_model) %>%
    modeltime_refit(data_prepared_tbl)

# modeltime_ensemble_refit_tbl <- modeltime_ensemble_calib_tbl %>%
#     modeltime_refit(data_prepared_tbl)

future_forecast_tbl <- refit_tbl %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = actual_data,
        keep_data   = TRUE
    ) %>%
    mutate(
        across(.cols = c(.value, .conf_lo, .conf_hi),
               .fns  = function(x) trans_fun_inv(x))
    ) %>%
    group_by(id)

future_forecast_tbl %>% 
    plot_modeltime_forecast(.facet_ncol = 2,.conf_interval_show = TRUE)

message("\nDone!")


