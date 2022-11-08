library(parallelly)
library(tidytuesdayR)
library(tidymodels)
library(embed)
library(bonsai)
library(baguette)
library(rules)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

fits_dir <- file.path("example_analyses", "wind", "candidate_fits")

if (!dir.exists(fits_dir)) {
  dir.create(fits_dir)
}

# ------------------------------------------------------------------------------

wind_turbine_raw <- tt_load("2020-10-27")$`wind-turbine`

wind_turbine <-
  wind_turbine_raw %>%
  rename(capacity = total_project_capacity_mw) %>%
  mutate(
    turbine_number_in_project = gsub("[[:digit:]]/", "", turbine_number_in_project),
    turbine_number_in_project = as.numeric(turbine_number_in_project),
    across(where(is.character), as.factor)
  ) %>%
  select(-objectid, -notes, -turbine_identifier, -project_name)

set.seed(1701)
wind_split <- initial_split(wind_turbine, strata = capacity)
wind_train <- training(wind_split)
wind_test  <- testing(wind_split)

set.seed(1702) 
wind_rs <- vfold_cv(wind_train, strata = capacity)

# ------------------------------------------------------------------------------

grid_ctrl <-
  control_grid(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

# ------------------------------------------------------------------------------

base_rec <- 
  recipe(capacity ~ ., data = wind_train) %>% 
  step_lencode_mixed(model, manufacturer, outcome = vars(capacity)) %>% 
  step_impute_knn(turbine_rated_capacity_k_w)

dummy_rec <- 
  base_rec %>% 
  step_dummy_extract(
    commissioning_date,
    sep = "/",
    naming = function(var, lvl, ordinal = FALSE, sep = "_") paste0("year_", lvl)
  ) %>%   
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# ------------------------------------------------------------------------------

bag_tree_rpart_spec <-
  bag_tree() %>%
  set_engine('rpart') %>%
  set_mode('regression')

boost_tree_lgb_spec <-
  boost_tree(
    tree_depth = tune(),
    trees = tune(),
    learn_rate = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune()
  ) %>% 
  set_engine('lightgbm') %>%
  set_mode('regression')

cubist_rules_Cubist_spec <-
  cubist_rules(committees = tune(), neighbors = tune()) %>%
  set_engine('Cubist')

decision_tree_rpart_spec <-
  decision_tree(min_n = tune(), cost_complexity = tune()) %>%
  set_engine('rpart') %>%
  set_mode('regression')

gam_mgcv_spec <- 
  gen_additive_mod(select_features = tune(), adjust_deg_free = tune()) %>% 
  set_mode("regression")

linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')

mars_earth_spec <-
  mars(prod_degree = tune()) %>%
  set_engine('earth') %>%
  set_mode('regression')

mlp_spec <-
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>%
  set_engine("nnet", MaxNWts = 10000) %>% 
  set_mode('regression')

mlp_param <- 
  mlp_spec %>% 
  extract_parameter_set_dials() %>% 
  update(hidden_units = hidden_units(c(2, 200)))

nearest_neighbor_kknn_spec <-
  nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_engine('kknn') %>%
  set_mode('regression')

rand_forest_ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('ranger') %>%
  set_mode('regression')

svm_rbf_kernlab_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('regression')

# ------------------------------------------------------------------------------

models_1 <- 
  list(
    bag_tree = bag_tree_rpart_spec,
    lightgbm = boost_tree_lgb_spec,
    cubist_rules = cubist_rules_Cubist_spec,
    decision_tree = decision_tree_rpart_spec,
    mars = mars_earth_spec,
    rand_forest = rand_forest_ranger_spec
  )

models_2 <- 
  list(
    mlp = mlp_spec,
    nearest_neighbor = nearest_neighbor_kknn_spec,
    svm_rbf = svm_rbf_kernlab_spec
  )


# ------------------------------------------------------------------------------


wind_wflow_set <- 
  workflow_set(
    preproc = list(plain = base_rec),
    models = models_1
  ) %>% 
  bind_rows(
    workflow_set(
      preproc = list(indicators = dummy_rec),
      models = models_2
    )
  ) %>% 
  option_add(id = "indicators_mlp", param_info = mlp_param)

wind_res <-
  wind_wflow_set %>%
  workflow_map(
    verbose = TRUE,
    seed = 1704,
    grid = 25,
    resamples = wind_rs,
    control = grid_ctrl
  ) 

# ------------------------------------------------------------------------------
# Save entries in the workflow set separately to reduce the size of the RData file

for (i in seq_along(wind_res$wflow_id)) {
  obj_nm <- paste0("wind_", wind_res$wflow_id[i])
  file_nm <- file.path(fits_dir, paste0(obj_nm, ".RData"))
  assign(obj_nm, value = wind_res %>% dplyr::slice(i))
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}

save(
  list = ls(pattern = "(_train$)|(_test$)"),
  file = file.path("example_analyses", "wind", "wind_data.RData"), 
  compress = "xz", 
  compression_level = 9
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")

