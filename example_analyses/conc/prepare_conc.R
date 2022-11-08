library(parallelly)
library(ongoal)
library(tidymodels)
library(bonsai)
library(baguette)
library(rules)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

fits_dir <- file.path("example_analyses", "conc", "candidate_fits")

if (!dir.exists(fits_dir)) {
  dir.create(fits_dir)
}

# ------------------------------------------------------------------------------

data(concrete) 

concrete <- 
  concrete %>% 
  group_by(cement, blast_furnace_slag, fly_ash, water, superplasticizer, 
           coarse_aggregate, fine_aggregate, age) 

averaged <- 
  concrete %>% 
  summarize(compressive_strength = mean(compressive_strength), .groups = "drop")

set.seed(1701)
conc_split <- initial_split(averaged, strata = compressive_strength)
conc_train <- training(conc_split)
conc_test  <- testing(conc_split)

set.seed(1702) 
conc_rs <- vfold_cv(conc_train, strata = compressive_strength)

# ------------------------------------------------------------------------------

grid_ctrl <-
  control_grid(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

# ------------------------------------------------------------------------------

predictors <- names(conc_train)[names(conc_train) != "compressive_strength"]

conc_rec <- 
  recipe(compressive_strength ~ ., data = conc_train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors())

conc_quad_rec <- 
  conc_rec %>% 
  step_interact(~ all_predictors():all_predictors()) %>% 
  step_poly(all_of(predictors), degree = 2)

# ------------------------------------------------------------------------------

bag_tree_rpart_spec <-
  bag_tree() %>%
  set_engine('rpart') %>%
  set_mode('regression')

bart_dparts_spec <-
  parsnip::bart(
    prior_terminal_node_coef = tune(),
    prior_terminal_node_expo = tune(),
    prior_outcome_range = tune(),
    trees = 500
  ) %>%
  set_mode("regression")

boost_tree_xgboost_spec <-
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
    epochs =tune()
  ) %>%
  set_engine("nnet", MaxNWts = 2000) %>% 
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

gam_wflow <- 
  workflow() %>% 
  add_model(
    gam_mgcv_spec,
    formula = compressive_strength ~ s(cement) + s(blast_furnace_slag) + 
      s(fly_ash) + s(water) + s(superplasticizer) + s(coarse_aggregate) + 
      s(fine_aggregate) + s(age)
  ) %>% 
  add_recipe(conc_rec)

# ------------------------------------------------------------------------------

main_models <- 
  list(
    bag_tree = bag_tree_rpart_spec,
    bart = bart_dparts_spec,
    xgboost = boost_tree_xgboost_spec,
    lightgbm = boost_tree_lgb_spec,
    cubist_rules = cubist_rules_Cubist_spec,
    decision_tree = decision_tree_rpart_spec,
    mars = mars_earth_spec,
    mlp = mlp_spec,
    nearest_neighbor = nearest_neighbor_kknn_spec,
    rand_forest = rand_forest_ranger_spec,
    svm_rbf = svm_rbf_kernlab_spec
  )

quad_models <- list(linear_reg = linear_reg_glmnet_spec)

# ------------------------------------------------------------------------------


conc_wflow_set <- 
  workflow_set(
    preproc = list(plain = conc_rec),
    models = main_models
  ) %>% 
  bind_rows(
    workflow_set(
      preproc = list(quadratic = conc_quad_rec),
      models = quad_models
    )
  ) %>% 
  option_add(id = "plain_mlp", param_info = mlp_param)


set.seed(1703)
gam_res <-
  gam_wflow %>%
  tune_grid(
    resamples = conc_rs,
    grid = 25,
    control = grid_ctrl
  )

conc_res <-
  conc_wflow_set %>%
  workflow_map(
    verbose = FALSE,
    seed = 1704,
    grid = 25,
    resamples = conc_rs,
    control = grid_ctrl
  ) %>% 
  bind_rows(as_workflow_set(plain_gam = gam_res))

# ------------------------------------------------------------------------------
# Save entries in the workflow set separately to reduce the size of the RData file

for (i in seq_along(conc_res$wflow_id)) {
  obj_nm <- paste0("conc_", conc_res$wflow_id[i])
  file_nm <- file.path(fits_dir, paste0(obj_nm, ".RData"))
  assign(obj_nm, value = conc_res %>% dplyr::slice(i))
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}

save(
  list = ls(pattern = "(_train$)|(_test$)"),
  file = file.path("example_analyses", "conc", "conc_data.RData"), 
  compress = "xz", 
  compression_level = 9
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")

