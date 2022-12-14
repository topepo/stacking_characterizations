library(parallelly)
library(tidymodels)
library(embed)
library(discrim)
library(bonsai)
library(baguette)
library(rules)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

fits_dir <- file.path("example_analyses", "rare", "candidate_fits")

if (!dir.exists(fits_dir)) {
  dir.create(fits_dir)
}

# ------------------------------------------------------------------------------

n <- 50000

set.seed(1)
class_sim_caret <- 
  sim_classification(n, num_linear = 25, intercept = -20) %>% 
  bind_cols(
    sim_noise(n, num_vars = 50, cov_type = "toeplitz", cov_param = 1 / 2)
  )


set.seed(1701)
rare_split <- initial_split(class_sim_caret, strata = class)
train <- training(rare_split)
test  <- testing(rare_split)

set.seed(1702) 
rare_rs <- vfold_cv(train, strata = class)

# ------------------------------------------------------------------------------

grid_ctrl <-
  control_grid(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

basic_recipe <-
  recipe(class ~ ., data = train) %>%
  step_normalize(all_numeric_predictors())

# ------------------------------------------------------------------------------

grid_ctrl <-
  control_grid(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )
bay_ctrl <-
  control_bayes(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE,
    verbose = TRUE,
    no_improve = 20L
  )
resamp_ctrl <-
  control_resamples(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

# ------------------------------------------------------------------------------

glmn_spec <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

glmn_spline_wflow <- 
  workflow() %>% 
  add_model(glmn_spec) %>% 
  add_recipe(basic_recipe)

set.seed(391)
glmn_spline_res <-
  glmn_spline_wflow %>%
  tune_grid(
    resamples = rare_rs,
    control = grid_ctrl,
    grid = 25
  )

save(
  glmn_spline_res,
  file = file.path(fits_dir, "rare_glmnet.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

fda_grid <- crossing(prod_degree = 1:2, num_terms = 2:25)

fda_manual_res <-
  discrim_flexible(prod_degree = tune(), num_terms = tune(), prune_method = "none") %>%
  tune_grid(basic_recipe, resamples = rare_rs, grid = fda_grid,
            control = grid_ctrl)

save(
  fda_manual_res,
  file = file.path(fits_dir, "rare_fda.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification")

svm_workflow <-
  workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(svm_spec)

set.seed(9264)
svm_res <-
  tune_grid(
    svm_workflow,
    resamples = rare_rs,
    grid = 25,
    control = grid_ctrl
  )

save(
  svm_res,
  file = file.path(fits_dir, "rare_svm.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------


nnet_spec <-
  mlp(hidden_units = tune::tune(),
      penalty = tune::tune(),
      epochs = tune()
  ) %>%
  set_engine("nnet", MaxNWts = 10000) %>% 
  set_mode('classification')

nnet_param <- 
  nnet_spec %>% 
  extract_parameter_set_dials() %>% 
  update(hidden_units = hidden_units(c(2, 25)))

nnet_workflow <-
  workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(nnet_spec)

set.seed(9264)
nnet_res <-
  tune_grid(
    nnet_workflow,
    resamples = rare_rs,
    grid = 25,
    control = grid_ctrl,
    param_info = nnet_param
  )

save(
  nnet_res,
  file = file.path(fits_dir, "rare_nnet.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

kknn_spec <-
  nearest_neighbor(
    neighbors = tune::tune(),
    weight_func = tune::tune(),
    dist_power = tune::tune()
  ) %>%
  set_mode('classification')

knn_workflow <-
  workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(kknn_spec)

set.seed(9264)
knn_res <-
  tune_grid(
    knn_workflow,
    resamples = rare_rs,
    grid = 25,
    control = grid_ctrl
  )

save(
  knn_res,
  file = file.path(fits_dir, "rare_knn.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

bart_spec <-
  parsnip::bart(prior_terminal_node_coef = tune(),
                prior_terminal_node_expo = tune()) %>%
  set_mode("classification")

bart_workflow <-
  workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(bart_spec)

set.seed(8449)
bart_res <-
  tune_grid(
    bart_workflow,
    resamples = rare_rs,
    grid = 25,
    control = grid_ctrl
  )

save(
  bart_res,
  file = file.path(fits_dir, "rare_bart.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

lgb_spec <-
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
             loss_reduction = tune(), sample_size = tune(), mtry = tune()) %>%
  set_engine("lightgbm", num_threads = 1) %>% 
  set_mode("classification")

lgb_param <-
  lgb_spec %>%
  extract_parameter_set_dials() %>%
  update(learn_rate = learn_rate(c(-2, 0)))

lgb_workflow <-
  workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(lgb_spec)

set.seed(3803)
lgb_res <-
  lgb_workflow %>%
  tune_grid(
    resamples = rare_rs,
    grid = 25,
    param_info = lgb_param,
    control = grid_ctrl
  )

save(lgb_res, file = file.path(fits_dir, "rare_lgb.RData"), compress = "xz", compression_level = 9)

# ------------------------------------------------------------------------------

save(
  train, test,
  file = file.path("example_analyses", "rare", "rare_data.RData")
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")

