library(parallelly)
library(tidymodels)
library(baguette)
library(bonsai)
library(discrim)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

fits_dir <- file.path("example_analyses", "hpc", "candidate_fits")

if (!dir.exists(fits_dir)) {
  dir.create(fits_dir)
}

# ------------------------------------------------------------------------------

data(hpc_data, package = "modeldata")

set.seed(1373)
hpc_split <- initial_split(hpc_data, prop = 8 / 10, strata = class)
hpc_train <- training(hpc_split)
hpc_test  <- testing(hpc_split)

set.seed(4497)
hpc_rs <- vfold_cv(hpc_train, repeats = 2)

ctrl_grid <- control_grid(save_workflow = TRUE, save_pred = TRUE)

# ------------------------------------------------------------------------------

bland_recipe <- recipe(formula = class ~ ., data = hpc_data)

norm_recipe <-
  recipe(formula = class ~ ., data = hpc_data) %>%
  step_log(compounds, input_fields) %>%
  step_log(num_pending, offset = 1) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

glmnet_recipe <- 
  recipe(formula = class ~ ., data = hpc_train) %>%
  step_log(compounds, input_fields) %>% 
  step_log(num_pending, offset = 1) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_ns(compounds, deg_free = tune("compounds")) %>% 
  step_ns(input_fields, deg_free = tune("inputs")) 

# ------------------------------------------------------------------------------

glmnet_spec <- 
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet") 

glmnet_workflow <- 
  workflow() %>% 
  add_recipe(glmnet_recipe) %>% 
  add_model(glmnet_spec) 

glmnet_param <- 
  glmnet_workflow %>% 
  extract_parameter_set_dials() %>% 
  update(
    compounds    = spline_degree(c(2L, 50L)),
    inputs = spline_degree(c(2L, 50L))
  )

set.seed(23892)
glmnet_grid <- 
  glmnet_param %>% 
  grid_max_entropy(size = 25)

set.seed(9624)
glmnet_res <-
  tune_grid(
    glmnet_workflow,
    resamples = hpc_rs,
    grid = glmnet_grid,
    param_info = glmnet_param,
    control = ctrl_grid
  ) 

save(
  glmnet_res,
  file = file.path(fits_dir, "hpc_glmnet.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

cart_bag_spec <- 
  bag_tree() %>%
  set_engine("rpart", times = 50) %>% 
  set_mode("classification")

cart_bag_workflow <- 
  workflow() %>% 
  add_recipe(bland_recipe) %>% 
  add_model(cart_bag_spec) 

set.seed(9624)
cart_bag_res <-
  fit_resamples(
    cart_bag_workflow,
    resamples = hpc_rs,
    control = ctrl_grid
  ) 

save(
  cart_bag_res,
  file = file.path(fits_dir, "hpc_cart_bag.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

fda_recipe <- 
  recipe(formula = class ~ ., data = hpc_data) %>%
  step_log(compounds, input_fields) %>% 
  step_log(num_pending, offset = 1)

fda_spec <- 
  discrim_flexible(num_terms = tune(), prod_degree = tune(), prune_method = "none")

fda_workflow <- 
  workflow() %>% 
  add_recipe(fda_recipe) %>% 
  add_model(fda_spec) 

fda_grid <- crossing(prod_degree = 1:2, num_terms = 2:40)

set.seed(9624)
fda_res <-
  tune_grid(
    fda_workflow,
    resamples = hpc_rs,
    grid = fda_grid,
    control = ctrl_grid
  ) 

save(
  fda_res,
  file = file.path(fits_dir, "hpc_fda.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

nnet_spec <-
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>%
  set_engine("nnet", MaxNWts = 5000) %>%
  set_mode("classification")

nnet_workflow <-
  workflow() %>%
  add_recipe(norm_recipe) %>%
  add_model(nnet_spec)

nnet_param <-
  nnet_workflow %>%
  extract_parameter_set_dials() %>%
  update(
    hidden_units = hidden_units(c(2, 75))
  )

set.seed(9624)
nnet_res <-
  tune_grid(
    nnet_workflow,
    resamples = hpc_rs,
    grid = 25,
    param_info = nnet_param,
    control = ctrl_grid
  )

save(
  nnet_res,
  file = file.path(fits_dir, "hpc_nnet.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

svmp_spec <- 
  svm_poly(cost = tune(), degree = tune(), scale_factor = tune()) %>% 
  set_mode("classification") 

svmp_workflow <- 
  workflow() %>% 
  add_recipe(norm_recipe) %>% 
  add_model(svmp_spec) 

svmp_param <- 
  svmp_workflow %>% 
  extract_parameter_set_dials() 

set.seed(9624)
svmp_res <-
  tune_grid(
    svmp_workflow,
    resamples = hpc_rs,
    grid = 25,
    param_info = svmp_param,
    control = ctrl_grid
  ) 

save(
  svmp_res,
  file = file.path(fits_dir, "hpc_svm.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

xgb_recipe <- 
  recipe(formula = class ~ ., data = hpc_data) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) 

xgb_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost") 

xgb_workflow <- 
  workflow() %>% 
  add_recipe(xgb_recipe) %>% 
  add_model(xgb_spec) 

set.seed(9624)
xgb_res <-
  tune_grid(
    xgb_workflow,
    resamples = hpc_rs,
    grid = 25,
    control = ctrl_grid
  ) 

save(
  xgb_res,
  file = file.path(fits_dir, "hpc_xgb.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

save(
  list = ls(pattern = "(_train$)|(_test$)"),
  file = file.path("example_analyses", "hpc", "hpc_data.RData"), 
  compress = "xz", 
  compression_level = 9
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")
