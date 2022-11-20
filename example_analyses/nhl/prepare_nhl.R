library(parallelly)
library(ongoal)
library(tidymodels)
library(embed)
library(bonsai)
library(discrim)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

fits_dir <- file.path("example_analyses", "nhl", "candidate_fits")

if (!dir.exists(fits_dir)) {
  dir.create(fits_dir)
}

# ------------------------------------------------------------------------------

pittsburgh <-
  on_goal %>%
  arrange(game_id, event_idx) %>%
  select(-game_id, -event_idx, -date_time, -event, -distance)

# ------------------------------------------------------------------------------

set.seed(1)
nhl_split <- initial_time_split(pittsburgh, prop = 3/4)
nhl_not_test <- training(nhl_split)
test <- testing(nhl_split)

set.seed(2)
nhl_val_split <- validation_split(nhl_not_test, prop = 9/10)
train <- training(nhl_val_split$splits[[1]])

# ------------------------------------------------------------------------------

basic_recipe <-
  recipe(formula = on_goal ~ ., data = train)

effects_encode_recipe <-
  basic_recipe %>%
  step_lencode_mixed(shooter, goaltender, shooter_nationality, outcome = vars(on_goal))

effects_and_dummy_recipe <-
  effects_encode_recipe %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_numeric_predictors())

all_dummy_recipe <-
  basic_recipe %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_lincomb(all_numeric_predictors())

spline_recipe <-
  effects_and_dummy_recipe %>%
  step_ns(angle, deg_free = tune("angle")) %>%
  step_ns(coord_x, deg_free = tune("coord_x")) %>%
  step_ns(game_seconds, deg_free = tune("game_seconds"))

normalized_recipe <-
  effects_and_dummy_recipe %>%
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

glmn_recipe <-
  effects_encode_recipe %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>% 
  step_ns(angle, deg_free = 50) %>% 
  step_ns(coord_x, deg_free = 50) %>% 
  step_ns(game_seconds, deg_free = 50) %>%
  step_normalize(all_numeric_predictors())

glmn_spline_wflow <- 
  workflow() %>% 
  add_model(glmn_spec) %>% 
  add_recipe(glmn_recipe)

set.seed(391)
glmn_spline_res <-
  glmn_spline_wflow %>%
  tune_grid(
    resamples = nhl_val_split,
    control = grid_ctrl,
    grid = 25
  )

save(
  glmn_spline_res,
  file = file.path(fits_dir, "nhl_glmnet.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

gam_f <- 
  on_goal ~ s(coord_x) + s(angle) + s(game_seconds) + goal_difference + 
  shooter + goaltender + period +  extra_attacker + season + behind_goal_line + 
  home_skaters + away_skaters

gam_spec <-
  gen_additive_mod(select_features = tune()) %>%
  set_mode("classification")

gam_workflow <-
  workflow() %>%
  add_recipe(effects_encode_recipe) %>%
  add_model(gam_spec, formula = gam_f)

gam_res <-
  tune_grid(gam_workflow, resamples = nhl_val_split, control = grid_ctrl)

save(
  gam_res,
  file = file.path(fits_dir, "nhl_gam.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

fda_gcv_res <-
  discrim_flexible(prod_degree = tune()) %>%
  tune_grid(effects_encode_recipe, resamples = nhl_val_split, grid = 2,
            control = grid_ctrl)

fda_grid <- crossing(prod_degree = 1:2, num_terms = 2:25)

fda_manual_res <-
  discrim_flexible(prod_degree = tune(), num_terms = tune(), prune_method = "none") %>%
  tune_grid(effects_encode_recipe, resamples = nhl_val_split, grid = fda_grid,
            control = grid_ctrl)

save(
  fda_manual_res,
  file = file.path(fits_dir, "nhl_fda.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification")

svm_workflow <-
  workflow() %>%
  add_recipe(normalized_recipe) %>%
  add_model(svm_spec)

set.seed(9264)
svm_res <-
  tune_grid(
    svm_workflow,
    resamples = nhl_val_split,
    grid = 25,
    control = grid_ctrl
  )

save(
  svm_res,
  file = file.path(fits_dir, "nhl_svm.RData"),
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
    resamples = nhl_val_split,
    grid = 25,
    control = grid_ctrl
  )

save(
  bart_res,
  file = file.path(fits_dir, "nhl_bart.RData"),
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
    resamples = nhl_val_split,
    grid = 25,
    param_info = lgb_param,
    control = grid_ctrl
  )

save(lgb_res, file = file.path(fits_dir, "nhl_lgb.RData"), compress = "xz", compression_level = 9)

# ------------------------------------------------------------------------------

save(
  train, test,
  file = file.path("example_analyses", "nhl", "nhl_data.RData")
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")

