library(parallelly)
library(ongoal)
library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(doMC)
library(KingCountyHouses)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

# ------------------------------------------------------------------------------

kc_data <-
  KingCountyHouses::home_prices %>%
  mutate(across(where(is.character), as.factor)) 

set.seed(9276)
kc_split <- initial_split(kc_data)
kc_train <- training(kc_split)
kc_test  <-  testing(kc_split)

set.seed(1184)
kc_rs <- vfold_cv(kc_train)

# ------------------------------------------------------------------------------

ctrl_grd <-
  control_grid(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

# ------------------------------------------------------------------------------

basic_recipe <-
  recipe(price ~ ., data = kc_train) %>%
  step_YeoJohnson(contains("sqft")) %>% 
  step_lencode_mixed(zip_code, outcome = vars(price)) %>%
  step_date(date_sold) %>%
  step_rm(date_sold)

dummy_recipe <-
  basic_recipe %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# ------------------------------------------------------------------------------

cb_spec <- cubist_rules(committees = tune::tune(), neighbors = tune::tune())

bart_spec <-
  bart(
    trees = tune::tune(),
    prior_terminal_node_coef = tune::tune(),
    prior_terminal_node_expo = tune::tune(),
    prior_outcome_range = tune::tune()
  ) %>%
  set_mode('regression')

mars_spec <-
  mars(prod_degree = tune::tune()) %>%
  set_mode('regression')

nnet_spec <-
  mlp(hidden_units = tune::tune(),
      penalty = tune::tune(),
      epochs = tune()
  ) %>%
  set_mode('regression')

kknn_spec <-
  nearest_neighbor(
    neighbors = tune::tune(),
    weight_func = tune::tune(),
    dist_power = tune::tune()
  ) %>%
  set_mode('regression')

svm_rbf_spec <-
  svm_rbf(cost = tune::tune(),
          rbf_sigma = tune::tune(),
          margin = tune::tune()) %>%
  set_mode('regression')

kc_set <-
  bind_rows(
    workflow_set(
      list(basic = basic_recipe),
      list(
        cubist = cb_spec, bart = bart_spec
      )
    ),
    workflow_set(
      list(indicators = dummy_recipe),
      list(
        knn = kknn_spec, mars = mars_spec,
        svm = svm_rbf_spec, nnet = nnet_spec
      )
    )
  )

# ------------------------------------------------------------------------------

kc_res <-
  kc_set %>%
  workflow_map(
    resamples = kc_rs,
    grid = 25,
    seed = 6485,
    control = ctrl_grd,
    verbose = TRUE
  )

# ------------------------------------------------------------------------------
# Save entries in the workflow set separately to reduce the size of the RData file

for (i in seq_along(kc_res$wflow_id)) {
  obj_nm <- paste0("kc_", kc_res$wflow_id[i])
  file_nm <- file.path("example_analyses", "kc", "base_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = kc_res %>% dplyr::slice(i))
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}

save(
  list = ls(pattern = "(_train$)|(_test$)"),
  file = file.path("example_analyses", "kc", "kc_data.RData"),
  compress = "xz",
  compression_level = 9
)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q(save = "no")
}

