
library(tidymodels)
library(agua)
library(h2o)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)

h2o.init()

# ------------------------------------------------------------------------------

load(file.path("example_analyses", "barley_data.RData"))

# ------------------------------------------------------------------------------

basic_recipe <- 
  recipe(barley ~ ., data = barley_train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_predictors())

pca_recipe <- 
  recipe(barley ~ ., data = barley_train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), num_comp = 50) %>% 
  step_normalize(starts_with("PC"))

pls_recipe <- 
  recipe(barley ~ ., data = barley_train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pls(all_predictors(), num_comp = 50, outcome = vars(barley))

# ------------------------------------------------------------------------------

set.seed(1)
barly_aml_spec <-
  auto_ml() %>%
  set_engine(
    "h2o",
    verbosity = "info",
    max_runtime_secs = 60 * 60,
    seed = 1000
  ) %>%
  set_mode("regression")

barly_pca_aml_wflow <- 
  workflow() %>% 
  add_recipe(pca_recipe) %>% 
  add_model(barly_aml_spec)

barly_pca_aml_fit <- 
  barly_pca_aml_wflow %>% 
  fit(data = barley_not_test)

# autoplot(barly_pca_aml_fit, metric = "rmse")

barly_pca_aml_fit %>% 
  extract_fit_engine() %>% 
  get_leaderboard()

# ------------------------------------------------------------------------------

barly_pls_aml_wflow <- 
  barly_pca_aml_wflow %>% 
  update_recipe(pls_recipe)

barly_pls_aml_fit <- 
  barly_pls_aml_wflow %>% 
  fit(data = barley_not_test)

# autoplot(barly_pls_aml_fit, metric = "rmse")

barly_pls_aml_fit %>% 
  extract_fit_engine() %>% 
  get_leaderboard()


# ------------------------------------------------------------------------------
# Evaluate testing data

predict(barly_pls_aml_fit, barley_test) %>% 
  bind_cols(barley_test) %>% 
  rmse(barley, .pred)

# Single best fit has
# A tibble: 1 × 3
#>     .metric .estimator .estimate
#>     <chr>   <chr>          <dbl>
#>   1 rmse    standard        2.45

# ------------------------------------------------------------------------------

h2o.shutdown(prompt = FALSE)

sessioninfo::session_info()

q(save = "no")

