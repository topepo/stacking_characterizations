library(tidymodels)
library(stacks)
library(embed)
library(rules)
library(doMC)

tidymodels_prefer()
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

source("helpers.R")

kc_set <- read_as_workflow_set(file.path("example_analyses", "kc", "base_fits"))

load(file.path("example_analyses", "kc", "kc_data.RData"))

# add candidates to a data stack
data_stack <- 
  stacks() %>%
  add_candidates(kc_set)

# define meta-learner
meta_spec <- 
  boost_tree(
    tree_depth = tune(),
    learn_rate = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    stop_iter = tune()
  ) %>% 
  set_engine('xgboost') %>%
  set_mode('regression')

meta_rec <-
  recipe(price ~ ., data = data_stack)

meta_learner <- 
  workflow() %>%
  add_model(meta_spec) %>%
  add_recipe(meta_rec)

# record time-to-fit for meta-learner fitting
timing <- system.time({
  set.seed(1)
  model_stack <-
    data_stack %>%
    blend_predictions(meta_learner = meta_learner)
})

model_stack_fitted <-
  add_members(model_stack, "kc")

metric <- model_stack$model_metrics[[1]]$.metric[[1]]

res_metric <-
  predict(model_stack_fitted, kc_test) %>%
  bind_cols(kc_test) %>%
  rmse(
    truth = !!attr(data_stack, "outcome"),
    estimate = .pred
  )

kc_stack_basic_xgb_metrics <- 
  list(
    dataset = "King County", 
    meta_learner = "xgboost", 
    time_to_fit = timing[["elapsed"]], 
    metric = metric, 
    metric_value = res_metric
  )

kc_stack_basic_xgb_metrics

save(
  kc_stack_basic_xgb_metrics, 
  file = file.path("metrics", "kc_basic_xgboost.RData")
)
