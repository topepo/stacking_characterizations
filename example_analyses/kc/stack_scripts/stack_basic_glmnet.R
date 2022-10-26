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

# no need to define a meta-learner--use the glmnet default.

# add candidates to a data stack
data_stack <- 
  stacks() %>%
  add_candidates(kc_set)

# record time-to-fit for meta-learner fitting
timing <- system.time({
  set.seed(1)
  model_stack <-
    data_stack %>%
    blend_predictions()
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

kc_stack_basic_glmnet_metrics <- 
  list(
    dataset = "King County", 
    meta_learner = "glmnet", 
    time_to_fit = timing[["elapsed"]], 
    metric = metric, 
    metric_value = res_metric$.estimate
  )

kc_stack_basic_glmnet_metrics

save(
  kc_stack_basic_glmnet_metrics, 
  file = file.path("metrics", "kc_basic_glmnet.RData")
)
