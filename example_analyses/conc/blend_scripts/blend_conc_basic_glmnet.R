library(tidymodels)
library(stacks)
library(baguette)
library(embed)
library(doMC)

tidymodels_prefer()
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

recipe <- "basic"
spec <- "glmnet"
dataset <- "conc"

source("helpers.R")

wf_set <- read_as_workflow_set(file.path("example_analyses", dataset, "candidate_fits"))

load(file.path("example_analyses", dataset, paste0(dataset, "_data.RData")))

# no need to define a meta-learner--use the glmnet default.

# add candidates to a data stack
data_stack <- 
  stacks() %>%
  add_candidates(wf_set)

# record time-to-fit for meta-learner fitting
timing <- system.time({
  set.seed(1)
  model_stack <-
    data_stack %>%
    blend_predictions()
})

model_stack_fitted <-
  add_members(model_stack, dataset)

metric <- model_stack$model_metrics[[1]]$.metric[[1]]

res_metric <-
  predict(model_stack_fitted, test) %>%
  bind_cols(test) %>%
  rmse(
    truth = !!attr(data_stack, "outcome"),
    estimate = .pred
  )

res <- 
  list(
    dataset = dataset, 
    recipe = recipe,
    spec = spec,
    time_to_fit = timing[["elapsed"]], 
    metric = metric, 
    metric_value = res_metric$.estimate
  )

save(
  res, 
  file = file.path("metrics", paste0(dataset, "_", recipe, "_", spec, ".RData"))
)
