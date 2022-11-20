library(parallelly)
library(ongoal)
library(tidymodels)
library(bonsai)
library(rules)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

# ------------------------------------------------------------------------------

load(file.path("data", "chimiometrie_2019.RData"))

chimiometrie_2019 <-
  chimiometrie_2019  %>%
  select(-soy_oil, -lucerne)

set.seed(87)
barley_split <-
  initial_split(chimiometrie_2019,
                prop = 1 - (500 / nrow(chimiometrie_2019)))
barley_not_test <- training(barley_split)
test  <-  testing(barley_split)

set.seed(2323)
barley_rs <- validation_split(barley_not_test, prop = 1 - (500 / nrow(barley_not_test)))
train <- analysis(barley_rs$splits[[1]])

# ------------------------------------------------------------------------------

ctrl_grd <-
  control_grid(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

# ------------------------------------------------------------------------------

basic_recipe <- 
  recipe(barley ~ ., data = train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_predictors())

pca_recipe <- 
  recipe(barley ~ ., data = train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), num_comp = tune()) %>% 
  step_normalize(starts_with("PC"))

pls_recipe <- 
  recipe(barley ~ ., data = train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pls(all_predictors(), num_comp = tune(), outcome = vars(barley))

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
  set_mode('regression') %>% 
  set_engine("nnet", MaxNWts = 5000)

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

barley_set <-
  workflow_set(
    list(none = basic_recipe, pca = pca_recipe, pls = pls_recipe),
    list(
      cubist = cb_spec, knn = kknn_spec, mars = mars_spec,
      svm = svm_rbf_spec, nnet = nnet_spec, bart = bart_spec
    )
  )

# ------------------------------------------------------------------------------
# Adjust the ranges of some of the preprocessing tuning parameters

for (model in grep("(pca)|(pls)", barley_set$wflow_id, value = TRUE)) {
  prms <-
    barley_set %>%
    extract_parameter_set_dials(model) %>%
    update(num_comp = num_comp(c(1, 50)))
  
  barley_set <-
    barley_set %>%
    option_add(id = model, param_info = prms)
}

# ------------------------------------------------------------------------------

barley_res <-
  barley_set %>%
  workflow_map(
    resamples = barley_rs,
    grid = 25,
    seed = 4580,
    control = ctrl_grd,
    verbose = TRUE
  )

# ------------------------------------------------------------------------------
# Save entries in the workflow set separately to reduce the size of the RData file
fits_dir <- file.path("example_analyses", "barley", "candidate_fits")

if (!dir.exists(fits_dir)) {
  dir.create(fits_dir)
}

for (i in seq_along(barley_res$wflow_id)) {
  obj_nm <- paste0("barley_", barley_res$wflow_id[i])
  file_nm <- file.path(fits_dir, paste0(obj_nm, ".RData"))
  assign(obj_nm, value = barley_res %>% dplyr::slice(i))
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}

save(
  train, test,
  file = file.path("example_analyses", "barley", "barley_data.RData"), 
  compress = "xz", 
  compression_level = 9
)
  
# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")

