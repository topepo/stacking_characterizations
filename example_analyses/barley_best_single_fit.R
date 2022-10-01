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
barley_test  <-  testing(barley_split)

set.seed(2323)
barley_rs <- validation_split(barley_not_test, prop = 1 - (500 / nrow(barley_not_test)))
barley_train <- analysis(barley_rs$splits[[1]])

# ------------------------------------------------------------------------------

ctrl_grd <-
  control_grid(
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

# ------------------------------------------------------------------------------
# Recollect data into a workflow set then a data stack

rdata_files <- list.files("example_analyses", pattern = "barley_", full.names = TRUE, )
rdata_files <- rdata_files[!grepl("barley_data", rdata_files)]
rdata_files <- rdata_files[grepl("RData$", rdata_files)]

return_row <- function(x) {
  load(x)
  get(gsub("\\.RData", "", basename(x)))
}

barley_res <- map_dfr(rdata_files, return_row)

# ------------------------------------------------------------------------------

barley_best <- 
  barley_res %>% 
  rank_results(rank_metric = "rmse") %>% 
  filter(.metric == "rmse") %>% 
  slice(1)

print(barley_best)

barley_best_res <- 
  barley_res %>% 
  extract_workflow_set_result(id = barley_best$wflow_id)

barley_best_config <-
  barley_best_res %>% 
  select_best(metric = "rmse")

print(barley_best_config)

barley_best_fit <- 
  barley_best_res %>% 
  extract_workflow() %>% 
  finalize_workflow(barley_best_config) %>% 
  fit(data = barley_train)

# ------------------------------------------------------------------------------
# Evaluate testing data

predict(barley_best_fit, barley_test) %>% 
  bind_cols(barley_test) %>% 
  rmse(barley, .pred)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")

