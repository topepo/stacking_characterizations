
library(tidymodels)
library(bonsai)
library(rules)
library(stacks)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE)
registerDoMC(cores = parallelly::availableCores())

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

barley_data_stack <-
  stacks() %>% 
  add_candidates(barley_res)

# Load test data

load(file.path("example_analyses", "barley_data.RData"))

# ------------------------------------------------------------------------------
# Basic stacked ensemble with linear, non-negative weights

set.seed(4085)
barley_non_neg_stack <- 
  barley_data_stack %>% 
  blend_predictions()

autoplot(barley_non_neg_stack)

# Adjust the penalty a bit
set.seed(4085)
barley_non_neg_stack <- 
  barley_data_stack %>% 
  blend_predictions(penalty = 10^seq(-5, 1, length.out = 20))

autoplot(barley_non_neg_stack) + 
  labs(title = "with extended penalization")
autoplot(barley_non_neg_stack, type = "weights") +
  labs(title = "with extended penalization")

set.seed(5547)
barley_non_neg_stack <- 
  barley_non_neg_stack %>% 
  fit_members()

barley_non_neg_stack$metrics %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean) %>% 
  slice(1:5)

# ------------------------------------------------------------------------------
# Basic stacked ensemble with linear weights allowed to be negative

set.seed(4085)
barley_poss_neg_stack <- 
  barley_data_stack %>% 
  blend_predictions(non_negative = FALSE)

autoplot(barley_poss_neg_stack)

# Adjust the penalty a bit
set.seed(4085)
barley_poss_neg_stack <- 
  barley_data_stack %>% 
  blend_predictions(non_negative = FALSE, penalty = 10^seq(-5, 0, length.out = 20))

autoplot(barley_poss_neg_stack) + 
  labs(title = "with extended penalization, allowed negative weights")
autoplot(barley_poss_neg_stack, type = "weights") + 
  labs(title = "with extended penalization, allowed negative weights")

set.seed(5547)
barley_poss_neg_stack <- 
  barley_poss_neg_stack %>% 
  fit_members()

barley_poss_neg_stack$metrics %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean) %>% 
  slice(1:5)

# ------------------------------------------------------------------------------
# Evaluate testing data

predict(barley_poss_neg_stack, barley_test) %>% 
  bind_cols(barley_test) %>% 
  rmse(barley, .pred)

# Single best fit has
# A tibble: 1 × 3
#>     .metric .estimator .estimate
#>     <chr>   <chr>          <dbl>
#>   1 rmse    standard        2.45


# ------------------------------------------------------------------------------

sessioninfo::session_info()

q(save = "no")

