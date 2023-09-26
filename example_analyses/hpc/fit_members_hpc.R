source("helpers.R")

hpc_set <- read_as_workflow_set(file.path("example_analyses", "hpc", "candidate_fits"))

load(file.path("example_analyses", "hpc", "hpc_data.RData"))

library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(stacks)
library(butcher)

# generate a minimal, unpenalized model stack in order to
# generate each model specification
hpc_data_stack <- 
  stacks() %>%
  add_candidates(hpc_set)

meta_learner <- multinom_reg() %>% set_engine("nnet", MaxNWts = 10000)

set.seed(1)
hpc_model_stack <-
  hpc_data_stack %>%
  blend_predictions(times = 3, meta_learner = meta_learner) %>%
  fit_members()

# dbarts::bart doesn't have a butcher method, and this element
# specifically accounts for most of its (large) object size
rm_yhat_train <- function(x) {
  if (inherits(x$fit$fit$fit, "bart")) {
    x$fit$fit$fit$yhat.train <- NULL
  }
  
  x
}

hpc_member_fits <- hpc_model_stack$member_fits
hpc_member_fits <- map(hpc_member_fits, butcher)
hpc_member_fits <- map(hpc_member_fits, rm_yhat_train)
hpc_member_fits_names <- names(hpc_member_fits)

if (!dir.exists(file.path("example_analyses", "hpc", "member_fits"))) {
  dir.create(file.path("example_analyses", "hpc", "member_fits"))
}

for (i in seq_along(hpc_member_fits)) {
  obj_nm <- hpc_member_fits_names[i]
  obj <- hpc_member_fits[[i]]
  file_nm <- file.path("example_analyses", "hpc", "member_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = hpc_member_fits[[i]])
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}
