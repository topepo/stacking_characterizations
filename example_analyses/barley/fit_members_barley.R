source("helpers.R")

barley_set <- read_as_workflow_set(file.path("example_analyses", "barley", "candidate_fits"))

load(file.path("example_analyses", "barley", "barley_data.RData"))

library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(stacks)
library(butcher)

# generate a minimal, unpenalized model stack in order to
# generate each model specification
barley_data_stack <- 
  stacks() %>%
  add_candidates(barley_set)

set.seed(1)
barley_model_stack <-
  barley_data_stack %>%
  blend_predictions(times = 3, meta_learner = linear_reg()) %>%
  fit_members()

# dbarts::bart doesn't have a butcher method, and this element
# specifically accounts for most of its (large) object size
rm_yhat_train <- function(x) {
  if (inherits(x$fit$fit$fit, "bart")) {
    x$fit$fit$fit$yhat.train <- NULL
  }
  
  x
}

barley_member_fits <- barley_model_stack$member_fits
barley_member_fits <- map(barley_member_fits, butcher)
barley_member_fits <- map(barley_member_fits, rm_yhat_train)
barley_member_fits_names <- names(barley_member_fits)

if (!dir.exists(file.path("example_analyses", "barley", "member_fits"))) {
  dir.create(file.path("example_analyses", "barley", "member_fits"))
}

for (i in seq_along(barley_member_fits)) {
  obj_nm <- barley_member_fits_names[i]
  obj <- barley_member_fits[[i]]
  file_nm <- file.path("example_analyses", "barley", "member_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = barley_member_fits[[i]])
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}
