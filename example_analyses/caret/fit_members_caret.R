source("helpers.R")

caret_set <- read_as_workflow_set(file.path("example_analyses", "caret", "candidate_fits"))

load(file.path("example_analyses", "caret", "caret_data.RData"))

library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(stacks)
library(butcher)

# generate a minimal, unpenalized model stack in order to
# generate each model specification
caret_data_stack <- 
  stacks() %>%
  add_candidates(caret_set)

set.seed(1)
caret_model_stack <-
  caret_data_stack %>%
  blend_predictions(times = 3, meta_learner = logistic_reg()) %>%
  fit_members()

# dbarts::bart doesn't have a butcher method, and this element
# specifically accounts for most of its (large) object size
rm_yhat_train <- function(x) {
  if (inherits(x$fit$fit$fit, "bart")) {
    x$fit$fit$fit$yhat.train <- NULL
  }
  
  x
}

caret_member_fits <- caret_model_stack$member_fits
caret_member_fits <- map(caret_member_fits, butcher)
caret_member_fits <- map(caret_member_fits, rm_yhat_train)
caret_member_fits_names <- names(caret_member_fits)

if (!dir.exists(file.path("example_analyses", "caret", "member_fits"))) {
  dir.create(file.path("example_analyses", "caret", "member_fits"))
}

for (i in seq_along(caret_member_fits)) {
  obj_nm <- caret_member_fits_names[i]
  obj <- caret_member_fits[[i]]
  file_nm <- file.path("example_analyses", "caret", "member_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = caret_member_fits[[i]])
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}
