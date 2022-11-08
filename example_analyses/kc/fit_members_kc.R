source("helpers.R")

kc_set <- read_as_workflow_set(file.path("example_analyses", "kc", "candidate_fits"))

load(file.path("example_analyses", "kc", "kc_data.RData"))

library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(stacks)
library(butcher)

# generate a minimal, unpenalized model stack in order to
# generate each model specification
kc_data_stack <- 
  stacks() %>%
  add_candidates(kc_set)

set.seed(1)
kc_model_stack <-
  kc_data_stack %>%
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

kc_member_fits <- kc_model_stack$member_fits
kc_member_fits <- map(kc_member_fits, butcher)
kc_member_fits <- map(kc_member_fits, rm_yhat_train)
kc_member_fits_names <- names(kc_member_fits)

for (i in seq_along(kc_member_fits)) {
  obj_nm <- kc_member_fits_names[i]
  obj <- kc_member_fits[[i]]
  file_nm <- file.path("example_analyses", "kc", "member_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = kc_member_fits[[i]])
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}
