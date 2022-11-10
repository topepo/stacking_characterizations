source("helpers.R")

wind_set <- read_as_workflow_set(file.path("example_analyses", "wind", "candidate_fits"))

load(file.path("example_analyses", "wind", "wind_data.RData"))

library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(stacks)
library(butcher)

# generate a minimal, unpenalized model stack in order to
# generate each model specification
wind_data_stack <- 
  stacks() %>%
  add_candidates(wind_set)

set.seed(1)
wind_model_stack <-
  wind_data_stack %>%
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

wind_member_fits <- wind_model_stack$member_fits
wind_member_fits <- map(wind_member_fits, butcher)
wind_member_fits <- map(wind_member_fits, rm_yhat_train)
wind_member_fits_names <- names(wind_member_fits)

if (!dir.exists(file.path("example_analyses", "wind", "member_fits"))) {
  dir.create(file.path("example_analyses", "wind", "member_fits"))
}

for (i in seq_along(wind_member_fits)) {
  obj_nm <- wind_member_fits_names[i]
  obj <- wind_member_fits[[i]]
  file_nm <- file.path("example_analyses", "wind", "member_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = wind_member_fits[[i]])
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}
