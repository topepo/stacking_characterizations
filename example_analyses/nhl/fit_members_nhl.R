source("helpers.R")

nhl_set <- read_as_workflow_set(file.path("example_analyses", "nhl", "candidate_fits"))

load(file.path("example_analyses", "nhl", "nhl_data.RData"))

library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(stacks)
library(butcher)

# generate a minimal, unpenalized model stack in order to
# generate each model specification
nhl_data_stack <- 
  stacks() %>%
  add_candidates(nhl_set)

set.seed(1)
nhl_model_stack <-
  nhl_data_stack %>%
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

nhl_member_fits <- nhl_model_stack$member_fits
nhl_member_fits <- map(nhl_member_fits, butcher)
nhl_member_fits <- map(nhl_member_fits, rm_yhat_train)
nhl_member_fits_names <- names(nhl_member_fits)

if (!dir.exists(file.path("example_analyses", "nhl", "member_fits"))) {
  dir.create(file.path("example_analyses", "nhl", "member_fits"))
}

for (i in seq_along(nhl_member_fits)) {
  obj_nm <- nhl_member_fits_names[i]
  obj <- nhl_member_fits[[i]]
  file_nm <- file.path("example_analyses", "nhl", "member_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = nhl_member_fits[[i]])
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}
