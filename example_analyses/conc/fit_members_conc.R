source("helpers.R")

conc_set <- read_as_workflow_set(file.path("example_analyses", "conc", "candidate_fits"))

load(file.path("example_analyses", "conc", "conc_data.RData"))

library(tidymodels)
library(bonsai)
library(rules)
library(embed)
library(stacks)
library(butcher)

# generate a minimal, unpenalized model stack in order to
# generate each model specification
conc_data_stack <- 
  stacks() %>%
  add_candidates(conc_set)

set.seed(1)
conc_model_stack <-
  conc_data_stack %>%
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

conc_member_fits <- conc_model_stack$member_fits
conc_member_fits <- map(conc_member_fits, butcher)
conc_member_fits <- map(conc_member_fits, rm_yhat_train)
conc_member_fits_names <- names(conc_member_fits)

if (!dir.exists(file.path("example_analyses", "conc", "member_fits"))) {
  dir.create(file.path("example_analyses", "conc", "member_fits"))
}

for (i in seq_along(conc_member_fits)) {
  obj_nm <- conc_member_fits_names[i]
  obj <- conc_member_fits[[i]]
  file_nm <- file.path("example_analyses", "conc", "member_fits", paste0(obj_nm, ".RData"))
  assign(obj_nm, value = conc_member_fits[[i]])
  save(list = obj_nm, file = file_nm, compress = "xz", compression_level = 9)
}
