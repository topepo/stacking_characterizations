# given a .RData filepath, load the object at that path and return as object
get_object <- function(x) {
  load(x)
  get(gsub("\\.RData", "", basename(x)))
}

# given a directory, read each workflow set row in the folder, row-bind together
read_as_workflow_set <- function(dir) {
  wflow_files <- list.files(dir, full.names = TRUE)
  
  wflow_rows <- lapply(wflow_files, get_object)

  dplyr::bind_rows(wflow_rows)
}


# given an unfitted model stack and the name of a dataset identifier,
# add the pre-fitted members to the model stack
add_members <- function(model_stack, dataset) {
  fits_path <- file.path("example_analyses", dataset, "member_fits")
  
  if (inherits(model_stack, "linear_stack")) {
    needed_members <- 
      stacks:::.get_glmn_coefs(
        model_stack[["coefs"]][["fit"]], 
        model_stack[["coefs"]][["spec"]][["args"]][["penalty"]]
      ) %>%
      dplyr::filter(estimate != 0 & terms != "(Intercept)") %>%
      dplyr::pull(terms)
  } else {
    needed_members <- 
      model_stack[["cols_map"]] %>%
      purrr::flatten_chr() %>%
      unique()
  }

  members_paths <- file.path(fits_path, paste0(needed_members, ".RData"))
  
  model_stack[["member_fits"]] <- lapply(members_paths, get_object)
  names(model_stack[["member_fits"]]) <- needed_members
  
  model_stack
}

# load needed parsnip + recipes extension packages
library(baguette)
library(rules)
library(bonsai)
library(embed)
library(discrim)
