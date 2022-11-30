# given a .RData filepath, load the object at that path and return as object
get_object <- function(x) {
  env <- new.env()
  nm <- load(x, env)[1]
  env[[nm]]
}

# takes a set of tuning results and enframes them as a row of a workflow set
enframe_as_wfs <- function(tuning_res) {
  res <-
    tibble::tibble(
      wflow_id = recipes::rand_id("res"),
      info = NA,
      option = NA,
      result = list(tuning_res)
    )
  
  structure(res, class = c("workflow_set", class(res)))
}

# given a directory, read each workflow set row in the folder, row-bind together
read_as_workflow_set <- function(dir) {
  wflow_files <- list.files(dir, full.names = TRUE)
  
  wflow_rows <- lapply(wflow_files, get_object)

  if (!inherits(wflow_rows[[1]], "workflow_set")) {
    wflow_rows <- lapply(wflow_rows, enframe_as_wfs)
  }
  
  res <- dplyr::bind_rows(wflow_rows)
  
  structure(
    res,
    class = class(wflow_rows[[1]])
  )
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
