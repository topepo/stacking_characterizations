make_spec <- function() {
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
    set_mode("regression")
}