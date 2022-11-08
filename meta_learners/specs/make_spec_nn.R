make_spec <- function() {
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
    set_engine("nnet") %>%
    set_mode("regression")
}
