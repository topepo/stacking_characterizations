make_spec <- function() {
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(),
             learn_rate = tune(), stop_iter = 10) %>%
    set_engine("xgboost") %>%
    set_mode("regression")
}