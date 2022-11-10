library(baguette)

make_spec <- function() {
  bag_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>%
    set_engine("rpart") %>%
    set_mode("regression")
}