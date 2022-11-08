make_recipe <- function(formula, data) {
  recipe(formula, data = data) %>%
    step_zv(all_predictors())
}