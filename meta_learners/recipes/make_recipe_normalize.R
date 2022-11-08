make_recipe <- function(formula, data) {
  recipe(formula, data = data) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
}