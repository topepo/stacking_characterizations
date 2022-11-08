make_recipe <- function(formula, data) {
  recipe(formula, data = data) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_pca(all_predictors(), threshold = 1) %>%
    step_normalize(all_numeric_predictors())
}