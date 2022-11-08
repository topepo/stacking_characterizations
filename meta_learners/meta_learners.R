# setup ------------------------------------------------------------------------
library(tidymodels)

ex_dat <- tibble(outcome = rnorm(5), predictor_1 = rnorm(5), predictor_2 = rnorm(5))


# model specifications ---------------------------------------------------------
# this will just be set as the current default meta-learner with no recipe
spec_glmnet <- linear_reg()

spec_bt <-
  bag_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

spec_bm <-
  bag_mars(num_terms = tune(), prod_degree = tune()) %>%
  set_engine("earth") %>% 
  set_mode("regression")

spec_svm <- 
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_mode("regression")

spec_nn <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("regression")

spec_xgb <-
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(),
             learn_rate = tune(), stop_iter = 10) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


meta_learners <-
  workflow_set(
    preproc = list(basic = rec_basic),
    models = list(linear_reg = spec_glmnet, boost_tree = spec_xgb)
  ) %>%
  bind_rows(
    workflow_set(
      preproc = list(normalize = rec_normalize),
      models = list(
        bag_tree = spec_bt,
        bag_mars = spec_bm,
        svm_rbf = spec_svm,
        mlp = spec_nn
      )
    )
  )  %>%
  bind_rows(
    workflow_set(
      preproc = list(pca = rec_pca),
      models = list(
        bag_tree = spec_bt,
        bag_mars = spec_bm,
        svm_rbf = spec_svm,
        mlp = spec_nn
      )
    )
  ) %>%
  bind_rows(
    workflow_set(
      preproc = list(renormalize = rec_renormalize),
      models = list(bag_tree = spec_bt, bag_mars = spec_bm)
    )
  )
