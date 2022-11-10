library(baguette)

make_spec <- function() {
  bag_mars(num_terms = tune(), prod_degree = tune()) %>%
    set_engine("earth") %>% 
    set_mode("regression")
}