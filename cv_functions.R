library(ModelMetrics)
library(broom)
library(tidyverse)

get_cv_stat <- function(models_list, stat) {
  statq <- enquo(stat)
  models_list %>% map_dbl(. %>% glance %>% 
                                select(!!statq) %>% 
                                as.numeric) %>% 
                  mean
}

get_cv_estimate <- function(models_list, term) {
  terme <- enexpr(term)
  termch <- as.character(terme)
  
  models_list %>% map_dbl(. %>% tidy %>% 
                                filter(term == !!termch) %>% 
                                select(estimate) %>%
                                as.numeric) %>% 
                  mean
}


get_cv_metric <- function(results_list, metric) {
  metrice <- enexpr(metric) 
  fn_expr <- expr(~ (!!metrice)(.$prediction , .$true_value))
  
  full_expr <- expr(results_list %>% map_dbl( !!fn_expr) %>%
                   mean)
  eval(full_expr)
}