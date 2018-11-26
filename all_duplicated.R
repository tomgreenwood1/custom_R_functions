# author: Tom Greenwood

library(tidyverse)
library(rlang)

test_df <- tibble(length = c(1,1,2,3,4,4), height = c(2,2,5,6,1,3), depth = c(3,3,5,4,2,2))

all_duplicated <- function (vector) { elements <- 1:length(vector)
                                     elements %>% map_lgl( ~ vector[.] %in% vector[-.])        }

filter_all_duplicated_v0 <- function(df, var) {
          varq <- enquo(var)
         df %>% { filter(.,all_duplicated(.$`!!`(get_expr(varq)))) } 
}

remove_all_duplicated_v0 <- function(df, var) {
          varq <- enquo(var)
         df %>% { filter(.,!all_duplicated(.$`!!`(get_expr(varq)))) } 
}

#testing 
test_df %>% remove_all_duplicated_v0(length)

test_df %>% filter_all_duplicated_v0(length)

filter_all_duplicated <- function(df, ...) {
          varsq <- enquos(...)
          
          make_arg <- function(x) { quo(all_duplicated(df$`!!`(get_expr(x)))) }
          
          args <- varsq %>% map(make_arg)
            
          df %>% { filter(., !!!args) } 
}

test_df %>% filter_all_duplicated(length)
# # A tibble: 4 x 3
#   length height depth
#    <dbl>  <dbl> <dbl>
# 1      1      2     3
# 2      1      2     3
# 3      4      1     2
# 4      4      3     2

test_df %>% filter_all_duplicated(length, height, depth)
# # A tibble: 2 x 3
#   length height depth
#    <dbl>  <dbl> <dbl>
# 1      1      2     3
# 2      1      2     3

remove_all_duplicated <- function(df, ...) {
          varsq <- enquos(...)
          
          make_arg <- function(x) { quo(all_duplicated(df$`!!`(get_expr(x)))) }
          
          args <- varsq %>% map(make_arg)
            
          df_minus <- df %>% { filter(., !!!args) } 
          
          dplyr::setdiff(df,df_minus)
}

test_df %>% remove_all_duplicated(length)
# # A tibble: 2 x 3
#   length height depth
#    <dbl>  <dbl> <dbl>
# 1      2      5     5
# 2      3      6     4

test_df %>% remove_all_duplicated(length, height, depth)
# # A tibble: 4 x 3
#   length height depth
#    <dbl>  <dbl> <dbl>
# 1      2      5     5
# 2      3      6     4
# 3      4      1     2
# 4      4      3     2
