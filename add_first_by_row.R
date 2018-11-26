library(tidyverse)
library(rlang)

# df = tibble(a = c("b", "d", "l", "m"), x = c(NA,NA, 1, 3), y = c(2,NA, 2, 3), z = c(1,2, 0, NA))
# 
# add_first <- function(x, y, z, ...) { c(x, y, z) %>% {.[!is.na(.)]} %>% .[1] } 
# 
# df %>% mutate(new_col = pmap_dbl(df, add_first))
# # # A tibble: 4 x 5
# #   a         x     y     z new_col
# #   <chr> <dbl> <dbl> <dbl>   <dbl>
# # 1 b        NA     2     1       2
# # 2 d        NA    NA     2       2
# # 3 l         1     2     0       1
# # 4 m         3     3    NA       3
# 
# 
# add_first_maker <- function(cols) {
# 	   cols <- cols %>% as_list %>% list_modify(., "..." = 0)
# 	   
# 	 	 new_function(cols, 
# 	 	 	           expr(c(x, y, z) %>% {.[!is.na(.)]} %>% .[1] )
# 	 	             )
# }
# 
# vars <- c(0,0,0) %>% set_names("x", "y", "z")
# 
# add_first1 <- add_first_maker(vars)
# 
# df %>% mutate(new_col = pmap_dbl(df, add_first1))
# # # A tibble: 4 x 5
# #   a         x     y     z new_col
# #   <chr> <dbl> <dbl> <dbl>   <dbl>
# # 1 b        NA     2     1       2
# # 2 d        NA    NA     2       2
# # 3 l         1     2     0       1
# # 4 m         3     3    NA       3


# df is the data frame you want to add the column to,
# cols is a character vector of column names which you want to scan
# new_name is the name of the new column you want to add
# predicate is the predicate function you want to apply to the columns. This function can be in the 
# purrr lambda style e.g ~ !is.na(.) or ~ . > 5


add_first_by_row <- function(df, cols, new_name, predicate) {
	   cond <- as_mapper(predicate)
	   condq <- enexpr(cond)
	   new_name <- enexpr(new_name) %>% as.character 
	   arg_names <- c(cols, "...") 
	   col_syms <- cols %>% syms
	   
	   args <- rep(0, times = length(cols)+1) %>% set_names(arg_names)
	 	 new_fun <- new_function(args, 
	 	 	           expr(c(!!!col_syms) %>% {.[(!!condq)(.)]} %>% .[1] )
	 	             )
	 	 
	 #	 return(new_fun)
	 	 df %>% mutate(!!new_name := pmap_dbl(df,new_fun))
}

# n.b. the below is for testing
# 
# vars <- c("x", "y")
# 
# 
# 
# df %>% add_first(vars, new_column, is.na)
# 
# vars1 <- c("x", "y", "z")
# 
# df %>% add_first(vars1, new_column, ~ !is.na(.))
# df %>% add_first(vars1, new_column, ~ . > 1)
# 
# df %>% add_first(vars1, new_column, function(x) {x %in% c(2,3)})
# 
# df %>% add_first(vars1, new_column, function(x) {x < 2 & !is.na(x) })
#  
