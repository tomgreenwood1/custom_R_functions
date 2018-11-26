library(tidyverse)
library(rlang)

# this function filters takes a data.frame and produces a list, each element 
# of which is one of all possible way you can filter the that data.frame on for two values of the column
# given by the var argument. This is useful in the case where you want to a statistical comparison
# for each pair of populations (and perhaps, find the closest match or something similar). 

# for a chi.sq you'd want to follow it with map(select()) %>% map(table), 
# for ks-test it'd be a map(split(.$)) %>% map(select())

all_filter_combs <- function(df, var) {
	varq <- enquo(var)
	values <-  expr(df$`!!`(get_expr(varq))) %>% eval %>% unique %>% 
		          combn(2, simplify = FALSE) %>%
		          transpose %>% map(function(x) x %>% set_names(., nm = x))
	
	names <- values %>% pmap(~ paste(.x, .y, sep = "-"))
		
	arg_fun <- function(a, b) {  quo(!!varq == !!a | !!varq == !!b) } 
		
	args <- values %>% pmap(arg_fun) %>% set_names(names)
	                         
	filter_fun <-  function(arg) { df %>% filter(!!arg)}
	
	args %>% map(filter_fun)
	
}

# # example usage
# mtcars %>% all_filter_combs(gear) %>% map(. %>% select(gear, carb)) %>% map(table)  
# # example usage
# mtcars %>% all_filter_combs(gear) %>% map(. %>% select(gear, mpg)) %>% map(. %>% split(.$gear)) 
