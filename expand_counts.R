library(tidyverse)
# author: Tom Greenwood

library(rlang)

# set.seed(121)
# test_counts <- tibble(Population = letters[1:4], Length = c(1,1,2,1), Number = sample(1:100, 4))
# test_counts1 <- tibble(Population = letters[1:4],
#                       Length = c(1,1,2,1),
#                       Height = c(2,3,3,5),
#                       Width = c(3,5,6,7),
# 	                    Number = sample(1:100, 4))

expand_counts <- function(df, count = NULL) { 
	  countq <- enexpr(count)
	  names <- df %>% select(-!!countq) %>% names 
	  namesq <- names %>% map(as.name)
	  
	  cols <- map(namesq, ~ expr(rep(!!., times = !!countq))
	  	          ) %>% set_names(namesq)
	
	  all_names <- df %>% names %>% map(as.name) 
    args <- rep(0, times = length(all_names)) %>% as.list %>% set_names(all_names)
    
    correct_function <- new_function(args,     # this makes the function as 
       	                             expr(tibble(!!!cols))  )
    pmap_dfr(df, correct_function)     # applies it 
}

# test_counts %>% expand_counts(count = Number) %>% 
# 	              group_by(Population, Length) %>%
# 	              summarise(Number = n()) %>%
# 	              ungroup %>%
# 	              { all.equal(., test_counts)}
# 	
# 
# test_counts1 %>% expand_counts(count = Number) %>% 
# 	              group_by(Population, Length, Height, Width) %>%
# 	              summarise(Number = n()) %>%
# 	              ungroup %>%
# 	              { all.equal(., test_counts1)}



