# author: Tom Greenwood

library(purrr)

your_best_formula <- function(data, target = NULL) {
	
	if (is.null(target)) { 
		target <- names(data)[1]
	}
	
	# create a function to select the best model from the tuning (for tidiness)
  select_best <- function(x) { x$formulae[[which(x$adj_r_squared == x$adj_r_squared %>% max)]] }
	
  all_vars <- data %>% select(-target) %>% names	
	
	map(1:length(all_vars), ~ combn(all_vars, ., simplify = FALSE)) %>% # makes a list of list of combinations
	modify_depth(., ~ glue::collapse(.,sep = "+"), .depth = 2) %>%  # turns the combinations vectors into strings
	unlist %>%                                                      # makes a vector from the list of strings
	map_chr(., ~ paste0(target_var,"~",.)) %>%                      # adds the target and tilde onto the vars to make
	as_tibble %>%                                                   # fomula-like strings
	rename(formulae = value) %>%                                 # re-naming for clarity of next code
	mutate(models = map(formulae, ~ lm(as.formula(.), data = data))) %>%  # build models off all formulae
	mutate(adj_r_squared = map_dbl(models, . %>% glance %>% .$adj.r.squared)) %>% # get the adj r2 for each model
	select_best                                                         # pick the best one
	
	
}
