# author: Tom Greenwood

library(RcppRoll)
library(rlang)

rollfuns <- function(var, fun, shortest = 5, longest = 10, weight_slope = 0 ) {
	
	var<- enquo(var)
	funq <- enquo(fun)
	
	indices <- seq(from = shortest, to = longest)
	map(indices, ~    quo(fun(!!var, .x, 
		                        weights = rep(1,.x) %>% 
			                       { .*(seq(1,1+weight_slope, length.out = .x))} %>%
			                       {./sum(.)*.x}
		                        )
		                   ) 
		 ) %>%
		 set_names(sprintf("%s_%s_%02d",  quo_name(funq), quo_text(var), indices))
}



