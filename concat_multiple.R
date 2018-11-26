# author: Tom Greenwood

library(tidyverse)
library(lubridate)

temps <- tibble(site = c("A","B","C") %>% rep(.,5) %>% sort,
	              Date = seq(from = date(dmy("06-06-2018")),to =  date(dmy("10-06-2018")), by = 1) %>% rep(.,3),
	              temp = runif(15, 15, 35)
	)

site_info <- tibble(site = c("A", "B", "B", "C"),
	                  category = c("school", "office", "shop", "field"))



a = left_join(temps, site_info)  %>% mutate(ab = rep(c("A","B"),10), bc = rep(c("x","r","w","e"),5))

# --------------------------------------------- define function --------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

concat_groups <- function(data, group_var, ...) {

   # extract column names
	group_varq <- enquo(group_var)  # group_by needs speech marks removed
	cat_varsq <- quos(...)
	cat_vars <- cat_varsq %>% map(. %>% as.character %>% .[2] %>% paste0("s")) 
	print(group_varq)
	print(cat_vars)
	print(cat_varsq)
	
	map2(cat_vars, cat_varsq, ~ data %>% group_by(!! group_varq) %>%
		                         summarise(!!.x := !! .y %>% unique %>% str_c(collapse = "/"))
	     ) %>% reduce(full_join)  # join them into one df

}


# ====================================================================================================================

# test it
test = concat_groups(a,Date,category,ab, bc)




