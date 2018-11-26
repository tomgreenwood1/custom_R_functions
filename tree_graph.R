# author: Tom Greenwood

library(ggraph)
library(igraph)
library(tidyverse)

tree_graph <- function(model, tree_num) {

tree <- getTree(model,                      # getTree is a randomForrest function for extracting a data.frame
                k = tree_num,                  # of nodes and paths, but it needs some altering before it's ready
                labelVar = TRUE) %>%            # to be parsed by igraph::graph_from_data_frame
                rownames_to_column () %>%           # need the rownames for since they actuall represent the nodes
                mutate(`split point` = ifelse(is.na(prediction),  `split point`, NA)) # if we leave 0's in it will
                                                                                      # add erroneous labels
graph_frame <- data.frame(
                    from = as.numeric(rep(tree$rowname , 2)),          # we need each connect to have it's own row
                    to   = c(tree$`left daughter`, tree$`right daughter`), # which means splitting out left and right
                    type = c(rep("<=", nrow(tree)), rep(">=", nrow(tree)))) %>% # make the decision on split explicit
                    mutate(type = ifelse(to == 0, NA, as.character(type))) %>%  
                    arrange(from) # arrange it so that edge labelling will be correct
           
            
graph <-  graph_frame %>% 
             graph_from_data_frame() %>%       # build a graph from the data frame (this is an igraph function)
             delete_vertices("0")       # the 0 vertices are redudant since the map 1-to-1 onto the verticies above
         
  
  V(graph)$node_names <- as.character(tree$`split var`)           # name the nodes
  V(graph)$node_split_values <- as.character(signif(tree$`split point`, digits = 3)) # show their split values
  V(graph)$leaf_labels <- tree$prediction  # leafs are predictions
  E(graph)$type <- as.character(E(graph)$type)  # assign edges with labels for the corresponding decision 

  
ggraph(graph,"dendrogram") +             # for details see ?ggraph
         geom_edge_link() +       
         geom_node_point() +
         geom_edge_link(aes(label = E(graph)$type), angle_calc = "along", label_dodge = unit(2.5, "mm")) +
         geom_node_text(aes(label = node_names), na.rm = TRUE, repel = TRUE) +
         geom_node_label(aes(label = node_split_values), vjust = 2.5, na.rm = TRUE, fill = "#FF9300") +
         geom_node_label(aes(label = leaf_labels, fill = leaf_labels), na.rm = TRUE, 
					                repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
         theme_artesia() +
	     theme(panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         panel.background = element_blank(),
         plot.background = element_rect(fill = "white"),
         panel.border = element_blank(),
         axis.line = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         plot.title = element_text(size = 14)) +
         scale_fill_artesia()
             
}
