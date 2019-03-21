library(entropy)
library(igraph)
library(optrees)
library(tidyverse)
library(ggplot2)

read_csv("Chu_Liu/GB-cltree.csv") %>% 
  dplyr::select(-X1) %>% 
  as.matrix()-> GB_weigthed_gower_features

gb_dependency_graph <- graph_from_edgelist(GB_weigthed_gower_features[,1:2])

E(gb_dependency_graph)$weight <- GB_weigthed_gower_features[,3]

png("Chu_Liu/GB_cltree_plot_tree.png", width = 1200, height = 1200, units = "px", pointsize = 10) 

plot.igraph(gb_dependency_graph, 
            vertex.label.color= "black",
            vertex.label.cex = 2, 
            vertex.size = 1, 
            layout = layout_as_tree(gb_dependency_graph, circular = F),             
            edge.width=2,
            edge.arrow.size=1, 
            asp = 1) -> GB_plot

dev.off()



