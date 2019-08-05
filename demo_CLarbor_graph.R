library(entropy)
library(igraph)
library(optrees)
library(tidyverse)
library(ggplot2)
library(randomcoloR)

Sahul_feature_abbrevs <- read_tsv("demo_data/Sahul_ID_desc.tsv") %>% 
  mutate(Feature_ID = paste0("SAHUL", str_pad(Feature_ID, 3, "left", "0"))) %>% 
  dplyr::select(-Feature, -binary_or_multi, -group) %>% 
  column_to_rownames("Feature_ID") %>%
  as.matrix() %>% 
  drop() 

Sahul_feature_groups <- read_tsv("demo_data/Sahul_ID_desc.tsv") %>% 
  .$group %>% 
  factor()

feature_palette <- distinctColorPalette(length(Sahul_feature_groups))
Sahul_feature_colors <- feature_palette[Sahul_feature_groups]
names(Sahul_feature_colors) <- Sahul_feature_abbrevs

##Controlled

Sahul_graph_df_unpruned <- read_tsv("output/CLarbors/Sahul_best_CLarbor_controlled.tsv") %>% 
  transmute(from = Sahul_feature_abbrevs[`Source feature`], to = Sahul_feature_abbrevs[`Target feature`], weight = Dependency)

Sahul_graph_df_pruned <- Sahul_graph_df_unpruned %>% 
  group_by(to) %>% 
  summarise(from = first(from), weight = first(weight)) %>% 
  dplyr::select(from, to, weight)

message("Note: In case of multiple incoming edges, we pick just the first one.")

Sahul_dependency_graph_unpruned <- graph.data.frame(Sahul_graph_df_unpruned) %>% 
  igraph::simplify()
Sahul_dependency_graph_pruned <- graph.data.frame(Sahul_graph_df_pruned) %>% 
  igraph::simplify()

pdf("output/Vizualisations/CLarbor_plots/Sahul_best_CLarbor_controlled_graph_unpruned.pdf", width = 20, height = 20) 

plot.igraph(Sahul_dependency_graph_unpruned, 
            # vertex.label.color= "black",
            vertex.label.color= Sahul_feature_colors[names(V(Sahul_dependency_graph_unpruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            # layout = -layout_as_tree(Sahul_dependency_graph_unpruned, circular = F)[,2:1],

                        edge.width=E(Sahul_dependency_graph_unpruned)$weight^2,
            edge.arrow.size=0.1, 
            asp = 1)

dev.off()

pdf("output/Vizualisations/CLarbor_plots/Sahul_best_CLarbor_controlled_graph_pruned.pdf", width = 20, height = 20) 

plot.igraph(Sahul_dependency_graph_pruned, 
            vertex.label.color= Sahul_feature_colors[names(V(Sahul_dependency_graph_pruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            layout = -layout_as_tree(Sahul_dependency_graph_pruned, circular = F)[,2:1],             
            edge.width=E(Sahul_dependency_graph_pruned)$weight^2,
            edge.arrow.size=0.1, 
            asp = 1)

dev.off()

##Uncontrolled

Sahul_graph_df_unpruned <- read_tsv("output/CLarbors/Sahul_best_CLarbor_uncontrolled.tsv") %>% 
  transmute(from = Sahul_feature_abbrevs[`Source feature`], to = Sahul_feature_abbrevs[`Target feature`], weight = Dependency)

Sahul_graph_df_pruned <- Sahul_graph_df_unpruned %>% 
  arrange(to) %>% 
  distinct(to, .keep_all = T)

message("Note: In case of multiple incoming edges, we pick just the first one.")

Sahul_dependency_graph_unpruned <- graph.data.frame(Sahul_graph_df_unpruned) %>% 
  igraph::simplify()
Sahul_dependency_graph_pruned <- graph.data.frame(Sahul_graph_df_pruned) %>% 
  igraph::simplify()

pdf("output/Vizualisations/CLarbor_plots/Sahul_best_CLarbor_uncontrolled_graph_unpruned.pdf", width = 20, height = 20) 

plot.igraph(Sahul_dependency_graph_unpruned, 
            vertex.label.color= Sahul_feature_colors[names(V(Sahul_dependency_graph_unpruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            # layout = -layout_as_tree(Sahul_dependency_graph_unpruned, circular = F)[,2:1],             
            edge.width=E(Sahul_dependency_graph_unpruned)$weight^2,
            edge.arrow.size=0.1)

dev.off()

pdf("output/Vizualisations/CLarbor_plots/Sahul_best_CLarbor_uncontrolled_graph_pruned.pdf", width = 20, height = 20) 

plot.igraph(Sahul_dependency_graph_pruned, 
            vertex.label.color= Sahul_feature_colors[names(V(Sahul_dependency_graph_pruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            layout = -layout_as_tree(Sahul_dependency_graph_pruned, circular = F)[,2:1],
            edge.width=E(Sahul_dependency_graph_pruned)$weight^2,
            edge.arrow.size=0.1, 
            asp = 1)

dev.off()

