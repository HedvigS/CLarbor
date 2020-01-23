source("package_dependencies.R")

feature_abbrevs <- read_tsv("data/tidied/desc.tsv") %>% 
    dplyr::select(Feature_ID, Abbreviation) %>% 
  column_to_rownames("Feature_ID") %>%
  as.matrix() %>% 
  drop() 

feature_groups <- read_tsv("data/tidied/desc.tsv") %>% 
  .$group %>% 
  factor()

feature_palette <- distinctColorPalette(length(feature_groups))
feature_colors <- feature_palette[feature_groups]
names(feature_colors) <- feature_abbrevs

##Uncontrolled

graph_df_unpruned <- read_tsv("output/CLarbors/best_CLarbor_uncontrolled.tsv") %>% 
  transmute(from = feature_abbrevs[`Source feature`], to = feature_abbrevs[`Target feature`], weight = Dependency)

graph_df_pruned <- graph_df_unpruned %>% 
  arrange(to) %>% 
  distinct(to, .keep_all = T)

message("Note: In case of multiple incoming edges, we pick just the first one.")

dependency_graph_unpruned <- graph.data.frame(graph_df_unpruned) %>% 
  igraph::simplify()
dependency_graph_pruned <- graph.data.frame(graph_df_pruned) %>% 
  igraph::simplify()

pdf("output/Vizualisations/CLarbor_plots/best_CLarbor_uncontrolled_graph_unpruned.pdf", width = 20, height = 20) 

plot.igraph(dependency_graph_unpruned, 
            vertex.label.color= feature_colors[names(V(dependency_graph_unpruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            # layout = -layout_as_tree(dependency_graph_unpruned, circular = F)[,2:1],             
            edge.width=E(dependency_graph_unpruned)$weight^2,
            edge.arrow.size=0.1)

dev.off()

pdf("output/Vizualisations/CLarbor_plots/best_CLarbor_uncontrolled_graph_pruned.pdf", width = 20, height = 20) 

plot.igraph(dependency_graph_pruned, 
            vertex.label.color= feature_colors[names(V(dependency_graph_pruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            layout = -layout_as_tree(dependency_graph_pruned, circular = F)[,2:1],
            edge.width=E(dependency_graph_pruned)$weight^2,
            edge.arrow.size=0.1, 
            asp = 1)

dev.off()

##Controlled

graph_df_unpruned <- read_tsv("output/CLarbors/best_CLarbor_controlled.tsv") %>% 
  transmute(from = feature_abbrevs[`Source feature`], to = feature_abbrevs[`Target feature`], weight = Dependency)

graph_df_pruned <- graph_df_unpruned %>% 
  group_by(to) %>% 
  summarise(from = first(from), weight = first(weight)) %>% 
  dplyr::select(from, to, weight)

message("Note: In case of multiple incoming edges, we pick just the first one.")

dependency_graph_unpruned <- graph.data.frame(graph_df_unpruned) %>% 
  igraph::simplify()
dependency_graph_pruned <- graph.data.frame(graph_df_pruned) %>% 
  igraph::simplify()

pdf("output/Vizualisations/CLarbor_plots/best_CLarbor_controlled_graph_unpruned.pdf", width = 20, height = 20) 

plot.igraph(dependency_graph_unpruned, 
            # vertex.label.color= "black",
            vertex.label.color= feature_colors[names(V(dependency_graph_unpruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            # layout = -layout_as_tree(dependency_graph_unpruned, circular = F)[,2:1],
            
            edge.width=E(dependency_graph_unpruned)$weight^2,
            edge.arrow.size=0.1, 
            asp = 1)

dev.off()

pdf("output/Vizualisations/CLarbor_plots/best_CLarbor_controlled_graph_pruned.pdf", width = 20, height = 20) 

plot.igraph(dependency_graph_pruned, 
            vertex.label.color= feature_colors[names(V(dependency_graph_pruned))],
            vertex.label.cex = 0.9, 
            vertex.size = 0, 
            vertex.color = "white",
            vertex.frame.color = "white",
            layout = -layout_as_tree(dependency_graph_pruned, circular = F)[,2:1],             
            edge.width=E(dependency_graph_pruned)$weight^2,
            edge.arrow.size=0.1, 
            asp = 1)

dev.off()

