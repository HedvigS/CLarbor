value_df <- read_tsv("Sahul_structure_wide.tsv")

sahul_IDS <- read_tsv("Sahul_structure_wide.tsv") %>%
  dplyr::select(Glottocode)

random_effects_df <- read_tsv("Glottolog_all_languoids_Heti_enhanced.tsv") %>%
  dplyr::select(Glottocode,
                Family_name,
                AUTOTYP_area) %>%
  right_join(sahul_IDS)

dependency_matrix_cond_MI <- function(value_df, random_effects_df){
  library(tidyverse)
  library(infotheo)
  if (sum(is.na(random_effects_df)) > 0) {
    stop("There is missing data in your dataframe for random effects. Please make sure that all rows have complete entries.")
  }
  
  random_effects_df_united <- random_effects_df %>% 
    unite("random_effects_united", -1)
  
  number_of_duplicates <- sum(duplicated(value_df[,1]))
  if (number_of_duplicates > 0){
    stop("Not all of the IDs are unique! Please recheck.")
  }
  
  if (!all(value_df[,1] == random_effects_df_united[,1])){
    stop("Random effects IDs and value IDs are not the same! Please recheck.")
  }
  
  make_features_factors <- function (df){
    df %>% 
      modify_at(-1, as.factor) %>% 
      rename_at(1, function(x){"rowname"}) %>% 
      column_to_rownames()
  }
  
  value_df_for_depfun <- make_features_factors(value_df)
  random_effects_df_for_depfun <- make_features_factors(random_effects_df_united)

  value_vars <- colnames(value_df_for_depfun)
  dfs_joined <- cbind(value_df_for_depfun, random_effects_df_for_depfun)

  depfun <- function(x,y){
    d <- dfs_joined %>% 
      select(Var1 = x, Var2 = y, random_effects_united) %>% 
      mutate(Var1 = if (x == y) {Var2} else {Var1}) %>% 
      drop_na()
    condinformation(d$Var1, d$Var2, d$random_effects_united)/condentropy(d$Var2, d$random_effects_united)
  }
  
  cat("Computing dependency matrix:\n")
  value_vars_grid <- expand.grid(value_vars, value_vars, stringsAsFactors = F)
  pb <- progress_estimated(nrow(value_vars_grid))
  dependencys_vector <- map2_dbl(value_vars_grid$Var1, value_vars_grid$Var2,
                                 function(x,y){
                                   pb$tick()$print()
                                   depfun(x,y)
                                 })
  dependencys <- matrix(pmax(dependencys_vector,0), nrow = length(value_vars), ncol = length(value_vars), dimnames = list(value_vars, value_vars))
  message(paste("The adjancey matrix for the weighted directed graph, the maximum spanning tree, is now created. There were", sum(is.nan(dependencys)), "cells with missing dependency values."))
  
  diag(dependencys) <- 1
  dependencys[is.nan(dependencys)] <- 0
  
  dependencys
}

#dep_matrix <- dependencys

cltree_from_dep_matrix_known_root <- function(dep_matrix, root){
  library(igraph)
  library(optrees)
  source("chuliu.R")
  
  value_vars <- colnames(dep_matrix)
  
  if(is.character(root)) {
    root <- which(value_vars == root)
  }
  
  G <- 1 - dep_matrix + .Machine$double.eps
  diag(G) <- 0
  dimnames(G) <- NULL
  # dep_graph <- graph_from_adjacency_matrix(G, weighted = T)
  # cltree_optrees <- msArborEdmonds(V(dep_graph), get.edgelist(dep_graph), source.node = root, stages.data = T)
  cltree <- chuliu(G,root)
  V(cltree)$name <- value_vars
  redundant_percentage<- 100*sum(1 - E(cltree)$weight)/length(E(cltree)$weight)
  message(paste("Your dataset contains", round(redundant_percentage,2), "% mass that can be considered as redundant."))
  cltree
}

cltree_from_dep_matrix_known_root(dependencys_sym, 1)

make_cltree_df <- function(weighted_tree){

  weighted_tree_df <- cbind(get.edgelist(cltree),E(cltree)$weight)
  weighted_tree_df_weights <- cbind(weighted_tree_df, 1-as.numeric(weighted_tree_df[,3]))

colnames(weighted_tree_df_weights) <- c("Source feature", "Target feature", "Weight", "Dependency")

weighted_tree_df_weights
}

write_tsv(type_convert(as_tibble(weighted_tree_df_weights)), "Sahul_cltree.tsv")
