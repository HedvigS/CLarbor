# value_df <- read_tsv("Sahul_structure_wide.tsv")
# 
# sahul_IDS <- read_tsv("Sahul_structure_wide.tsv") %>% 
#   dplyr::select(Glottocode)
# 
# random_effects_df <- read_tsv("Glottolog_all_languoids_Heti_enhanced.tsv") %>% 
#   dplyr::select(Glottocode,
#                 Family_name,
#                 AUTOTYP_area) %>% 
#   right_join(sahul_IDS)

dependency_matrix_cond_MI <- function(value_df, random_effects_df){
  
require(tidyverse)
require(infotheo)

##################CHU-LIU pruning ###########################

# This script first creates a complete, weighted and directed graph (network). The weight is given by the dependency. If A very often precits the value of B, then the edge leading from A to B is heavier. This does not mean that the returning edge of B to A is of equal weight or even related at all. The dependencies here are calculated based on relative entropy, see the Hammarstrom & O'Connor paper.
# The script then creates a maximum spanning tree. A tree is a directed non-cylical (i.e. rooted) connected graph, it has direction, one root node and every node is connected to every other, and especially the root node. A forest is the same, minus the enforced single root. You could run a forest script and get one tree though.
# The tree is just a kind of graph, it doesn't inherently case about shortest path or anything else, it cares about the conditions it is given when you ask it to come into existence. It always needs conditions, even "random subtree" is a condition. The same goes for a spanning tree, it's just a kind of graph and needs conditions to be given to it.
# A spanning tree is a tree that necessarily covers all nodes in the data set. A tree could technically only cover a subset. 
#A maximum spanning tree is when the tree makes use of the weights of the edges to maximize the sum of the edge weights. Another way to think about this is that when a edge is heavy, it makes for faster travel and that the maximum spanning tree is trying to connect all the nodes in such a way that the travel time is as low as possible.
# The tree, all these trees, will need a root. It's fairly non-problematic to just pick any node as the root, especially if you have a large amount of nodes (in this case we have approx 550). The root of a tree per definition only has out-going edges and no in-going edges. The leaf nodes have only one in-going edge and no out-going. The intermediate nodes per definition have only one in-coming but infitine out-going (this is not an enforced binary tree).
# In this case, we take the maximum spanning tree (a tree that covers all nodes and cares about weights) and we also assign weight to the nodes. This weight is 1 minus the incoming edge weight. This means that nodes that are highly predicatble by their fellow nodes will be given a low weight. This signifies that they contribute little to the distance between two societies, they're not very important variables. In this tree, the leaf nodes will be variables that have the little predictable power. However, we did arbitratily picked the root node, if we hadn't we would have picked the node that has the highest predictive power and that would mean that the route from the root to the leaves would be as short as possible. This is not what we've done.
# In our case it's still true that the leaves are the most predictabel variables, it's just that they're not necessarily the most preidictable given the specific root.
# The reason why we're not calculating the weight based on the sum of the edge weights on the route from the root to the node is because we're doing this for all the non-root nodes and not just the leaf nodes, and we don't want to double count and we don't want the node weights to be depenendent on each other.
# The root gets a weight of one, it's as if it had an in-coming edge weight of 0 (1 minues 0 is 1).
# we then build a network where only the heaviest incoming edge to each node exist. This is not a sequential process but parallell. This may result in loops. Next, we need to resolve said loops. We do so by collapsing them into a supernode, delete one edge and then create a new network. We continue doing this until we have no loops. Remember, loops can consist of many edges, but only one edge needs to be deleted for the loop to cease. This results in a spanning tree. This is not yet a minimum spanning tree, that's next step.
# The minumum spanning tree is created from the earlier tree with supernodes by expanding each node into it's original nodes.

if (sum(is.na(random_effects_df)) > 0) {
  stop("There is missing data in your dataframe for random effects. Please make sure that all rows have complete entries.")
}

random_effect_vars <- colnames(random_effects_df[,-1])

random_effects_df_united <- random_effects_df %>% 
  unite("random_effects_united", random_effect_vars, remove = T) %>% 
  mutate("random_effects_united" = as.factor(random_effects_united))

number_of_duplicates <- sum(duplicated(value_df[,1]))

if (number_of_duplicates > 0){
  stop("Not all of the IDs are unique! Please recheck.")
}

if (!all(value_df[,1] == random_effects_df_united[,1])){
  stop("Random effects IDs and value IDs are not the same! Please recheck.")
}

make_features_factors <- function (df){
  df[,-1] %>% 
    map_df(as.factor) %>% 
    mutate(rowname = df[,1][[1]]) %>% 
    column_to_rownames()
}

value_df_for_depfun <- make_features_factors(value_df)
random_effects_df_for_depfun <- make_features_factors(random_effects_df_united)

#this is function that creates an adjacency matrix, a table, that contains all the nodes (variables) as rows and columns. The rows are the source nodes and the columns are the target. The intersecting value, the cell given a certain source and target, is the edge weight of that directed edge.
   #This is a function that calculates the dependency between two nodes in the adjacency matrix (table) of the graph. We're using an entropy function that is ill-suited for small numbers of values, but that's not a problem because even though our absoulte number of poorly filled variables it is not small relatively.

value_vars <- colnames(value_df_for_depfun)

dfs_joined <- cbind(value_df_for_depfun, random_effects_df_for_depfun)


depfun <- function(x,y){
  d = as.matrix(dfs_joined[,c(x, y, "random_effects_united")])
  d = d[complete.cases(d),,drop = F]
  condinformation(d[,1], d[,2], d[,3])/condentropy(d[,2], d[,3])}

# Testing only:
dependencys <- matrix(nrow = length(value_vars), ncol = length(value_vars))
loop_length <- length(value_vars)*(length(value_vars)+1)/2
for (i in 1:length(value_vars)){
  for (j in i:length(value_vars)){
    progress <- (loop_length - ((length(value_vars) - i)*((length(value_vars) - i)+1)/2) + j)/loop_length
    if (j == i){
      message(paste("Currently at ",  round(100*progress, 2), "%"))
      }
    dependencys[i,j] <- depfun(value_vars[i],value_vars[j])
    }
  }

#This is an older version that does the same thing as the above forloop but with Outer instead. The foorloop is more talky, which is preferred since this takes a long time to run.
#dependencys <- outer(value_vars,value_vars,Vectorize(depfun)) #this is two for-loops that assigns edge weights in the graph, it applies the function depfun to every pair of variables. This creates the adjacency matrix for the weighted graph.

dependencys_sym <- dependencys

dependencys_sym[lower.tri(dependencys_sym)] <- t(dependencys_sym)[lower.tri(dependencys_sym)]

message(paste("The adjancey matrix for the weighted directed graph, the maximum spanning tree, is now created. There were", sum(is.nan(dependencys_sym)), "cells with missing dependency values."))

dependencys_sym[is.nan(dependencys_sym)] <- 0

diag(dependencys_sym) <- 1

colnames(dependencys_sym) <- value_vars
rownames(dependencys_sym) <- value_vars

to_drop <- c(which(apply(dependencys_sym, 2, function(x){mean(is.nan(x))}) == 1), which(colMeans(dependencys_sym, na.rm = T) == 0))

dependencys_sym_no_missing <- dependencys_sym[-to_drop, -to_drop]

}

#dep_matrix <- dependencys_sym_no_missing


cltree_from_dep_matrix_known_root <- function(dep_matrix, root){
  require(igraph)
  require(optrees)
  
#  source("chuliu.R")

value_vars <-colnames(dep_matrix)

if(is.character(root)) {
  root <- which(value_vars == root)
  }

G <- 1 - dep_matrix

dimnames(G) <- NULL

dep_graph <- graph_from_adjacency_matrix(G + .Machine$double.eps, weighted = T)

cltree_optrees <- getMinimumArborescence(V(dep_graph), get.edgelist(dep_graph), source.node = root)

#cltree <- chuliu(G,root)

V(cltree_optrees)$name <- value_vars

redundant_percentage<- 100*sum(1 - E(cltree_optrees)$weight)/length(E(cltree_optrees)$weight)

message(paste("Your dataset contains", redundant_percentage, "% mass that can be considered as redundant."))

cltree_optrees

}

cltree_from_dep_matrix_known_root(dependencys_sym, 1)

make_cltree_df <- function(weighted_tree){
weighted_tree_df <- cbind(get.edgelist(weighted_tree),E(weighted_tree)$weight)

weighted_tree_df_weights <- cbind(weighted_tree_df, 1-as.numeric(weighted_tree_df[,3]))

colnames(weighted_tree_df_weights) <- c("Source feature", "Target feature", "Weight", "Dependency")

weighted_tree_df_weights
}

