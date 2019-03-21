library(igraph)
library(optrees)
library(tidyverse)
library(infotheo)

source("chuliu.R")

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

Glottolog <- read_tsv("Glottolog_all_languoids_Heti_enhanced.tsv") %>% 
  dplyr::rename(Glottocode = Language_glottocode) %>% 
  dplyr::select(Glottocode,
                Family_name,
                Macroarea,
                Latitude,
                Longitude,
                level,
                Language_level_ID,
                AUTOTYP_area) 

Grambank_wide <- read_tsv("../../GB_descriptive_stats/GB_wide/GB_wide_strict.tsv")  %>% 
  rename(Glottocode = Language_ID) %>%  
  left_join(Glottolog) 

#   filter(Glottocode != "chem12512") %>% 
#   filter(Glottocode != "utee1244") %>% 
#   filter(Glottocode != "sout2969") %>% 
#   filter(Glottocode != "cuti1242") %>% 
#   filter(Glottocode != "katu1276") %>% 
#   filter(Glottocode != "umii1235") 

#hedvig to move to make gb wide
# %>% 
#   dplyr::select(-Glottocode) %>% 
#   rename(Glottocode = Language_level_ID) %>% 
#   dplyr::select(Glottocode, everything())
#   
# Grambank_sample <- aggregate(na_prop ~ Glottocode, Grambank_wide, FUN = min) %>% 
#   inner_join(Grambank_wide) %>% 
#   distinct() 

Grambank_only_features <- Grambank_wide %>% 
  dplyr::select(-Family_name, -level, -Longitude, -Latitude, -Macroarea, -na_prop, -AUTOTYP_area)

Grambank_meta <- Grambank_wide %>% 
  dplyr::select(Glottocode, Family_name, Longitude,  Latitude, Macroarea, AUTOTYP_area) %>% 
  unite(Family_area, c("AUTOTYP_area", "Family_name"), remove = F)

GB_lgs <- Grambank_only_features$Glottocode

Grambank_for_depfun <- as.data.frame(lapply(Grambank_only_features[,-1], as.factor))
GBvar <- colnames(Grambank_only_features)
rownames(Grambank_for_depfun) <- GB_lgs

#this is function that creates an adjacency matrix, a table, that contains all the nodes (variables) as rows and columns. The rows are the source nodes and the columns are the target. The intersecting value, the cell given a certain source and target, is the edge weight of that directed edge.
   #This is a function that calculates the dependency between two nodes in the adjacency matrix (table) of the graph. We're using an entropy function that is ill-suited for small numbers of values, but that's not a problem because even though our absoulte number of poorly filled variables it is not small relatively.

GBvar <- GBvar[grep("GB", GBvar)]

Grambank_only_features %>% 
  full_join(Grambank_meta) -> Grambank_joined

Grambank_joined$Family_area <- as.factor(Grambank_joined$Family_area)

depfun <- function(x,y){
  d = as.matrix(Grambank_joined[,c(x, y, "Family_area")])
  d = d[complete.cases(d),]
  condinformation(d[,1], d[,2], d[,3])/condentropy(d[,2], d[,3])}

dependencys <- outer(GBvar,GBvar,Vectorize(depfun)) #this is two for-loops that assigns edge weights in the graph, it applies the function depfun to every pair of variables. This creates the adjacency matrix for the weighted graph.

message(paste("The adjancey matrix for the weighted directed graph, the maximum spanning tree, is now created. There were", sum(is.na(dependencys)), "cells with missing dependency values."))

dimnames(dependencys) <- list(GBvar,GBvar)
G <- 1 - dependencys
dimnames(G) <- NULL

cltree <- chuliu(G,1)
V(cltree)$name <- GBvar
#write.csv(cbind(get.edgelist(cltree),E(cltree)$weight),"GB-cltree.csv")
GBweights <- c(1,unlist(sapply(2:length(GBvar),function (x) max(incident(cltree,x,mode = "in")$weight,0))))

GBcltree <- cbind(get.edgelist(cltree),E(cltree)$weight)
GB_weigthed_gower_features <- cbind(GBcltree, 1-as.numeric(GBcltree[,3]))
colnames(GB_weigthed_gower_features) <- c("Source feature", "Target feature", "Weight", "Dependency")
write.csv(GB_weigthed_gower_features,"Chu_Liu/GB-cltree.csv")

message(paste("GB contains", 100*sum(as.numeric(GB_weigthed_gower_features[,4]))/nrow(GB_weigthed_gower_features), "% feature mass that can be considered as redundant."))

