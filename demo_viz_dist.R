library(tidyverse) 
library(ape)
library(RColorBrewer)
library(ggpubr)
library(reshape2)

glottolog <- read_tsv("demo_data/Glottolog_all_languoids_Heti_enhanced.tsv") %>% 
  dplyr::select(Name, Glottocode, Family_name, AUTOTYP_area)


###NJTs

#unwegithed NN

sahul_unweighted_dist <- as.dist(read_tsv("output/distance_matrices/Sahul_unweighted_dist.tsv"))


Sahul_njs_unweighted <- njs(sahul_unweighted_dist)

Sahul_njs_unweighted$tip.label %>% 
  tibble() %>% 
  dplyr::select(Glottocode = ".") %>% 
  left_join(glottolog) %>%
  .$Name -> Sahul_njs_unweighted$tip.label

Sahul_njs_unweighted$tip.label %>% 
  tibble() %>% 
  dplyr::select(Name = ".") %>% 
  left_join(glottolog) %>% 
  .$Family_name -> Sahul_family

n_colors <- length(unique(Sahul_family))

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

pie(rep(1,n_colors), col=sample(col_vector, n_colors))

Sahul_family_colors <- col_vector[as.factor(Sahul_family)]

pdf("output/Vizualisations/distance_based/Neighbour_joining_arbors/Sahul_NJT_unwegithed.pdf", height = 20, width = 5)
plot(Sahul_njs_unweighted, tip.color = Sahul_family_colors, node.pos = 1, cex =  0.6)
dev.off()


##NJT weighted uncontrolled

sahul_weighted_uncontrolled_dist <- as.dist(read_tsv("output/distance_matrices/Sahul_weighted_uncontrolled_dist.tsv"))

Sahul_njs_weighted_uncontrolled <- njs(sahul_weighted_uncontrolled_dist)

Sahul_njs_weighted_uncontrolled$tip.label %>% 
  tibble() %>% 
  dplyr::select(Glottocode = ".") %>% 
  left_join(glottolog) %>%
  .$Name -> Sahul_njs_weighted_uncontrolled$tip.label

Sahul_njs_weighted_uncontrolled$tip.label %>% 
  tibble() %>% 
  dplyr::select(Name = ".") %>% 
  left_join(glottolog) %>% 
.$Family_name -> Sahul_family

n_colors <- length(unique(Sahul_family))

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

pie(rep(1,n_colors), col=sample(col_vector, n_colors))

Sahul_family_colors <- col_vector[as.factor(Sahul_family)]

pdf("output/Vizualisations/distance_based/Neighbour_joining_arbors/Sahul_NJT_weighted_uncontrolled.pdf", height = 20, width = 5)
plot(Sahul_njs_weighted_uncontrolled, tip.color = Sahul_family_colors, node.pos = 1, cex =  0.6)
dev.off()

##NJT weighted controlled

sahul_weighted_controlled_dist <- as.dist(read_tsv("output/distance_matrices/Sahul_weighted_controlled_dist.tsv"))

sahul_weighted_controlled_dist_for_NJ <- sahul_weighted_controlled_dist

#sahul_weighted_controlled_dist_for_NJ[is.na(sahul_weighted_controlled_dist_for_NJ)] <- mean(sahul_weighted_controlled_dist_for_NJ, na.rm = T)

Sahul_njs_weighted_controlled <- njs(sahul_weighted_controlled_dist_for_NJ)

Sahul_njs_weighted_controlled$tip.label %>% 
  tibble() %>% 
  dplyr::select(Glottocode = ".") %>% 
  left_join(glottolog) %>%
  .$Name -> Sahul_njs_weighted_controlled$tip.label

Sahul_njs_weighted_controlled$tip.label %>% 
  tibble() %>% 
  dplyr::select(Name = ".") %>% 
  left_join(glottolog) %>% 
  .$Family_name -> Sahul_family

n_colors <- length(unique(Sahul_family))

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

pie(rep(1,n_colors), col=sample(col_vector, n_colors))

Sahul_family_colors <- col_vector[as.factor(Sahul_family)]


pdf("output/Vizualisations/distance_based/Neighbour_joining_arbors/Sahul_NJT_wegithed_controlled.pdf", height = 20, width = 5)
plot(Sahul_njs_weighted_controlled, tip.color = Sahul_family_colors, node.pos = 1, cex =  0.6)
dev.off()

pdf("output/Vizualisations/distance_based/Neighbour_joining_arbors/Sahul_NJT_all_three.pdf", height = 10, width = 10)
par(mfrow = c(1,3))
plot(Sahul_njs_unweighted, tip.color = Sahul_family_colors, node.pos = 1, cex =  0.6)
plot(Sahul_njs_weighted_uncontrolled, tip.color = Sahul_family_colors, node.pos = 1, cex =  0.6)
plot(Sahul_njs_weighted_controlled, tip.color = Sahul_family_colors, node.pos = 1, cex =  0.6)
dev.off()


### MDS UNweighted

sahul_unweighted_dist_for_MDS <- sahul_unweighted_dist

#sahul_unweighted_dist_for_MDS[is.na(sahul_unweighted_dist_for_MDS)] <- mean(sahul_unweighted_dist, na.rm = T)

Sahul_unweighted_MDS <- cmdscale(sahul_unweighted_dist_for_MDS, k = 5) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  rename(Glottocode = rowname) %>% 
  left_join(glottolog)

Sahul_unweighted_MDS_plot <- ggplot(data = Sahul_unweighted_MDS) +
  geom_point(aes(x = V1, y = V2, color = AUTOTYP_area), size = 0.5, alpha = 0.8) +
  coord_fixed() +
  theme_classic() +
  theme(legend.position = "None")

plot(Sahul_unweighted_MDS_plot)


#MDS weighted uncontrolled

sahul_weighted_uncontrolled_dist_for_MDS <- sahul_weighted_uncontrolled_dist

#sahul_weighted_uncontrolled_dist_for_MDS[is.na(sahul_weighted_uncontrolled_dist_for_MDS)] <- mean(sahul_weighted_uncontrolled_dist_for_MDS, na.rm = T)

Sahul_weighted_uncontrolled_MDS <- cmdscale(sahul_weighted_uncontrolled_dist_for_MDS, k = 5) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  rename(Glottocode = rowname) %>% 
  left_join(glottolog)

Sahul_weighted_uncontrolled_MDS_plot <- ggplot(data = Sahul_weighted_uncontrolled_MDS) +
  geom_point(aes(x = V1, y = V2, color = AUTOTYP_area), size = 0.5, alpha = 0.8) +
  coord_fixed() +
  theme_classic() +
  theme(legend.position = "None")

plot(Sahul_weighted_uncontrolled_MDS_plot)


#MDS weighted controlled

sahul_weighted_controlled_dist_for_MDS <- sahul_weighted_controlled_dist

#sahul_weighted_controlled_dist_for_MDS[is.na(sahul_weighted_controlled_dist_for_MDS)] <- mean(sahul_weighted_controlled_dist_for_MDS, na.rm = T)

Sahul_weighted_controlled_MDS <- cmdscale(sahul_weighted_controlled_dist_for_MDS, k = 5) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  rename(Glottocode = rowname) %>% 
  left_join(glottolog) 

Sahul_weighted_controlled_MDS_plot <- ggplot(data = Sahul_weighted_controlled_MDS) +
  geom_point(aes(x = V1, y = V2, color = AUTOTYP_area), size = 0.5, alpha = 0.8) +
  coord_fixed() +
  theme_classic()  +
 theme(legend.position = "None")



Sahul_unweighted_MDS_cropped <- Sahul_unweighted_MDS %>% 
  dplyr::select(Glottocode, unweighted_V1 = V1, unweighted_V2 = V2) %>% 
  mutate(which = "unweighted")

Sahul_weighted_uncontrolled_MDS_cropped <- Sahul_weighted_uncontrolled_MDS %>% 
  dplyr::select(Glottocode, weighted_uncontrolled_V1 = V1,weighted_uncontrolled_V2 = V2) %>% 
  mutate(which = "weighted_uncontrolled")

Sahul_weighted_controlled_MDS_cropped <- Sahul_weighted_controlled_MDS %>% 
  dplyr::select(Glottocode, weighted_controllde_V1 = V1, weighted_controlledV2 = V2)  %>% 
  mutate(which = "weighted_controlled")


Sahul_weighted_uncontrolled_MDS

sahul_MDS_all_three <- ggarrange(Sahul_unweighted_MDS_plot, Sahul_weighted_uncontrolled_MDS_plot, Sahul_weighted_controlled_MDS_plot,  ncol = 3, nrow = 1)

plot(sahul_MDS_all_three)


pdf("output/Vizualisations/distance_based/MDS/Sahul_MDS_all_three.pdf", height = 10, width = 10)
plot(sahul_MDS_all_three)
dev.off()

  
