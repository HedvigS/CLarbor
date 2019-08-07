source("package_dependencies.R")

unweighted_dist <- as.dist(read_tsv("output/distance_matrices/unweighted_dist.tsv"))

observations<- unweighted_dist %>% 
  as.matrix() %>% 
  colnames()

glottolog <- read_tsv("data/Glottolog_lookup_table_Heti_edition.tsv") %>% 
  dplyr::select(Name = Name_stripped_no_spaces, ID = glottocode, AUTOTYP_area, Family_color) %>% 
  filter(ID %in% observations)

###NJTs

#unwegithed NN

njs_unweighted <- njs(unweighted_dist)

njs_unweighted$tip.label %>% 
  tibble() %>% 
  dplyr::select(ID = ".") %>% 
  left_join(glottolog) %>%
  .$Name -> njs_unweighted$tip.label

njs_unweighted$tip.label %>% 
  tibble() %>% 
  dplyr::select(Name = ".") %>% 
  left_join(glottolog) %>% 
  .$Family_color -> Family_colors

pdf("output/Vizualisations/distance_based/Neighbour_joining_trees/NJT_unwegithed.pdf", height = 20, width = 5)
plot(njs_unweighted, tip.color = Family_colors, node.pos = 1, cex =  0.6)
dev.off()

##NJT weighted uncontrolled

weighted_uncontrolled_dist <- as.dist(read_tsv("output/distance_matrices/weighted_uncontrolled_dist.tsv"))

njs_weighted_uncontrolled <- njs(weighted_uncontrolled_dist)

njs_weighted_uncontrolled$tip.label %>% 
  tibble() %>% 
  dplyr::select(ID = ".") %>% 
  left_join(glottolog) %>%
  .$Name -> njs_weighted_uncontrolled$tip.label

njs_weighted_uncontrolled$tip.label %>% 
  tibble() %>% 
  dplyr::select(Name = ".") %>% 
  left_join(glottolog) %>% 
  .$Family_color -> Family_colors

pdf("output/Vizualisations/distance_based/Neighbour_joining_trees/NJT_weighted_uncontrolled.pdf", height = 20, width = 5)
plot(njs_weighted_uncontrolled, tip.color = Family_colors, node.pos = 1, cex =  0.6)
dev.off()

##NJT weighted controlled

weighted_controlled_dist <- as.dist(read_tsv("output/distance_matrices/weighted_controlled_dist.tsv"))

weighted_controlled_dist_for_NJ <- weighted_controlled_dist

#weighted_controlled_dist_for_NJ[is.na(weighted_controlled_dist_for_NJ)] <- mean(weighted_controlled_dist_for_NJ, na.rm = T)

njs_weighted_controlled <- njs(weighted_controlled_dist_for_NJ)

njs_weighted_controlled$tip.label %>% 
  tibble() %>% 
  dplyr::select(ID = ".") %>% 
  left_join(glottolog) %>%
  .$Name -> njs_weighted_controlled$tip.label

njs_weighted_controlled$tip.label %>% 
  tibble() %>% 
  dplyr::select(Name = ".") %>% 
  left_join(glottolog) %>% 
  .$Family_color -> Family_colors

pdf("output/Vizualisations/distance_based/Neighbour_joining_trees/NJT_wegithed_controlled.pdf", height = 20, width = 5)
plot(njs_weighted_controlled, tip.color = Family_colors, node.pos = 1, cex =  0.6)
dev.off()

pdf("output/Vizualisations/distance_based/Neighbour_joining_trees/NJT_all_three.pdf", height = 10, width = 10)
par(mfrow = c(1,3))
plot(njs_unweighted, tip.color = Family_colors, node.pos = 1, cex =  0.6)
plot(njs_weighted_uncontrolled, tip.color = Family_colors, node.pos = 1, cex =  0.6)
plot(njs_weighted_controlled, tip.color = Family_colors, node.pos = 1, cex =  0.6)
dev.off()

### MDS UNweighted

unweighted_dist_for_MDS <- unweighted_dist

#unweighted_dist_for_MDS[is.na(unweighted_dist_for_MDS)] <- mean(unweighted_dist, na.rm = T)

unweighted_MDS <- cmdscale(unweighted_dist_for_MDS, k = 5) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  rename(ID = rowname) %>% 
  left_join(glottolog)

unweighted_MDS_plot <- ggplot(data = unweighted_MDS) +
  geom_point(aes(x = V1, y = V2, color = Family_color), size = 0.5, alpha = 0.8) +
  coord_fixed() +
  theme_classic() +
  theme(legend.position = "None")  +
  scale_color_identity()

plot(unweighted_MDS_plot)


#MDS weighted uncontrolled

weighted_uncontrolled_dist_for_MDS <- weighted_uncontrolled_dist

#weighted_uncontrolled_dist_for_MDS[is.na(weighted_uncontrolled_dist_for_MDS)] <- mean(weighted_uncontrolled_dist_for_MDS, na.rm = T)

weighted_uncontrolled_MDS <- cmdscale(weighted_uncontrolled_dist_for_MDS, k = 5) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  rename(ID = rowname) %>% 
  left_join(glottolog)

weighted_uncontrolled_MDS_plot <- ggplot(data = weighted_uncontrolled_MDS) +
  geom_point(aes(x = V1, y = V2, color = Family_color), size = 0.5, alpha = 0.8) +
  coord_fixed() +
  theme_classic() +
  theme(legend.position = "None") +
  scale_color_identity()

plot(weighted_uncontrolled_MDS_plot)


#MDS weighted controlled

weighted_controlled_dist_for_MDS <- weighted_controlled_dist

#weighted_controlled_dist_for_MDS[is.na(weighted_controlled_dist_for_MDS)] <- mean(weighted_controlled_dist_for_MDS, na.rm = T)

weighted_controlled_MDS <- cmdscale(weighted_controlled_dist_for_MDS, k = 5) %>% 
  as.data.frame() %>% 
  rownames_to_column("ID") %>% 
  as_tibble() %>% 
  left_join(glottolog) 

weighted_controlled_MDS_plot <- ggplot(data = weighted_controlled_MDS) +
  geom_point(aes(x = V1, y = V2, color = Family_color), size = 0.5, alpha = 0.8) +
  coord_fixed() +
  theme_classic()  +
 theme(legend.position = "None") +
  scale_color_identity()


unweighted_MDS_cropped <- unweighted_MDS %>% 
  dplyr::select(ID, unweighted_V1 = V1, unweighted_V2 = V2) %>% 
  mutate(which = "unweighted")

weighted_uncontrolled_MDS_cropped <- weighted_uncontrolled_MDS %>% 
  dplyr::select(ID, weighted_uncontrolled_V1 = V1,weighted_uncontrolled_V2 = V2) %>% 
  mutate(which = "weighted_uncontrolled")

weighted_controlled_MDS_cropped <- weighted_controlled_MDS %>% 
  dplyr::select(ID, weighted_controllde_V1 = V1, weighted_controlledV2 = V2)  %>% 
  mutate(which = "weighted_controlled")


weighted_uncontrolled_MDS

MDS_all_three <- ggarrange(unweighted_MDS_plot, weighted_uncontrolled_MDS_plot, weighted_controlled_MDS_plot,  ncol = 3, nrow = 1)

plot(MDS_all_three)


pdf("output/Vizualisations/distance_based/MDS/MDS_all_three.pdf", height = 10, width = 10)
plot(MDS_all_three)
dev.off()

  
