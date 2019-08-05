source("package_dependencies.R")

Sahul_lgs <- read_tsv("demo_data/Sahul_structure_wide.tsv") %>% 
  dplyr::select(Glottocode)

Sahul_lat_long <- read_tsv("demo_data/Glottolog_all_languoids_Heti_enhanced.tsv") %>% 
  inner_join(Sahul_lgs) %>% 
  dplyr::select(Glottocode, Longitude, Latitude) %>% 
  column_to_rownames("Glottocode") 

Sahul_geo_dist <- rdist.earth(Sahul_lat_long, Sahul_lat_long, miles = F)
diag(Sahul_geo_dist) <- 0

colnames(Sahul_geo_dist) <- rownames(Sahul_lat_long) 
rownames(Sahul_geo_dist ) <- rownames(Sahul_lat_long) 

Sahul_geo_dist_list <- Sahul_geo_dist %>% 
  melt() %>% 
  filter(Var1 != Var2) %>% 
  rename(geo_dist = value)

#unweighted dist list
Sahul_unweighted_dists <- read_tsv("output/distance_matrices/Sahul_unweighted_dist.tsv") %>% 
  as.matrix()

rownames(Sahul_unweighted_dists) <- colnames(Sahul_unweighted_dists)

Sahul_unweighted_dists_list <- Sahul_unweighted_dists %>% 
  melt() %>% 
  filter(Var1 != Var2) %>% 
  rename(unweighted_dist = value)


#weighted uncontrolled dist list
Sahul_weighted_uncontrolled_dists <- read_tsv("output/distance_matrices/Sahul_weighted_uncontrolled_dist.tsv") %>% 
  as.matrix()

rownames(Sahul_weighted_uncontrolled_dists) <- colnames(Sahul_weighted_uncontrolled_dists)

Sahul_weighted_uncontrolled_dists_list <- Sahul_weighted_uncontrolled_dists %>% 
  melt() %>% 
  filter(Var1 != Var2) %>% 
  rename(Weighted_uncontrolled_dist = value)



#weighted controlled dist list
Sahul_weighted_controlled_dists <- read_tsv("output/distance_matrices/Sahul_weighted_controlled_dist.tsv") %>% 
  as.matrix()

rownames(Sahul_weighted_controlled_dists) <- colnames(Sahul_weighted_controlled_dists)

Sahul_weighted_controlled_dists_list <- Sahul_weighted_controlled_dists %>% 
  melt() %>% 
  filter(Var1 != Var2) %>% 
  rename(Weighted_controlled_dist = value)


#all three
Sahul_all_dists <- Sahul_geo_dist_list %>% 
  full_join(Sahul_unweighted_dists_list) %>% 
  full_join(Sahul_weighted_uncontrolled_dists_list) %>% 
  full_join(Sahul_weighted_controlled_dists_list)


ggplot(data = Sahul_all_dists) +
  geom_point(aes(x = geo_dist, y = unweighted_dist), color = "red", alpha = 0.5) +
  geom_point(aes(x = geo_dist, y = Weighted_uncontrolled_dist), color = "blue", alpha = 0.5) + geom_point(aes(x = geo_dist, y = Weighted_controlled_dist), color = "green", alpha = 0.5) +
  theme_classic()

ggsave("output/Vizualisations/distance_based/Geo_dist_comparison.png")
  





