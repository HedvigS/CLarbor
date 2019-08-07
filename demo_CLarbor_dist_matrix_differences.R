source("package_dependencies.R")

unweighted_matrix <- read_tsv("output/distance_matrices/unweighted_dist.tsv") %>% 
  as.matrix()

weighted_controlled_matrix <- read_tsv("output/distance_matrices/weighted_controlled_dist.tsv") %>% 
  as.matrix()

weighted_uncontrolled_matrix <- read_tsv("output/distance_matrices/weighted_uncontrolled_dist.tsv") %>% 
  as.matrix()

rownames(unweighted_matrix) <- rownames(weighted_controlled_matrix) <- rownames(weighted_uncontrolled_matrix) <- 
  colnames(unweighted_matrix)

Glottolog <- read_tsv("data/Glottolog_lookup_table_Heti_edition.tsv") %>% 
  dplyr::select(Name, ID = glottocode, AUTOTYP_area, Family_color, Longitude, Latitude)

diff_1 <- weighted_uncontrolled_matrix - unweighted_matrix

diff_1_pairs <- diff_1 %>% as.data.frame() %>% 
  rownames_to_column() %>% 
  gather(colname, difference, -rowname) %>% 
  transmute(pair = map2_chr(rowname, colname, function(x,y){sort(c(x,y)) %>% paste(collapse = " ")}),
            difference) %>% 
  distinct() %>%
  separate(pair, c("lang1", "lang2")) %>% 
  left_join(Glottolog %>% 
              dplyr::select(ID, Name) %>% 
              rename(lang1 = ID, Name1 = Name)) %>% 
              left_join(Glottolog %>% dplyr::select(ID, Name) %>%
              rename(lang2 = ID, Name2 = Name)) %>% 
  dplyr::select(lang1 = Name1, lang2 = Name2, difference) %>% 
  arrange(difference)

diff_1_table <- full_join(head(diff_1_pairs, 5),
                          tail(diff_1_pairs, 5))

diff_2 <- weighted_controlled_matrix - weighted_uncontrolled_matrix

diff_2_pairs <- diff_2 %>% as.data.frame() %>% 
  rownames_to_column() %>% 
  gather(colname, difference, -rowname) %>% 
  transmute(pair = map2_chr(rowname, colname, function(x,y){sort(c(x,y)) %>% paste(collapse = " ")}),
            difference) %>% 
  distinct() %>%
  separate(pair, c("lang1", "lang2")) %>% 
              left_join(Glottolog %>% 
              dplyr::select(ID, Name) %>% 
              rename(lang1 = ID, Name1 = Name)) %>% 
            left_join(Glottolog %>% dplyr::select(ID, Name) %>% rename(lang2 = ID, Name2 = Name)) %>% 
  dplyr::select(lang1 = Name1, lang2 = Name2, difference) %>% 
  arrange(difference)

diff_2_table <- full_join(head(diff_2_pairs, 5),
                          tail(diff_2_pairs, 5))

diff_1_avgs <- diff_1_pairs %>% 
  unite("Name", lang1, lang2) %>% 
  mutate(Name = str_split(Name, "_")) %>% 
  unnest() %>% 
  group_by(Name) %>% 
  summarise(difference = mean(difference)) %>% 
  left_join(Glottolog %>% dplyr::select(Name, Longitude, Latitude))

diff_2_avgs <- diff_2_pairs %>% 
  unite("Name", lang1, lang2) %>% 
  mutate(Name = str_split(Name, "_")) %>% 
  unnest() %>% 
  group_by(Name) %>% 
  summarise(difference = mean(difference)) %>% 
  left_join(Glottolog %>% dplyr::select(Name, Longitude, Latitude))

# Plotting!

world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)
lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

basemap <- ggplot() +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", size = 0.3)  + 
  theme(legend.position="none",
        panel.grid.major = element_blank(), #all of these lines are just removing default things like grid lines, axises etc
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") +
  xlim(c(90, 230)) +
  ylim(c(-56, 20))

diff1_plot <- basemap +
  geom_point(data = diff_1_avgs, size = 1.5, aes(x=Longitude, y=Latitude, color=difference), 
             shape = 19, alpha = 0.6, stroke = 0) +
  theme(title  = element_text(size = 135)) +
  scale_color_gradient2()

diff2_plot <- basemap +
  geom_point(data = diff_2_avgs, size = 1.5, aes(x=Longitude, y=Latitude, color=difference), 
             shape = 19, alpha = 0.6, stroke = 0) +
  theme(title  = element_text(size = 135)) +
  scale_color_gradient2()

pdf(height = 5, width = 9.2, file = "output/vizualisations/difference_maps/diff1_map_unweighted_vs_weighted_uncontrolled.pdf")
plot(diff1_plot)
dev.off()

pdf(height = 5, width = 9.2, file = "output/vizualisations/difference_maps/diff2_map_weighted_uncontrolled_vs_weighted_controlled.pdf")
plot(diff2_plot)
dev.off()