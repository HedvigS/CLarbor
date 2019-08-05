library(tidyverse)
library(ggmap)
library(maptools)
library(maps)
library(mapdata)
library(ggplot2)
library(mapproj)

#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#read in
glottolog <- read_tsv("demo_data/Glottolog_all_languoids_Heti_enhanced.tsv")

Sahul_sample <- read_tsv("demo_data/tidied/Sahul_value_df.tsv") %>% 
  dplyr::select(Glottocode) %>% 
  left_join(glottolog)

#shifting the longlat of the dataframe to match the pacific centered map
Sahul_sample_lat_long_shifted <- Sahul_sample %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

#Basemap
basemap <- ggplot(Sahul_sample_lat_long_shifted) +
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
  
#  expand_limits(x = Sahul_sample_lat_long_shifted$Longitude, y = Sahul_sample_lat_long_shifted$Latitude)

Sahul_sample_plot_minus_dutch <- basemap + 
       geom_point(size = 1.5, aes(x=Longitude, y=Latitude, color=Family_name), 
                  shape = 19, alpha = 0.6, stroke = 0) +
         theme(title  = element_text(size = 135)) 

pdf(height = 5, width = 9.2, file = "output/vizualisations/Sahul_sample_map.pdf")
plot(Sahul_sample_plot_minus_dutch)
dev.off()
