if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               optrees,
               infotheo,
               furrr,
               fields,
               igraph,
               cluster, 
               ape, 
               ggmap, 
               entropy,
               maptools, 
               maps, 
               mapdata,
               ggplot2, 
               mapproj, 
               knitr, 
               gplots,
               MASS, 
               colorspace,
               RColorBrewer,
               randomcoloR,
               glue, 
               reshape2)


p_load_gh("abrozzi/microbio")

