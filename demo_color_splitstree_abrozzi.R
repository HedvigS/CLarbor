source("package_dependencies.R")

#This script colours the tips and labels of a splitstree graph systematically by using the package "microbio" by abrozzi on github. The package is not (yet) on cran, so the package_dependencies script installs it directly from GitHub. 

#The function "custom_nexus()" of that package lets us colour the network. It expects certain things, I'll outline here what so that if you want to use this script to colour your own networks you can.

#custom_nexus()expects 
# * a nexus file with a network block with the segment "translate" within (taxa in the approriate order for the network). You cannot use the taxa block list, it needs to be in the order that splitstree has ordered the labels around the network. 
# * label names without spaces
# * a set of vectors for font and colours that is as long as the translate object
# you'll need to specify all colours, there aren't defauls

# I dealt with this by reading in the nexus file, subsetting to the vlabels but (same order as "translate"). I happen to know that my labels are names from a particular table of languages, so I make a df of those and join up all the kinds of meta data that I might want to colour them by. In this case, language_family. 

##UNWEIGHTED

##setting up the df of vlabel data

raw_data <- readLines("output/Vizualisations/distance_based/Neighbournet/splitstree_saves/sahul_unweighted_uncolored.nex") %>%
  tibble(
    line = 1:length(.),
    data = .
  )


#the vlabels chunk is the only chunk where all lines have "x="
raw_data %>% 
  filter(str_detect(data, " x=")) -> vlabels_raw

read_tsv("Glottolog_lookup_table_Heti_edition.tsv") %>%
  dplyr::select(Name = Name_stripped_no_spaces, Language_ID = glottocode,Family_name = Family_name_isolates_distinct, Macroarea, Countries) -> Glottolog

vlabels_raw %>% 
  separate(col = data, into = c("index", "Name", "formatting"), sep = fixed("'")) %>% 
  dplyr::select(-formatting, -line) %>%   
  left_join(Glottolog) -> vlabels

n <- length(unique(vlabels$Family_name))

#Below are 3 different ways of finding distinctive colors for the tips. The first 2 glue a bunch of things toegher

# 
#Colouring method 1
#If you need more than 73 distinctive colours, use this chunk. It has 433.
# color_vector <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
# 
# color_vector <- sample(color_vector, n)
# 
# vlabels$color_lc <- color_vector[as.factor(desc(vlabels$Family_name))]

#Colouring method 2
#If you have 74 or fewer colors you need, you could glue together all qualtiative palettes in RColorBrewer
# qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
#  
#  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#  
#  color_vector <- sample(col_vector, n)

#Colouring method 3
#using a function from the randomcoloR package
color_vector <- distinctColorPalette(n)

vlabels$color_lc <- color_vector[as.factor(desc(vlabels$Family_name))]

vlabels <- vlabels %>% 
  mutate(font = "Arial-BOLD-9") %>% 
  mutate(color_background = "white") %>% 
  mutate(color_tip = color_lc) %>% 
  mutate(color_fg = color_lc)
  
#Abrozz'is Microbio

inputfile <- "output/Vizualisations/distance_based/Neighbournet/splitstree_saves/sahul_unweighted_uncolored.nex"

outfile <- "output/Vizualisations/distance_based/Neighbournet/coloured/sahul_unweighted_colored.nex"

tips <- as.character(vlabels$Name)
col <- vlabels$color_tip
fg <- vlabels$color_fg
lc <- vlabels$color_lc
s <- "o"
w <- 4
h <- 4
lk <- vlabels$color_background
edgecolor <- "black"
f <- vlabels$font

custom_nexus(nexus.file=inputfile, tips=tips, colors=col, vlabels=tips, f=f, lc=lc, lk=lk, fg= fg, s = s, w = w, h = h, edgecolor=edgecolor, outfile= outfile, plot = F)


##WEIGHTED UNCONTROLLED

##setting up the df of vlabel data

raw_data <- readLines("output/Vizualisations/distance_based/Neighbournet/splitstree_saves/sahul_weighted_uncontrolled_uncolored.nex") %>%
  tibble(
    line = 1:length(.),
    data = .
  )


#the vlabels chunk is the only chunk where all lines have "x="
raw_data %>% 
  filter(str_detect(data, " x=")) -> vlabels

vlabels %>% 
  separate(col = data, into = c("index", "Name", "formatting"), sep = fixed("'")) %>% 
  separate(col = formatting, into = c("position", "color"), sep = ",") %>% 
  mutate(comma = ",") %>% 
  mutate(single_quote1 = "'") %>%  
  mutate(single_quote2 = "'") -> vlabels

read_tsv("Glottolog_lookup_table_Heti_edition.tsv") %>%
  dplyr::select(Name = Name_stripped_no_spaces, Language_ID = glottocode,Family_name = Family_name_isolates_distinct, Macroarea, Countries) -> Glottolog

vlabels %>% 
  left_join(Glottolog) -> vlabels

n <- length(unique(vlabels$Family_name))

vlabels$color_lc <- color_vector[as.factor(desc(vlabels$Family_name))]

vlabels <- vlabels %>% 
  mutate(font = "Arial-BOLD-9") %>% 
  mutate(color_background = "white") %>% 
  mutate(color_tip = color_lc) %>% 
  mutate(color_fg = color_lc)

#Abrozz'is Microbio

inputfile <- "output/Vizualisations/distance_based/Neighbournet/splitstree_saves/sahul_weighted_uncontrolled_uncolored.nex"

outfile <- "output/Vizualisations/distance_based/Neighbournet/coloured/sahul_weighted_uncontrolled_colored.nex"

tips <- as.character(vlabels$Name)
col <- vlabels$color_tip
fg <- vlabels$color_fg
lc <- vlabels$color_lc
s <- "o"
w <- 4
h <- 4
lk <- vlabels$color_background
edgecolor <- "gray15"
f <- vlabels$font

custom_nexus(nexus.file=inputfile, tips=tips, colors=col, vlabels=tips, f=f, lc=lc, lk=lk, fg= fg, s = s, w = w, h = h, edgecolor=edgecolor, outfile= outfile, plot = F)






##WEIGHTED controlled

##setting up the df of vlabel data

raw_data <- readLines("output/Vizualisations/distance_based/Neighbournet/splitstree_saves/sahul_weighted_controlled_uncolored.nex") %>%
  tibble(
    line = 1:length(.),
    data = .
  )


#the vlabels chunk is the only chunk where all lines have "x="
raw_data %>% 
  filter(str_detect(data, " x=")) -> vlabels

vlabels %>% 
  separate(col = data, into = c("index", "Name", "formatting"), sep = fixed("'")) %>% 
  separate(col = formatting, into = c("position", "color"), sep = ",") %>% 
  mutate(comma = ",") %>% 
  mutate(single_quote1 = "'") %>%  
  mutate(single_quote2 = "'") -> vlabels

read_tsv("Glottolog_lookup_table_Heti_edition.tsv") %>%
  dplyr::select(Name = Name_stripped_no_spaces, Language_ID = glottocode,Family_name = Family_name_isolates_distinct, Macroarea, Countries) -> Glottolog

vlabels %>% 
  left_join(Glottolog) -> vlabels

n <- length(unique(vlabels$Family_name))

vlabels$color_lc <- color_vector[as.factor(desc(vlabels$Family_name))]

vlabels <- vlabels %>% 
  mutate(font = "Arial-BOLD-9") %>% 
  mutate(color_background = "white") %>% 
  mutate(color_tip = color_lc) %>% 
  mutate(color_fg = color_lc)

#Abrozz'is Microbio

inputfile <- "output/Vizualisations/distance_based/Neighbournet/splitstree_saves/sahul_weighted_controlled_uncolored.nex"

outfile <- "output/Vizualisations/distance_based/Neighbournet/coloured/sahul_weighted_controlled_colored.nex"

tips <- as.character(vlabels$Name)
col <- vlabels$color_tip
fg <- vlabels$color_fg
lc <- vlabels$color_lc
s <- "o"
w <- 4
h <- 4
lk <- vlabels$color_background
edgecolor <- "gray15"
f <- vlabels$font

custom_nexus(nexus.file=inputfile, tips=tips, colors=col, vlabels=tips, f=f, lc=lc, lk=lk, fg= fg, s = s, w = w, h = h, edgecolor=edgecolor, outfile= outfile, plot = F)