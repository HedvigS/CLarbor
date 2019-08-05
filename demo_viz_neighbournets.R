library(tidyverse)
library(cluster)
library(glue)

glottolog <- read_tsv("Glottolog_lookup_table_Heti_edition.tsv") %>% 
  dplyr::select(Name = Name_stripped_no_spaces, glottocode, Family_name = Family_name_isolates_distinct, AUTOTYP_area)

Sahul_names <- colnames(read_tsv("output/distance_matrices/sahul_unweighted_dist.tsv")) %>% 
  as.data.frame() %>% 
  rename("glottocode" = ".") %>% 
  left_join(glottolog) %>% 
  dplyr::select(Name)
  
#unweighted

sahul_unweighted_dist_df <- read_tsv("output/distance_matrices/sahul_unweighted_dist.tsv")

langs_string <- Sahul_names %>% 
  mutate(index = 1:n() - 1) %>% 
  glue_data("[{index}] '{Name}'")

nrow(Sahul_names) -> taxa_number

rownames(sahul_unweighted_dist_df) <- NULL
colnames(sahul_unweighted_dist_df) <- NULL

knitr::kable(sahul_unweighted_dist_df) %>% 
  str_remove_all("\\||-|:") %>% 
  .[-c(1,2)] %>% 
  paste0(collapse = "\n") -> matrix_string

glue('
     #nexus
     BEGIN TAXA;
     DIMENSIONS NTAX = {taxa_number};
     TAXLABELS 
     {langs_string %>% paste0(collapse = "\n")};
     END; 
     
     [taxa]
     
     BEGIN DISTANCES;
     dimensions ntax = {taxa_number};
     format labels=no diagonal triangle=both;
     MATRIX 
     {matrix_string};
     END; [distances]
     ') -> sahul_unweighted_dist_nexus

write_lines(sahul_unweighted_dist_nexus, "output/Vizualisations/distance_based/Neighbournet/sahul_unweighted_uncolored.nex")

#weighted uncontrolled

sahul_weighted_uncontrolled_dist_df <- read_tsv("output/distance_matrices/Sahul_weighted_uncontrolled_dist.tsv")

langs_string <- Sahul_names %>% 
  mutate(index = 1:n() - 1) %>% 
  glue_data("[{index}] '{Name}'")

nrow(Sahul_names) -> taxa_number

rownames(sahul_weighted_uncontrolled_dist_df) <- NULL
colnames(sahul_weighted_uncontrolled_dist_df) <- NULL

knitr::kable(sahul_weighted_uncontrolled_dist_df) %>% 
  str_remove_all("\\||-|:") %>% 
  .[-c(1,2)] %>% 
  paste0(collapse = "\n") -> matrix_string

glue('
     #nexus
     BEGIN TAXA;
     DIMENSIONS NTAX = {taxa_number};
     TAXLABELS 
     {langs_string %>% paste0(collapse = "\n")};
     END; 
     
     [taxa]
     
     BEGIN DISTANCES;
     dimensions ntax = {taxa_number};
     format labels=no diagonal triangle=both;
     MATRIX 
     {matrix_string};
     END; [distances]
     ') -> sahul_weighted_uncontrolled_dist_nexus

write_lines(sahul_weighted_uncontrolled_dist_nexus, "output/Vizualisations/distance_based/Neighbournet/sahul_weighted_uncontrolled_uncolored.nex")





#weighted controlled

sahul_weighted_controlled_dist_df <- read_tsv("output/distance_matrices/Sahul_weighted_controlled_dist.tsv")

langs_string <- Sahul_names %>% 
  mutate(index = 1:n() - 1) %>% 
  glue_data("[{index}] '{Name}'")

nrow(Sahul_names) -> taxa_number

rownames(sahul_weighted_controlled_dist_df) <- NULL
colnames(sahul_weighted_controlled_dist_df) <- NULL

knitr::kable(sahul_weighted_controlled_dist_df) %>% 
  str_remove_all("\\||-|:") %>% 
  .[-c(1,2)] %>% 
  paste0(collapse = "\n") -> matrix_string

glue('
     #nexus
     BEGIN TAXA;
     DIMENSIONS NTAX = {taxa_number};
     TAXLABELS 
     {langs_string %>% paste0(collapse = "\n")};
     END; 
     
     [taxa]
     
     BEGIN DISTANCES;
     dimensions ntax = {taxa_number};
     format labels=no diagonal triangle=both;
     MATRIX 
     {matrix_string};
     END; [distances]
     ') -> sahul_weighted_controlled_dist_nexus

write_lines(sahul_weighted_controlled_dist_nexus, "output/Vizualisations/distance_based/Neighbournet/sahul_weighted_controlled_uncolored.nex")
