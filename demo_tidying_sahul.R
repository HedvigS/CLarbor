source("package_dependencies.R")

sahul_data <- read_tsv("demo_data/Sahul_structure_wide.tsv") 

#There are a few instances of variables that have only missing values for all observations. Here we exclude them

sahul_abbrev <- read_tsv("demo_data/Sahul_ID_desc.tsv") %>% 
    mutate(Feature_ID = paste0("SAHUL", str_pad(Feature_ID, 3, "left", "0")))

col_na_means <- sahul_data %>% 
  as.matrix() %>% 
  is.na() %>% 
  colMeans() 

Sahul_cols_to_keep <- tibble(Feature_ID = colnames(sahul_data),
       feature_na_prop = col_na_means) %>% 
  filter(Feature_ID != "Glottocode") %>% 
  left_join(sahul_abbrev) %>% 
  arrange(-feature_na_prop) %>% 
  distinct(Feature, .keep_all = T) %>% 
  filter(feature_na_prop < 0.5) %>% 
  dplyr::select(Feature_ID) %>% 
  as.matrix() %>% 
  as.vector()

#Some languages have a large amount of missing values. Let's exclude those that have less than 50% filled.

sahul_data %>% 
  column_to_rownames("Glottocode") %>% 
  apply(1, function(x) mean(is.na(x))) -> sahul_data$na_prop

sahul_data_few_missing_values <- sahul_data %>% 
  arrange(na_prop) %>% 
  filter(na_prop < 0.5) %>% 
  dplyr::select(-na_prop) %>% 
  dplyr::select(Glottocode, Sahul_cols_to_keep)

##Missing values per feature, for later reference

Feat_missing_stats <- tibble(Feature_ID = colnames(sahul_data_few_missing_values[,-1]),
  na_prop_feat = colMeans(is.na(sahul_data_few_missing_values[,-1]), na.rm = T),
  num_non_na = apply(sahul_data_few_missing_values[,-1], 2, function(x){length(table(x))}))

Feat_missing_stats$Feature_ID <- fct_reorder(Feat_missing_stats$Feature_ID, Feat_missing_stats$na_prop_feat)

ggplot(data = Feat_missing_stats) +
  geom_line(aes(x= Feature_ID, y = na_prop_feat, group = 1))

##Write the tidied dataframe to file
write_tsv(sahul_data_few_missing_values, "demo_data/tidied/Sahul_value_df.tsv")

#Write a df for the meta-data that will become the external variables.
sahul_IDS <- sahul_data_few_missing_values %>% 
  dplyr::select(Glottocode)

read_tsv("demo_data/Glottolog_all_languoids_Heti_enhanced.tsv") %>%
  dplyr::select(Glottocode,
                Family_name,
                AUTOTYP_area) %>%
  right_join(sahul_IDS) %>%
  write_tsv("demo_data/tidied/Sahul_external_variables_df.tsv")
