source("package_dependencies.R")

data_fn <-list.files("data" , full.names = T, pattern = "wide.tsv")

data <- read_tsv(data_fn) %>% 
  rename("ID" = 1)

#reading in the external variables, this can be whatever you like as long as the identifier that ties it to the data is in the first
external_variables_fn <- "data/Glottolog_lookup_table_Heti_edition.tsv" 

#There are a few instances of variables that have only missing values for all observations. Here we exclude them

col_na_means <- data %>% 
  as.matrix() %>% 
  is.na() %>% 
  colMeans() 

cols_to_keep <- tibble(Feature_ID = colnames(data),
       feature_na_prop = col_na_means) %>% 
  filter(Feature_ID != "ID") %>% 
  arrange(-feature_na_prop) %>% 
  distinct(Feature_ID, .keep_all = T) %>% 
  filter(feature_na_prop < 0.5) %>% 
  dplyr::select(Feature_ID) %>% 
  as.matrix() %>% 
  as.vector()

#Some languages have a large amount of missing values. Let's exclude those that have less than 50% filled.

data %>% 
  column_to_rownames("ID") %>% 
  apply(1, function(x) mean(is.na(x))) -> data$na_prop

data_few_missing_values <- data %>% 
  arrange(na_prop) %>% 
  filter(na_prop < 0.5) %>% 
  dplyr::select(-na_prop) %>% 
  dplyr::select(ID, cols_to_keep)

##Missing values per feature, for later reference

Feat_missing_stats <- tibble(Feature_ID = colnames(data_few_missing_values[,-1]),
  na_prop_feat = colMeans(is.na(data_few_missing_values[,-1]), na.rm = T),
  num_non_na = apply(data_few_missing_values[,-1], 2, function(x){length(table(x))}))

Feat_missing_stats$Feature_ID <- fct_reorder(Feat_missing_stats$Feature_ID, Feat_missing_stats$na_prop_feat)

#ggplot(data = Feat_missing_stats) +
#  geom_line(aes(x= Feature_ID, y = na_prop_feat, group = 1))

##Write the tidied dataframe to file
write_tsv(data_few_missing_values, "data/tidied/value_df.tsv")

#Write a df for the meta-data that will become the external variables.
obs_remaining <- data_few_missing_values %>% 
  dplyr::select(ID)

read_tsv(external_variables_fn) %>%
  rename("ID" = 1) %>% 
  dplyr::select(ID,
                Family_name = Top_genetic_unit_ID_isolates_distinct,
                AUTOTYP_area) %>%
  right_join(obs_remaining) %>%
  write_tsv("data/tidied/external_variables_df.tsv")
