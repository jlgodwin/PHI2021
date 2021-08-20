library(tidycensus)
library(tidyverse)
library(sf)
library(stringr)
library(ipumsr)
library(data.table)

myKey <- "493b01690c601ceeadbfc1bfc0089bae12b3f476"
census_api_key(myKey) 

###creating a data file for all educational attainment tidycensus data
###will be in format of CSV for ease of use in PHI visualization tool
years <- c(2010:2019)
var_df <- load_variables(2018, "acs5", cache = TRUE)
var_df <- var_df[grepl("C15002", var_df$name), ] 
var_df$concept <- gsub("\\)", "", var_df$concept) 
var_df$concept <- gsub(paste0("SEX BY EDUCATIONAL ", 
                              "ATTAINMENT FOR THE ", 
                              "POPULATION 25 YEARS ", 
                              "AND OVER \\("), "", 
                       var_df$concept[grepl("C15002", var_df$name)])

C15002A_acs <- lapply(years, function(x){
  as.data.frame()
  get_acs("tract",
          table = "C15002A",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})

table_C15002A <- var_df %>%
  filter(str_starts(name, "C15002A")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002A$less_than_hs <- ifelse(table_C15002A$short_label == "Less than high school diploma", 1, 0)
table_C15002A$hs_grad <- ifelse(table_C15002A$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002A$some_college <- ifelse(table_C15002A$short_label == "Some college or associate's degree", 1, 0)
table_C15002A$college_grad <- ifelse(table_C15002A$short_label == "Bachelor's degree or higher", 1, 0)

C15002A_acs <- lapply(years, function(x){
  get_acs("tract",
          table = "C15002A",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})