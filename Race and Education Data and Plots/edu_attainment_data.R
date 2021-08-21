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

var_df <- load_variables(2019, "acs5", cache = TRUE)
var_df <- var_df[grepl("C15002", var_df$name), ] 
var_df$concept <- gsub("\\)", "", var_df$concept) 
var_df$concept <- gsub(paste0("SEX BY EDUCATIONAL ", 
                              "ATTAINMENT FOR THE ", 
                              "POPULATION 25 YEARS ", 
                              "AND OVER \\("), "", 
                       var_df$concept[grepl("C15002", var_df$name)])
### create data frame for white alone by educational attainment 
C15002A_2009 <- 
  get_acs("tract",
          table = "C15002A",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002A_2009$Year <- c(2009)

C15002A_2014 <- 
  get_acs("tract",
          table = "C15002A",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002A_2014$Year <- c(2014)

C15002A_2019 <- 
  get_acs("tract",
          table = "C15002A",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002A_2019$Year <- c(2019)

table_C15002A <- var_df %>%
  filter(str_starts(name, "C15002A")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002A$less_than_hs <- ifelse(table_C15002A$short_label == "Less than high school diploma", 1, 0)
table_C15002A$hs_grad <- ifelse(table_C15002A$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002A$some_college <- ifelse(table_C15002A$short_label == "Some college or associate's degree", 1, 0)
table_C15002A$college_grad <- ifelse(table_C15002A$short_label == "Bachelor's degree or higher", 1, 0)

C15002A <- bind_rows(C15002A_2009, C15002A_2014, C15002A_2019)

C15002A <- C15002A %>% 
  left_join(table_C15002A, by = c('variable' = 'name'))

### create data frame for Black/African American by edu attainment

C15002B_2009 <- 
  get_acs("tract",
          table = "C15002B",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002B_2009$Year <- c(2009)

C15002B_2014 <- 
  get_acs("tract",
          table = "C15002B",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002B_2014$Year <- c(2014)

C15002B_2019 <- 
  get_acs("tract",
          table = "C15002B",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002B_2019$Year <- c(2019)

table_C15002B <- var_df %>%
  filter(str_starts(name, "C15002B")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002B$less_than_hs <- ifelse(table_C15002B$short_label == "Less than high school diploma", 1, 0)
table_C15002B$hs_grad <- ifelse(table_C15002B$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002B$some_college <- ifelse(table_C15002B$short_label == "Some college or associate's degree", 1, 0)
table_C15002B$college_grad <- ifelse(table_C15002B$short_label == "Bachelor's degree or higher", 1, 0)

C15002B <- bind_rows(C15002B_2009, C15002B_2014, C15002B_2019)

C15002B <- C15002B %>% 
  left_join(table_C15002B, by = c('variable' = 'name'))

### create data frame for AIAN edu attainment
C15002C_2009 <- 
  get_acs("tract",
          table = "C15002C",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002C_2009$Year <- c(2009)

C15002C_2014 <- 
  get_acs("tract",
          table = "C15002C",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002C_2014$Year <- c(2014)

C15002C_2019 <- 
  get_acs("tract",
          table = "C15002C",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002C_2019$Year <- c(2019)

table_C15002C <- var_df %>%
  filter(str_starts(name, "C15002C")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002C$less_than_hs <- ifelse(table_C15002C$short_label == "Less than high school diploma", 1, 0)
table_C15002C$hs_grad <- ifelse(table_C15002C$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002C$some_college <- ifelse(table_C15002C$short_label == "Some college or associate's degree", 1, 0)
table_C15002C$college_grad <- ifelse(table_C15002C$short_label == "Bachelor's degree or higher", 1, 0)

C15002C <- bind_rows(C15002C_2009, C15002C_2014, C15002C_2019)

C15002C <- C15002C %>% 
  left_join(table_C15002C, by = c('variable' = 'name'))

### create data frame for Asian edu attainment 
C15002D_2009 <- 
  get_acs("tract",
          table = "C15002D",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002D_2009$Year <- c(2009)

C15002D_2014 <- 
  get_acs("tract",
          table = "C15002D",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002D_2014$Year <- c(2014)

C15002D_2019 <- 
  get_acs("tract",
          table = "C15002D",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002D_2019$Year <- c(2019)

table_C15002D <- var_df %>%
  filter(str_starts(name, "C15002D")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002D$less_than_hs <- ifelse(table_C15002D$short_label == "Less than high school diploma", 1, 0)
table_C15002D$hs_grad <- ifelse(table_C15002D$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002D$some_college <- ifelse(table_C15002D$short_label == "Some college or associate's degree", 1, 0)
table_C15002D$college_grad <- ifelse(table_C15002D$short_label == "Bachelor's degree or higher", 1, 0)

C15002D <- bind_rows(C15002D_2009, C15002D_2014, C15002D_2019)

C15002D <- C15002D %>% 
  left_join(table_C15002D, by = c('variable' = 'name'))

### create data frame for NHPI edu attainment 
C15002E_2009 <- 
  get_acs("tract",
          table = "C15002E",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002E_2009$Year <- c(2009)

C15002E_2014 <- 
  get_acs("tract",
          table = "C15002E",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002E_2014$Year <- c(2014)

C15002E_2019 <- 
  get_acs("tract",
          table = "C15002E",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002E_2019$Year <- c(2019)

table_C15002E <- var_df %>%
  filter(str_starts(name, "C15002E")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002E$less_than_hs <- ifelse(table_C15002E$short_label == "Less than high school diploma", 1, 0)
table_C15002E$hs_grad <- ifelse(table_C15002E$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002E$some_college <- ifelse(table_C15002E$short_label == "Some college or associate's degree", 1, 0)
table_C15002E$college_grad <- ifelse(table_C15002E$short_label == "Bachelor's degree or higher", 1, 0)

C15002E <- bind_rows(C15002E_2009, C15002E_2014, C15002E_2019)

C15002E <- C15002E %>% 
  left_join(table_C15002E, by = c('variable' = 'name'))

### create data frame for other race edu attainment
C15002F_2009 <- 
  get_acs("tract",
          table = "C15002F",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002F_2009$Year <- c(2009)

C15002F_2014 <- 
  get_acs("tract",
          table = "C15002F",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002F_2014$Year <- c(2014)

C15002F_2019 <- 
  get_acs("tract",
          table = "C15002F",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002F_2019$Year <- c(2019)

table_C15002F <- var_df %>%
  filter(str_starts(name, "C15002F")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002F$less_than_hs <- ifelse(table_C15002F$short_label == "Less than high school diploma", 1, 0)
table_C15002F$hs_grad <- ifelse(table_C15002F$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002F$some_college <- ifelse(table_C15002F$short_label == "Some college or associate's degree", 1, 0)
table_C15002F$college_grad <- ifelse(table_C15002F$short_label == "Bachelor's degree or higher", 1, 0)

C15002F <- bind_rows(C15002F_2009, C15002F_2014, C15002F_2019)

C15002F <- C15002F %>% 
  left_join(table_C15002F, by = c('variable' = 'name'))

### create data frame for two or more races edu attainment
C15002G_2009 <- 
  get_acs("tract",
          table = "C15002G",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002G_2009$Year <- c(2009)

C15002G_2014 <- 
  get_acs("tract",
          table = "C15002G",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002G_2014$Year <- c(2014)

C15002G_2019 <- 
  get_acs("tract",
          table = "C15002G",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002G_2019$Year <- c(2019)

table_C15002G <- var_df %>%
  filter(str_starts(name, "C15002G")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002G$less_than_hs <- ifelse(table_C15002G$short_label == "Less than high school diploma", 1, 0)
table_C15002G$hs_grad <- ifelse(table_C15002G$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002G$some_college <- ifelse(table_C15002G$short_label == "Some college or associate's degree", 1, 0)
table_C15002G$college_grad <- ifelse(table_C15002G$short_label == "Bachelor's degree or higher", 1, 0)

C15002G <- bind_rows(C15002G_2009, C15002G_2014, C15002G_2019)

C15002G <- C15002G %>% 
  left_join(table_C15002G, by = c('variable' = 'name'))

### create data frame for White, non-Hispanic edu attainment
C15002H_2009 <- 
  get_acs("tract",
          table = "C15002H",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002H_2009$Year <- c(2009)

C15002H_2014 <- 
  get_acs("tract",
          table = "C15002H",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002H_2014$Year <- c(2014)

C15002H_2019 <- 
  get_acs("tract",
          table = "C15002H",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002H_2019$Year <- c(2019)

table_C15002H <- var_df %>%
  filter(str_starts(name, "C15002H")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002H$less_than_hs <- ifelse(table_C15002H$short_label == "Less than high school diploma", 1, 0)
table_C15002H$hs_grad <- ifelse(table_C15002H$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002H$some_college <- ifelse(table_C15002H$short_label == "Some college or associate's degree", 1, 0)
table_C15002H$college_grad <- ifelse(table_C15002H$short_label == "Bachelor's degree or higher", 1, 0)

C15002H <- bind_rows(C15002H_2009, C15002H_2014, C15002H_2019)

C15002H <- C15002H %>% 
  left_join(table_C15002H, by = c('variable' = 'name'))

#### create data frame for Hispanic/Latino edu attainment
C15002I_2009 <- 
  get_acs("tract",
          table = "C15002I",
          year = 2009,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002I_2009$Year <- c(2009)

C15002I_2014 <- 
  get_acs("tract",
          table = "C15002I",
          year = 2014,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002I_2014$Year <- c(2014)

C15002I_2019 <- 
  get_acs("tract",
          table = "C15002I",
          year = 2019,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE)
C15002I_2019$Year <- c(2019)

table_C15002I <- var_df %>%
  filter(str_starts(name, "C15002I")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

table_C15002I$less_than_hs <- ifelse(table_C15002I$short_label == "Less than high school diploma", 1, 0)
table_C15002I$hs_grad <- ifelse(table_C15002I$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002I$some_college <- ifelse(table_C15002I$short_label == "Some college or associate's degree", 1, 0)
table_C15002I$college_grad <- ifelse(table_C15002I$short_label == "Bachelor's degree or higher", 1, 0)

C15002I <- bind_rows(C15002I_2009, C15002I_2014, C15002I_2019)

C15002I <- C15002I %>% 
  left_join(table_C15002I, by = c('variable' = 'name'))

### combine all races data into one data set to create a CSV of it
edu_attainment_data <- bind_rows(C15002A, C15002B, C15002C, C15002D, C15002E,
                                 C15002F, C15002G, C15002H, C15002I)
edu_attainment_data <- edu_attainment_data %>% 
  filter(short_label != "Total:") %>% 
  filter(short_label != "Male:") %>% 
  filter(short_label != "Female:")

write_csv(edu_attainment_data, "edu_attainment_data.csv")
