library(tidycensus)
library(tidyverse)
library(sf)
library(stringr)
library(ipumsr)
library(data.table)
myKey <- "493b01690c601ceeadbfc1bfc0089bae12b3f476"
census_api_key(myKey) 

#####################
# -- ACS 2015-19 -- #
#####################

# Download the Variable dictionary
var_df <- load_variables(2018, "acs5", cache = TRUE)
var_df <- var_df[grepl("C15002", var_df$name), ] 
var_df$concept <- gsub("\\)", "", var_df$concept) 
var_df$concept <- gsub(paste0("SEX BY EDUCATIONAL ", 
                              "ATTAINMENT FOR THE ", 
                              "POPULATION 25 YEARS ", 
                              "AND OVER \\("), "", 
                       var_df$concept[grepl("C15002", var_df$name)])

years <- c(2010:2019)

raw_inc_df <- lapply(years, function(x){
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

###########################################
# -- load King County tract-level data -- #
###########################################
##PART 1
## create white only tract-level dataset for King county

table_C15002A <- var_df %>%
  filter(str_starts(name, "C15002A")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

#create my own indicators
table_C15002A$less_than_hs <- ifelse(table_C15002A$short_label == "Less than high school diploma", 1, 0)
table_C15002A$hs_grad <- ifelse(table_C15002A$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002A$some_college <- ifelse(table_C15002A$short_label == "Some college or associate's degree", 1, 0)
table_C15002A$college_grad <- ifelse(table_C15002A$short_label == "Bachelor's degree or higher", 1, 0)


####### PART 1A - Less than HS attainment data

##### create estimate plot for less than HS, White alone
less_than_hs_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_white_estimate[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less than High School",
         subtitle = paste0("Estimate, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create White less than HS educational attainment plot for CoV - NEED TO FIX
less_than_hs_white_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_white_CoV[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Less than High School",
         subtitle = paste0("Coefficent of Variation, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('Less_than_hs_white_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_white_estimate[[as.character(year)]], 
               less_than_hs_white_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

########
##PART 1B - White HS Grad/Equivalent Data
hs_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_white_estimate[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent",
         subtitle = paste0("Estimate, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create White less than HS educational attainment plot for CoV - NEED TO FIX
hs_white_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_white_CoV[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "High School Graduate or Equivalent",
         subtitle = paste0("Coefficent of Variation, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('hs_white_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(hs_white_estimate[[as.character(year)]], 
               hs_white_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

####################
### Part 1C: White Some College/AA edu attainment data
SC_AA_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_white_estimate[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Estimate, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create White less than HS educational attainment plot for CoV - NEED TO FIX
SC_AA_white_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_white_CoV[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Coefficient of Variation, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('SC_AA_white_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(SC_AA_white_estimate[[as.character(year)]], 
               SC_AA_white_CoV[[as.character(year)]], ncol = 2)
}
dev.off()
############################
### PART 1D: White Bachelor's Degree or Higher Data
uni_white_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_white_estimate[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate Values, White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create White bachelor's or higher educational attainment plot for CoV 
uni_white_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_white_CoV[[as.character(year)]] <- raw_inc_df[[year.idx]] %>% 
    left_join(table_C15002A, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation,White alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('college_grad_white_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(uni_white_estimate[[as.character(year)]], 
               uni_white_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

##########################################
##Part 2:
# create Black/African American data set/plot
C15002B_acs <- lapply(years, function(x){
  get_acs("tract",
          table = "C15002B",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})

table_C15002B <- var_df %>%
  filter(str_starts(name, "C15002B")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

#create my own indicators
table_C15002B$less_than_hs <- ifelse(table_C15002B$short_label == "Less than high school diploma", 1, 0)
table_C15002B$hs_grad <- ifelse(table_C15002B$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002B$some_college <- ifelse(table_C15002B$short_label == "Some college or associate's degree", 1, 0)
table_C15002B$college_grad <- ifelse(table_C15002B$short_label == "Bachelor's degree or higher", 1, 0)

######## Part 2A - Black less than HS attainment
less_than_hs_black_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_black_estimate[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Estimate, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Black less than HS educational attainment plot for CoV 
less_than_hs_black_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_black_CoV[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Coefficient of Variation, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('less_than_hs_Black_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_black_estimate[[as.character(year)]], 
               less_than_hs_black_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

###### Part 2B: Black HS grad/equivalent attainment 
hs_black_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_black_estimate[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent",
         subtitle = paste0("Estimate, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Black HS/equivalent educational attainment plot for CoV 
hs_black_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_black_CoV[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "High School Graduate or Equivalent",
         subtitle = paste0("Coefficient of Variation, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('hs_grad_Black_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(hs_black_estimate[[as.character(year)]], 
               hs_black_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

### part 2C - Black some college/AA attainment 
SC_AA_black_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_black_estimate[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(some_college== 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Estimate, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Black HS/equivalent educational attainment plot for CoV 
SC_AA_black_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_black_CoV[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Coefficient of Variation, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('some_college_Black_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(SC_AA_black_estimate[[as.character(year)]], 
               SC_AA_black_CoV[[as.character(year)]], ncol = 2)
}
dev.off()


####Part 2D: Black Bachelor's or higher degree attainment
### part 2C - Black some college/AA attainment 
uni_black_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_black_estimate[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Black HS/equivalent educational attainment plot for CoV 
uni_black_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_black_CoV[[as.character(year)]] <- C15002B_acs[[year.idx]] %>% 
    left_join(table_C15002B, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation, Black alone, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('college_grad_Black_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(uni_black_estimate[[as.character(year)]], 
               uni_black_CoV[[as.character(year)]], ncol = 2)
}
dev.off()


####################### PART 3
###create data set and plot for American Indian/Alaska Native edu attainment
C15002C_acs <- lapply(years, function(x){
  get_acs("tract",
          table = "C15002C",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})

table_C15002C <- var_df %>%
  filter(str_starts(name, "C15002C")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>% 
  dplyr::select(name, short_label)

#create my own indicators
table_C15002C$less_than_hs <- ifelse(table_C15002C$short_label == "Less than high school diploma", 1, 0)
table_C15002C$hs_grad <- ifelse(table_C15002C$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002C$some_college <- ifelse(table_C15002C$short_label == "Some college or associate's degree", 1, 0)
table_C15002C$college_grad <- ifelse(table_C15002C$short_label == "Bachelor's degree or higher", 1, 0)


######## Part 3A - AIAN less than HS attainment

less_than_hs_AIAN_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_AIAN_estimate[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Estimate, American Indian/Alaska Native, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create AIAN less than HS educational attainment plot for CoV 
less_than_hs_AIAN_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_AIAN_CoV[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Coefficient of Variation, American Indian/Alaska Native
                           , ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('less_than_hs_AIAN_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_AIAN_estimate[[as.character(year)]], 
               less_than_hs_AIAN_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

#####Part 3B - AIAN high school grad/equivalent
hs_AIAN_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_AIAN_estimate[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent Education",
         subtitle = paste0("Estimate, American Indian/Alaska Native, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create AIAN less than HS educational attainment plot for CoV 
hs_AIAN_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_AIAN_CoV[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "High School Grduate or Equivalent Education",
         subtitle = paste0("Coefficient of Variation, American Indian/Alaska Native
                           , ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('hs_AIAN_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(hs_AIAN_estimate[[as.character(year)]], 
               hs_AIAN_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

### part 3C - AIAN some college/AA attainment 
SC_AA_AIAN_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_AIAN_estimate[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Estimate, American Indian/Alaska Native, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create AIAN less than HS educational attainment plot for CoV 
SC_AA_AIAN_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_AIAN_CoV[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Coefficient of Variation, American Indian/Alaska Native
                           , ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('some_college_AIAN_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(SC_AA_AIAN_estimate[[as.character(year)]], 
               SC_AA_AIAN_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

####Part 3D: AIAN Bachelor's or higher degree attainment
uni_AIAN_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_AIAN_estimate[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Bachelor's Degree or Higher, Estimate",
         subtitle = paste0("American Indian/Alaska Native, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create AIAN less than HS educational attainment plot for CoV 
uni_AIAN_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_AIAN_CoV[[as.character(year)]] <- C15002C_acs[[year.idx]] %>% 
    left_join(table_C15002C, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Bachelor's Degree or Higher, Coefficient of Variation",
         subtitle = paste0("American Indian/Alaska Native, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('college_grad_AIAN_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(uni_AIAN_estimate[[as.character(year)]], 
               uni_AIAN_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

#######PART 4: Asian edu attainment
C15002D_acs <- lapply(years, function(x){
  get_acs("tract",
          table = "C15002D",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})

table_C15002D <- var_df %>%
  filter(str_starts(name, "C15002D")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>%
  dplyr::select(name, short_label)

#create my own indicators
table_C15002D$less_than_hs <- ifelse(table_C15002D$short_label == "Less than high school diploma", 1, 0)
table_C15002D$hs_grad <- ifelse(table_C15002D$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002D$some_college <- ifelse(table_C15002D$short_label == "Some college or associate's degree", 1, 0)
table_C15002D$college_grad <- ifelse(table_C15002D$short_label == "Bachelor's degree or higher", 1, 0)

######## Part 4A - Asian less than HS attainment
less_than_hs_Asian_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_Asian_estimate[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Estimate, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
less_than_hs_Asian_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_Asian_CoV[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Coefficient of Variation, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('less_than_hs_Asian_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_Asian_estimate[[as.character(year)]], 
               less_than_hs_Asian_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

#####Part 4B - Asian high school grad/equivalent
hs_Asian_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_Asian_estimate[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent",
         subtitle = paste0("Estimate, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian HS grad educational attainment plot for CoV 
hs_Asian_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_Asian_CoV[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "High School Graduate or Equivalent",
         subtitle = paste0("Coefficient of Variation, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('hs_grad_Asian_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(hs_Asian_estimate[[as.character(year)]], 
               hs_Asian_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

### part 4C - Asian some college/AA attainment 
SC_AA_Asian_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_Asian_estimate[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Estimate, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
SC_AA_Asian_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_Asian_CoV[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Coefficient of Variation, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('some_college_Asian_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(SC_AA_Asian_estimate[[as.character(year)]], 
               SC_AA_Asian_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

####Part 4D: Asian Bachelor's or higher degree attainment
uni_Asian_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_Asian_estimate[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
uni_Asian_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_Asian_CoV[[as.character(year)]] <- C15002D_acs[[year.idx]] %>% 
    left_join(table_C15002D, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation, Asian, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('college_grad_Asian_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(uni_Asian_estimate[[as.character(year)]], 
               uni_Asian_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

##### Part 5 - Native Hawaiian and Pacific Islander Educational Attainment
C15002E_acs <- lapply(years, function(x){
  get_acs("tract",
          table = "C15002E",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})

table_C15002E <- var_df %>%
  filter(str_starts(name, "C15002E")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>%
  dplyr::select(name, short_label)

#create my own indicators
table_C15002E$less_than_hs <- ifelse(table_C15002E$short_label == "Less than high school diploma", 1, 0)
table_C15002E$hs_grad <- ifelse(table_C15002E$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002E$some_college <- ifelse(table_C15002E$short_label == "Some college or associate's degree", 1, 0)
table_C15002E$college_grad <- ifelse(table_C15002E$short_label == "Bachelor's degree or higher", 1, 0)

######## Part 5A - NHPI less than HS attainment
less_than_hs_NHPI_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_NHPI_estimate[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Estimate, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create NHPI less than HS educational attainment plot for CoV 
less_than_hs_NHPI_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_NHPI_CoV[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Coefficient of Variation, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('less_than_hs_NHPI_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_NHPI_estimate[[as.character(year)]], 
               less_than_hs_NHPI_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

#####Part 5B - NHPI high school grad/equivalent
hs_NHPI_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_NHPI_estimate[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent Education",
         subtitle = paste0("Estimate, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create NHPI less than HS educational attainment plot for CoV 
hs_NHPI_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_NHPI_CoV[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "High School Graduate or Equivalent Education",
         subtitle = paste0("Coefficient of Variation, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('hs_grad_NHPI_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(hs_NHPI_estimate[[as.character(year)]], 
               hs_than_hs_NHPI_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

### part 5C - NHPI some college/AA attainment 
SC_AA_NHPI_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_NHPI_estimate[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent Education",
         subtitle = paste0("Estimate, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create NHPI less than HS educational attainment plot for CoV 
SC_AA_NHPI_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_NHPI_CoV[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Coefficient of Variation, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('some_college_NHPI_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(SC_AA_NHPI_estimate[[as.character(year)]], 
               SC_AA_NHPI_CoV[[as.character(year)]], ncol = 2)
}
dev.off()


####Part 5D: NHPI Bachelor's or higher degree attainment
uni_NHPI_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_NHPI_estimate[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create NHPI less than HS educational attainment plot for CoV 
uni_NHPI_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_NHPI_CoV[[as.character(year)]] <- C15002E_acs[[year.idx]] %>% 
    left_join(table_C15002E, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation, NHPI, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('college_grad_NHPI_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(uni_NHPI_estimate[[as.character(year)]], 
               uni_NHPI_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

######### PART 6 - OTHER RACE EDUCATIONAL ATTAINMENT 
C15002F_acs <- lapply(years, function(x){
  get_acs("tract",
          table = "C15002F",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})

table_C15002F <- var_df %>%
  filter(str_starts(name, "C15002F")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>%
  dplyr::select(name, short_label)

#create my own indicators
table_C15002F$less_than_hs <- ifelse(table_C15002F$short_label == "Less than high school diploma", 1, 0)
table_C15002F$hs_grad <- ifelse(table_C15002F$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002F$some_college <- ifelse(table_C15002F$short_label == "Some college or associate's degree", 1, 0)
table_C15002F$college_grad <- ifelse(table_C15002F$short_label == "Bachelor's degree or higher", 1, 0)

######## Part 6A - other less than HS attainment
less_than_hs_other_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_other_estimate[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Estimate, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
less_than_hs_other_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_other_CoV[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Coefficient of Variation, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('less_than_hs_other_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_other_estimate[[as.character(year)]], 
               less_than_hs_other_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

#####Part 5B - other high school grad/equivalent
hs_other_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_other_estimate[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent Education",
         subtitle = paste0("Estimate, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
hs_other_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_other_CoV[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "High School Graduate or Equivalent Education",
         subtitle = paste0("Coefficient of Variation, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('hs_other_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(hs_other_estimate[[as.character(year)]], 
               hs_other_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

### part 5C - other some college/AA attainment 

SC_AA_other_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_other_estimate[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Estimate, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
SC_AA_other_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_other_CoV[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(some_college== 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Coefficient of Variation, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('some_college_other_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(SC_AA_other_estimate[[as.character(year)]], 
               SC_AA_other_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

####Part 5D: other Bachelor's or higher degree attainment
uni_other_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_other_estimate[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
uni_other_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_other_CoV[[as.character(year)]] <- C15002F_acs[[year.idx]] %>% 
    left_join(table_C15002F, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation, Other Race, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('college_grad_other_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(uni_other_estimate[[as.character(year)]], 
               uni_other_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

###### Part 7 - 2 or more races educational attainment
C15002G_acs <- lapply(years, function(x){
  get_acs("tract",
          table = "C15002G",
          year = x,
          state = "WA",
          county = "King",
          geometry = TRUE, 
          survey = "acs5",
          moe = 90,
          cache_table = TRUE) %>%
    mutate(Year = x)})

table_C15002G <- var_df %>%
  filter(str_starts(name, "C15002G")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>%
  dplyr::select(name, short_label)

#create my own indicators
table_C15002G$less_than_hs <- ifelse(table_C15002G$short_label == "Less than high school diploma", 1, 0)
table_C15002G$hs_grad <- ifelse(table_C15002G$short_label == "High school graduate (includes equivalency)", 1, 0)
table_C15002G$some_college <- ifelse(table_C15002G$short_label == "Some college or associate's degree", 1, 0)
table_C15002G$college_grad <- ifelse(table_C15002G$short_label == "Bachelor's degree or higher", 1, 0)

####Part 7A - Less than High School multiracial attainment 
less_than_hs_multiracial_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_multiracial_estimate[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Less than High School Education",
         subtitle = paste0("Estimate, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create Asian less than HS educational attainment plot for CoV 
less_than_hs_multiracial_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  less_than_hs_multiracial_CoV[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(less_than_hs == 1) %>%
    group_by(GEOID, less_than_hs) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Less Than High School Education",
         subtitle = paste0("Coefficient of Variation, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('less_than_hs_multiracial_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(less_than_hs_multiracial_estimate[[as.character(year)]], 
               less_than_hs_multiracial_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

### part 7B - multiracial some college/AA attainment 
hs_multiracial_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_multiracial_estimate[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "High School Graduate or Equivalent Education",
         subtitle = paste0("Estimate, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create HS grad educational attainment plot for CoV 
hs_multiracial_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  hs_multiracial_CoV[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(hs_grad == 1) %>%
    group_by(GEOID, hs_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "High School Graduate or Equivalent",
         subtitle = paste0("Coefficient of Variation, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('hs_multiracial_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(hs_multiracial_estimate[[as.character(year)]], 
               hs_multiracial_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

#### Part 7C - High school grad/equiv attainment - multiracial 
SC_AA_multiracial_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_multiracial_estimate[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Estimate, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create some college grad educational attainment plot for CoV 
SC_AA_multiracial_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  SC_AA_multiracial_CoV[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(some_college == 1) %>%
    group_by(GEOID, some_college) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Some College or Associate's Degree",
         subtitle = paste0("Coefficient of Variation, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('some_college_multiracial_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(SC_AA_multiracial_estimate[[as.character(year)]], 
               SC_AA_multiracial_CoV[[as.character(year)]], ncol = 2)
}
dev.off()

####Part 7D: multiracial Bachelor's or higher degree attainment
uni_multiracial_estimate <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_multiracial_estimate[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = estimate)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Estimate, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

##create college grad educational attainment plot for CoV 
uni_multiracial_CoV <- list()
for(year in years){
  year.idx <- match(year, years)
  uni_multiracial_CoV[[as.character(year)]] <- C15002G_acs[[year.idx]] %>% 
    left_join(table_C15002G, by = c("variable" = "name"))%>%
    filter(college_grad == 1) %>%
    group_by(GEOID, college_grad) %>% 
    summarize(
      estimate = sum(estimate),
      moe = sum(moe)
    ) %>%
    data.table()%>% 
    mutate(SE = moe/qnorm(.95)) %>% 
    mutate(CoV = SE/estimate) %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf(aes(fill = CoV)) +
    labs(title = "Bachelor's Degree or Higher",
         subtitle = paste0("Coefficient of Variation, Two or More Races, ", year)) +
    scale_fill_viridis_c() + 
    theme_void()
}

pdf('college_grad_multiracial_over_time.pdf', 
    width = 8, height = 4)
for(year in years){
  grid.arrange(uni_multiracial_estimate[[as.character(year)]], 
               uni_multiracial_CoV[[as.character(year)]], ncol = 2)
}
dev.off()