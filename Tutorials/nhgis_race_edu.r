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
###########################################
# -- load King County tract-level data -- #
###########################################
##PART 1
## create white only tract-level dataset for King county
tract_edu_white <- get_acs("tract",
                           table = "C15002A",
                           year = 2018,
                           state = "WA",
                           county = "King",
                           geometry = TRUE, 
                           survey = "acs5")

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

tract_edu_white_lab <- tract_edu_white %>%
  left_join(table_C15002A, by = c("variable" = "name")) 

####### PART 1A - Less than HS attainment data
# create less than HS attainment data set for White individuals
white_less_than_hs <- tract_edu_white_lab %>%
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
white_less_than_hs<- white_less_than_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

## do we need summary estimate still? not quite sure what this is for?
##white_less_than_hs <- white_less_than_hs[, summary_est := sum(estimate), by = 'GEOID']

##create White less than HS educational attainment plot for estimate only - how do i get 1 to go away?
white_less_than_hs_estimate_plot <- white_less_than_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Less than High School Education, White Alone",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

##create White less than HS educational attainment plot for CoV - fix labels
white_less_than_hs_CoV_plot <- white_less_than_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Less than High School Education, White Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

########
##PART 1B - White HS Grad/Equivalent Data
##create White HS grad/equivalent attainment dataset
white_hs <- tract_edu_white_lab %>%
  filter(hs_grad == 1) %>%
  group_by(GEOID, hs_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
white_hs<- white_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create White HS/equivalent educational attainment plot for estimate only - fix labels
white_hs_estimate_plot <- white_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate), size = .25) +
  labs(title = "High School Graduate or Equivalent, White Alone",
       subtitle = "Using Estimate Value",
       fill = "Estimate") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create White HS/equiv educational attainment plot for CoV - fix labels
white_hs_CoV_plot <- white_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV), size = .25) +
  labs(title = "High School Graduate or Equivalent, White Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

####################
### Part 1C: White Some College/AA edu attainment data
## create White some college/AA dataset for plots
White_some_college_AA <- tract_edu_white_lab %>%
  filter(some_college == 1) %>%
  group_by(GEOID, some_college) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
White_some_college_AA<- White_some_college_AA %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create White some college/AA educational attainment plot for estimate only - fix labels
white_SC_AA_estimate_plot <- White_some_college_AA %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate), size = .25) +
  facet_wrap(vars(some_college)) +
  labs(title = "Some College or Associate's Degree, White Alone",
       subtitle = "Using Estimate Value",
       fill = "Estimate") +
  scale_fill_viridis_c() + 
  theme_void()

##create White some college/AA educational attainment plot for CoV - fix labels
white_SC_AA_CoV_plot <- White_some_college_AA %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV), size = .25) +
  labs(title = "Some College or Associate's Degree, White Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

############################
### PART 1D: White Bachelor's Degree or Higher Data
#### create White Bachelor's + dataset
white_uni_degree <- tract_edu_white_lab %>%
  filter(college_grad == 1) %>%
  group_by(GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
white_uni_degree<- white_uni_degree %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create White bachelor's or higher educational attainment plot for estimate only - fix labels
white_uni_estimate_plot <- white_uni_degree %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate), size = .25) +
  labs(title = "Bachelor's Degree or Higher, White Alone",
       subtitle = "Using Estimate Value",
       fill = "Estimate") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create White bachelor's or higher educational attainment plot for CoV - fix labels
white_uni_CoV_plot <- white_uni_degree %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV), size = .25) +
  labs(title = "Bachelor's Degree or Higher, White Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()
##########################################
##Part 2:
# create Black/African American data set/plot
tract_edu_Black <- get_acs("tract",
                           table = "C15002B",
                           year = 2018,
                           state = "WA",
                           county = "King",
                           geometry = TRUE, 
                           survey = "acs5")

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

tract_edu_Black_lab <- tract_edu_Black %>%
  left_join(table_C15002B, by = c("variable" = "name")) 
table_C15002B <- var_df %>%
  filter(str_starts(name, "C15002B")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>%
  dplyr::select(name, short_label)

######## Part 2A - Black less than HS attainment
### create Black less than HS dataset 
Black_less_than_hs <- tract_edu_Black_lab %>%
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Black_less_than_hs<- Black_less_than_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Black less than HS educational attainment plot for estimate only - how do i get 1 to go away?
Black_less_than_hs_estimate_plot <- Black_less_than_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Less than High School Education, Black Alone",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Black less than HS educational attainment plot for CoV - fix labels
Black_less_than_hs_CoV_plot <- Black_less_than_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Less than High School Education, Black Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

###### Part 2B: Black HS grad/equivalent attainment 
Black_hs <- tract_edu_Black_lab %>%
  filter(hs_grad == 1) %>%
  group_by(GEOID, hs_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Black_hs<- Black_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Black HS grad/equiv educational attainment plot for estimate only - how do i get 1 to go away?
Black_hs_estimate_plot <- Black_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "High School Graduate or Equivalent Education, Black Alone",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Black less than HS educational attainment plot for CoV - fix labels
Black_hs_CoV_plot <- Black_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "High School Graduate or Equivalent Education, Black Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

### part 2C - Black some college/AA attainment 
Black_SC_AA <- tract_edu_Black_lab %>%
  filter(some_college == 1) %>%
  group_by(GEOID, some_college) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Black_SC_AA<- Black_SC_AA %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Black some college/AA plot for estimate only - how do i get 1 to go away?
Black_SC_AA_estimate_plot <- Black_SC_AA %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Some College or Associate's Degree, Black Alone",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Black some college/AA educational attainment plot for CoV - fix labels
Black_SC_AA_CoV_plot <- Black_SC_AA %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Some College or Associate's Degree, Black Alone Black Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

####Part 2D: Black Bachelor's or higher degree attainment

Black_college_grad <- tract_edu_Black_lab %>%
  filter(college_grad == 1) %>%
  group_by(GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Black_college_grad<- Black_college_grad %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Black college grad plot for estimate only - how do i get 1 to go away?
Black_college_grad_estimate_plot <- Black_college_grad %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Bachelor's Degree or Higher, Black Alone",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Black college grad educational attainment plot for CoV - fix labels
Black_college_grad_CoV_plot <- Black_college_grad %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Bachelor's Degree or Higher, Black Alone",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

####################### PART 3
###create data set and plot for American Indian/Alaska Native edu attainment
tract_edu_AIAN <- get_acs("tract",
                          table = "C15002C",
                          year = 2018,
                          state = "WA",
                          county = "King",
                          geometry = TRUE, 
                          survey = "acs5")

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

tract_edu_AIAN_lab <- tract_edu_AIAN %>%
  left_join(table_C15002C, by = c("variable" = "name")) 

######## Part 3A - AIAN less than HS attainment
### create AIAN less than HS dataset 
AIAN_less_than_hs <- tract_edu_AIAN_lab %>%
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
AIAN_less_than_hs<- AIAN_less_than_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create AIAN less than HS educational attainment plot for estimate only - how do i get 1 to go away?
AIAN_less_than_hs_estimate_plot <- AIAN_less_than_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Less than High School Education, American Indian/Alaska Native",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

##create AIAN less than HS educational attainment plot for CoV - fix labels
AIAN_less_than_hs_CoV_plot <- AIAN_less_than_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Less than High School Education, American Indian/Alaska Native",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

#####Part 3B - AIAN high school grad/equivalent
AIAN_hs <- tract_edu_AIAN_lab %>%
  filter(hs_grad == 1) %>%
  group_by(GEOID, hs_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
AIAN_hs<- AIAN_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create AIAN HS grad/equiv educational attainment plot for estimate only - how do i get 1 to go away?
AIAN_hs_estimate_plot <- AIAN_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "High School Graduate or Equivalent Education, American Indian/Alaska Native",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create AIAN less than HS educational attainment plot for CoV - fix labels
AIAN_hs_CoV_plot <- AIAN_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "High School Graduate or Equivalent Education, American Indian/Alaska Native",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

### part 3C - AIAN some college/AA attainment 
AIAN_SC_AA <- tract_edu_AIAN_lab %>%
  filter(some_college == 1) %>%
  group_by(GEOID, some_college) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
AIAN_SC_AA<- AIAN_SC_AA %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create AIAN some college/AA plot for estimate only 
AIAN_SC_AA_estimate_plot <- AIAN_SC_AA %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Some College or Associate's Degree, American Indian/Alaska Native",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

##create AIAN some college/AA educational attainment plot for CoV - fix labels
AIAN_SC_AA_CoV_plot <- AIAN_SC_AA %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Some College or Associate's Degree, American Indian/Alaska Native",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

####Part 3D: AIAN Bachelor's or higher degree attainment

AIAN_college_grad <- tract_edu_AIAN_lab %>%
  filter(college_grad == 1) %>%
  group_by(GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
AIAN_college_grad<- AIAN_college_grad %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create AIAN college grad plot for estimate only - how do i get 1 to go away?
AIAN_college_grad_estimate_plot <- AIAN_college_grad %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Bachelor's Degree or Higher, American Indian/Alaska Native",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create AIAN college grad educational attainment plot for CoV - fix labels
AIAN_college_grad_CoV_plot <- AIAN_college_grad %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Bachelor's Degree or Higher, American Indian/Alaska Native",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

#######PART 4: Asian edu attainment
tract_edu_Asian <- get_acs("tract",
                           table = "C15002D",
                           year = 2018,
                           state = "WA",
                           county = "King",
                           geometry = TRUE, 
                           survey = "acs5")

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

tract_edu_Asian_lab <- tract_edu_Asian %>%
  left_join(table_C15002D, by = c("variable" = "name")) 

######## Part 4A - Asian less than HS attainment
### create Asian less than HS dataset 
Asian_less_than_hs <- tract_edu_Asian_lab %>%
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Asian_less_than_hs<- Asian_less_than_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Asian less than HS educational attainment plot for estimate only - how do i get 1 to go away?
Asian_less_than_hs_estimate_plot <- Asian_less_than_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Less than High School Education, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Asian less than HS educational attainment plot for CoV - fix labels
Asian_less_than_hs_CoV_plot <- Asian_less_than_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Less than High School Education, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

#####Part 3B - Asian high school grad/equivalent
Asian_hs <- tract_edu_Asian_lab %>%
  filter(hs_grad == 1) %>%
  group_by(GEOID, hs_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Asian_hs<- Asian_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Asian HS grad/equiv educational attainment plot for estimate only - how do i get 1 to go away?
Asian_hs_estimate_plot <- Asian_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "High School Graduate or Equivalent Education, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Asian less than HS educational attainment plot for CoV - fix labels
Asian_hs_CoV_plot <- Asian_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "High School Graduate or Equivalent Education, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

### part 3C - Asian some college/AA attainment 
Asian_SC_AA <- tract_edu_Asian_lab %>%
  filter(some_college == 1) %>%
  group_by(GEOID, some_college) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Asian_SC_AA<- Asian_SC_AA %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Asian some college/AA plot for estimate only 
Asian_SC_AA_estimate_plot <- Asian_SC_AA %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Some College or Associate's Degree, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Asian some college/AA educational attainment plot for CoV - fix labels
Asian_SC_AA_CoV_plot <- Asian_SC_AA %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Some College or Associate's Degree, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

####Part 2D: Asian Bachelor's or higher degree attainment

Asian_college_grad <- tract_edu_Asian_lab %>%
  filter(college_grad == 1) %>%
  group_by(GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
Asian_college_grad<- Asian_college_grad %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create Asian college grad plot for estimate only - how do i get 1 to go away?
Asian_college_grad_estimate_plot <- Asian_college_grad %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Bachelor's Degree or Higher, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create Asian college grad educational attainment plot for CoV - fix labels
Asian_college_grad_CoV_plot <- Asian_college_grad %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Bachelor's Degree or Higher, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##### Part 5 - Native Hawaiian and Pacific Islander Educational Attainment
tract_edu_NHPI <- get_acs("tract",
                           table = "C15002E",
                           year = 2018,
                           state = "WA",
                           county = "King",
                           geometry = TRUE, 
                           survey = "acs5")

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

tract_edu_NHPI_lab <- tract_edu_NHPI %>%
  left_join(table_C15002E, by = c("variable" = "name")) 

######## Part 5A - NHPI less than HS attainment
### create NHPI less than HS dataset 
NHPI_less_than_hs <- tract_edu_NHPI_lab %>%
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
NHPI_less_than_hs<- NHPI_less_than_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create NHPI less than HS educational attainment plot for estimate only - how do i get 1 to go away?
NHPI_less_than_hs_estimate_plot <- NHPI_less_than_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Less than High School Education, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

##create NHPI less than HS educational attainment plot for CoV - fix labels
NHPI_less_than_hs_CoV_plot <- NHPI_less_than_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Less than High School Education, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

#####Part 5B - NHPI high school grad/equivalent
NHPI_hs <- tract_edu_NHPI_lab %>%
  filter(hs_grad == 1) %>%
  group_by(GEOID, hs_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
NHPI_hs<- NHPI_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create NHPI HS grad/equiv educational attainment plot for estimate only - how do i get 1 to go away?
NHPI_hs_estimate_plot <- NHPI_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "High School Graduate or Equivalent Education, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create NHPI HS grad/equivalent educational attainment plot for CoV - fix labels
NHPI_hs_CoV_plot <- NHPI_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "High School Graduate or Equivalent Education, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

### part 5C - NHPI some college/AA attainment 
NHPI_SC_AA <- tract_edu_NHPI_lab %>%
  filter(some_college == 1) %>%
  group_by(GEOID, some_college) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
NHPI_SC_AA<- NHPI_SC_AA %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create NHPI some college/AA plot for estimate only 
NHPI_SC_AA_estimate_plot <- NHPI_SC_AA %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Some College or Associate's Degree, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

##create NHPI some college/AA educational attainment plot for CoV - fix labels
NHPI_SC_AA_CoV_plot <- NHPI_SC_AA %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Some College or Associate's Degree, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

####Part 5D: NHPI Bachelor's or higher degree attainment

NHPI_college_grad <- tract_edu_NHPI_lab %>%
  filter(college_grad == 1) %>%
  group_by(GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
NHPI_college_grad<- NHPI_college_grad %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create NHPI college grad plot for estimate only - how do i get 1 to go away?
NHPI_college_grad_estimate_plot <- NHPI_college_grad %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Bachelor's Degree or Higher, Native Hawaiian/Pacific Islander",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create NHPI college grad educational attainment plot for CoV - fix labels
NHPI_college_grad_CoV_plot <- NHPI_college_grad %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Bachelor's Degree or Higher, Native Hawaiian/Pacific Islander",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

######### PART 6 - OTHER RACE EDUCATIONAL ATTAINMENT 
tract_edu_other <- get_acs("tract",
                          table = "C15002F",
                          year = 2018,
                          state = "WA",
                          county = "King",
                          geometry = TRUE, 
                          survey = "acs5")

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

tract_edu_other_lab <- tract_edu_other %>%
  left_join(table_C15002F, by = c("variable" = "name")) 

######## Part 6A - other less than HS attainment
### create other less than HS dataset 
other_less_than_hs <- tract_edu_other_lab %>%
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
other_less_than_hs<- other_less_than_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create other less than HS educational attainment plot for estimate only - how do i get 1 to go away?
other_less_than_hs_estimate_plot <- other_less_than_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Less than High School Education, Other Race",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

##create other less than HS educational attainment plot for CoV - fix labels
other_less_than_hs_CoV_plot <- other_less_than_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Less than High School Education, Other Race",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

#####Part 5B - other high school grad/equivalent
other_hs <- tract_edu_other_lab %>%
  filter(hs_grad == 1) %>%
  group_by(GEOID, hs_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
other_hs<- other_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create other HS grad/equiv educational attainment plot for estimate only - how do i get 1 to go away?
other_hs_estimate_plot <- other_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "High School Graduate or Equivalent Education, Other Race",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create other HS grad/equivalent educational attainment plot for CoV - fix labels
other_hs_CoV_plot <- other_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "High School Graduate or Equivalent Education, Other Race",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

### part 5C - other some college/AA attainment 
other_SC_AA <- tract_edu_other_lab %>%
  filter(some_college == 1) %>%
  group_by(GEOID, some_college) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
other_SC_AA<- other_SC_AA %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create other some college/AA plot for estimate only 
other_SC_AA_estimate_plot <- other_SC_AA %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Some College or Associate's Degree, Other Race",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

##create other some college/AA educational attainment plot for CoV - fix labels
other_SC_AA_CoV_plot <- other_SC_AA %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Some College or Associate's Degree, Other Race",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

####Part 5D: other Bachelor's or higher degree attainment

other_college_grad <- tract_edu_other_lab %>%
  filter(college_grad == 1) %>%
  group_by(GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
other_college_grad<- other_college_grad %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create other college grad plot for estimate only - how do i get 1 to go away?
other_college_grad_estimate_plot <- other_college_grad %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Bachelor's Degree or Higher, Other Race",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create other college grad educational attainment plot for CoV - fix labels
other_college_grad_CoV_plot <- other_college_grad %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Bachelor's Degree or Higher, Other Race",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

###### Part 7 - 2 or more races educational attainment
tract_edu_multiracial <- get_acs("tract",
                           table = "C15002G",
                           year = 2018,
                           state = "WA",
                           county = "King",
                           geometry = TRUE, 
                           survey = "acs5")

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

tract_edu_multiracial_lab <- tract_edu_multiracial %>%
  left_join(table_C15002G, by = c("variable" = "name")) 

######## Part 6A - multiracial less than HS attainment
### create multiracial less than HS dataset 
multiracial_less_than_hs <- tract_edu_multiracial_lab %>%
  filter(less_than_hs == 1) %>%
  group_by(GEOID, less_than_hs) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
multiracial_less_than_hs<- multiracial_less_than_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create multiracial less than HS educational attainment plot for estimate only - how do i get 1 to go away?
multiracial_less_than_hs_estimate_plot <- multiracial_less_than_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Less than High School Education, 2 or More Races",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

##create multiracial less than HS educational attainment plot for CoV - fix labels
multiracial_less_than_hs_CoV_plot <- multiracial_less_than_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Less than High School Education, 2 or More Races",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(less_than_hs)) +
  scale_fill_viridis_c() + 
  theme_void()

#####Part 5B - multiracial high school grad/equivalent
multiracial_hs <- tract_edu_multiracial_lab %>%
  filter(hs_grad == 1) %>%
  group_by(GEOID, hs_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
multiracial_hs<- multiracial_hs %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create multiracial HS grad/equiv educational attainment plot for estimate only - how do i get 1 to go away?
multiracial_hs_estimate_plot <- multiracial_hs %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "High School Graduate or Equivalent Education, 2 or More Races",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create multiracial HS grad/equivalent educational attainment plot for CoV - fix labels
multiracial_hs_CoV_plot <- multiracial_hs %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "High School Graduate or Equivalent Education, 2 or More Races",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(hs_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

### part 5C - multiracial some college/AA attainment 
multiracial_SC_AA <- tract_edu_multiracial_lab %>%
  filter(some_college == 1) %>%
  group_by(GEOID, some_college) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
multiracial_SC_AA<- multiracial_SC_AA %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create multiracial some college/AA plot for estimate only 
multiracial_SC_AA_estimate_plot <- multiracial_SC_AA %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Some College or Associate's Degree, 2 or More Races",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

##create multiracial some college/AA educational attainment plot for CoV - fix labels
multiracial_SC_AA_CoV_plot <- multiracial_SC_AA %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Some College or Associate's Degree, 2 or More Races",
       subtitle = "Coefficient of variation",
       fill = "Coefficient of variation") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

####Part 5D: multiracial Bachelor's or higher degree attainment

multiracial_college_grad <- tract_edu_multiracial_lab %>%
  filter(college_grad == 1) %>%
  group_by(GEOID, college_grad) %>% 
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

## calculate SE and CoV 
multiracial_college_grad<- multiracial_college_grad %>% 
  mutate(SE = moe/qnorm(.95)) %>% 
  mutate(CoV = SE/estimate)

##create multiracial college grad plot for estimate only - how do i get 1 to go away?
multiracial_college_grad_estimate_plot <- multiracial_college_grad %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  labs(title = "Bachelor's Degree or Higher, 2 or More Races",
       subtitle = "Using Estimate Value")+
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()

##create multiracial college grad educational attainment plot for CoV - fix labels
multiracial_college_grad_CoV_plot <- multiracial_college_grad %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = CoV)) +
  labs(title = "Bachelor's Degree or Higher, 2 or More Races",
       subtitle = "Coefficient of Variation",
       fill = "Coefficient of Variation") +
  facet_wrap(vars(college_grad)) +
  scale_fill_viridis_c() + 
  theme_void()
