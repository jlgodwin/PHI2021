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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
  facet_wrap(vars(some_college)) +
  scale_fill_viridis_c() + 
  theme_void()

####Part 2D: AIAN Bachelor's or higher degree attainment

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
       subtitle = "Coefficient of Variance",
       fill = "Coefficient of Variance") +
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

tract_edu_Asian_lab <- tract_edu_Asian %>%
  left_join(table_C15002D, by = c("variable" = "name"))

# We discard the total type for now
tract_edu_Asian <- tract_edu_Asian_lab %>%
  filter(!(short_label %in% c("Total", "Female", "Male"))) %>%
  group_by(GEOID, short_label) %>%
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

tract_edu_Asian <- tract_edu_Asian[, summary_est := sum(estimate), by = 'GEOID']
tract_edu_Asian <- tract_edu_Asian[, prop := estimate/summary_est]

##create plot for Asian educational attainment
tract_edu_Asian %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = prop), size = .25) +
  facet_wrap(vars(short_label)) +
  scale_fill_viridis_c() + 
  theme_void()