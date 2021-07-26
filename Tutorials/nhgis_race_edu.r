library(tidycensus)
library(tidyverse)
library(sf)
library(stringr)
library(ipumsr)

myKey <- "493b01690c601ceeadbfc1bfc0089bae12b3f476"
census_api_key(myKey) 

#####################
# -- ACS 2015-19 -- #
#####################

# Download the Variable dictionary
var_df <- load_variables(2018, "acs5", cache = TRUE)

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

tract_edu_white_lab <- tract_edu_white %>%
  left_join(table_C15002A, by = c("variable" = "name"))

# We discard the total type for now - for White only data
tract_edu_white <- tract_edu_white_lab %>%
  filter(!(short_label %in% c("Total", "Female", "Male"))) %>%
  group_by(GEOID, short_label) %>%
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

tract_edu_white <- tract_edu_white[, summary_est := sum(estimate), by = 'GEOID']
tract_edu_white <- tract_edu_white[, prop := estimate/summary_est]
##create White educational attainment plot
tract_edu_white %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = prop), size = .25) +
  facet_wrap(vars(short_label)) +
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

tract_edu_Black_lab <- tract_edu_Black %>%
  left_join(table_C15002B, by = c("variable" = "name"))

# We discard the total type for now
tract_edu_Black <- tract_edu_Black_lab %>%
  filter(!(short_label %in% c("Total", "Female", "Male"))) %>%
  group_by(GEOID, short_label) %>%
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

tract_edu_Black <- tract_edu_Black[, summary_est := sum(estimate), by = 'GEOID']
tract_edu_Black <- tract_edu_Black[, prop := estimate/summary_est]

##create plot for Black/African American educational attainment
tract_edu_Black %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = prop), size = .25) +
  facet_wrap(vars(short_label)) +
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

tract_edu_AIAN_lab <- tract_edu_AIAN %>%
  left_join(table_C15002C, by = c("variable" = "name"))

# We discard the total type for now
tract_edu_AIAN <- tract_edu_AIAN_lab %>%
  filter(!(short_label %in% c("Total", "Female", "Male"))) %>%
  group_by(GEOID, short_label) %>%
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

tract_edu_AIAN <- tract_edu_AIAN[, summary_est := sum(estimate), by = 'GEOID']
tract_edu_AIAN <- tract_edu_AIAN[, prop := estimate/summary_est]

##create plot for American Indian/Alaska Native educational attainment
tract_edu_AIAN %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = prop), size = .25) +
  facet_wrap(vars(short_label)) +
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
  left_join(table_C15002B, by = c("variable" = "name"))

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

