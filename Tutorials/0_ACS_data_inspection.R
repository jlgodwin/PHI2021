########################################################
###
### First Analysis of ACS data
### Updated July 8th, 2021

rm(list=ls())
###################
# -- Libraries -- #
###################

library(tidycensus)
library(sf)
library(mapview)
library(data.table)
library(tidyverse)

# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)

#####################
# -- ACS 2015-19 -- #
#####################

# Download the Variable dictionary
var_df <- load_variables(2019, "acs5", cache = TRUE)

###########################################
# -- load King County tract-level data -- #
###########################################
tract_hh <- get_acs("tract",
                    table = "B11016",
                    summary_var = "B11016_001",
                    year = 2019,
                    state = "WA",
                    county = "King",
                    geometry = TRUE, 
                    survey = "acs5")

table_B11016 <- var_df %>%
  filter(str_starts(name, "B11016")) %>%
  mutate(short_label = str_split(label, "!!"), 
         short_label = map_chr(short_label, tail, 1)) %>%
  dplyr::select(name, short_label)

tract_hh_lab <- tract_hh %>%
  left_join(table_B11016, by = c("variable" = "name"))

# We discard the household type (family/non-family) for now
tract_hh_size <- tract_hh_lab %>%
  filter(!(short_label %in% c("Total:","Family households:","Nonfamily households:"))) %>%
  group_by(GEOID, short_label) %>%
  summarize(
    estimate = sum(estimate),
    moe = sum(moe)
  ) %>%
  data.table()

# We calculate the proportion of households in each household-size category
tract_hh_size <- tract_hh_size[, summary_est := sum(estimate), by = 'GEOID']
tract_hh_size <- tract_hh_size[, prop := estimate/summary_est]

tract_hh_size %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = prop), size = .25) +
  facet_wrap(vars(short_label)) +
  scale_fill_viridis_c() + 
  theme_void()

#######################
## Time series in KC ##
#######################

years <- 2005:2019

# as we focus on KC, acs1 year data are adequate 
#(acs 5-year tabulation needed for looking at things at a more granular level)

all_var_df <- bind_rows(lapply(years, function(x) {
  load_variables(x, "acs1", cache = TRUE) %>%
    mutate(YEAR = x)
}))

all_var_df %>%
  filter(name == "B11016_001")

# We loop through years of the ACS 1-year data and bind the rows together
acs1DF <- bind_rows(lapply(years, function(x){
  get_acs(
    "county",
    table = "B11016",
    summary_var = "B11016_001",
    state = "WA",
    county = "King",
    year = x,
    survey = "acs1",
    moe = 95,
    cache_table = TRUE) %>%
    mutate(Year = x)}))

# Again, we want to lump together household counts regardless of family types
hhDF <- acs1DF  %>%
  mutate(hh_size = case_when(
    variable == "B11016_001" ~ 0, # total
    variable == "B11016_003" ~ 2,
    variable == "B11016_004" ~ 3,
    variable == "B11016_005" ~ 4,
    variable == "B11016_006" ~ 5,
    variable == "B11016_007" ~ 6,
    variable == "B11016_008" ~ 7,
    variable == "B11016_010" ~ 1,
    variable == "B11016_011" ~ 2,
    variable == "B11016_012" ~ 3,
    variable == "B11016_013" ~ 4,
    variable == "B11016_014" ~ 5,
    variable == "B11016_015" ~ 6,
    variable == "B11016_016" ~ 7,
  ),
  hh_size = factor(hh_size)) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hh_size)) %>% 
  data.table()

# We calculate the proportion of households in each household-size category, and its associated margin of error
plotDF <- hhDF[, list(estimate = sum(estimate),
                      moe = moe_sum(moe,estimate)), by = 'GEOID,Year,hh_size']
plotDF <- plotDF %>%
  filter(hh_size == 0) %>%
  rename(summary_est=estimate,
         summary_moe = moe) %>%
  dplyr::select(-hh_size) %>%
  full_join(plotDF) %>%
  mutate(
    prop = estimate/summary_est,
    prop_moe = moe_prop(estimate, summary_est, 
                        moe, summary_moe)
  ) %>%
  filter(hh_size != 0) 

# Census data

census00DF <- get_decennial("county",
                          table = c("H013"),
                          summary_var = "H013001",
                          year = 2000,
                          state = "WA",
                          county = "King",
                          geometry = FALSE)
census10DF <- get_decennial("county",
                            table = c("H013"),
                            summary_var = "H013001",
                            year = 2010,
                            state = "WA",
                            county = "King",
                            geometry = FALSE)
censusDF <- census00DF %>%
  mutate(Year = 2000) %>%
  rbind(census10DF %>%
          mutate(Year = 2010)) %>%
  mutate(
    hh_size = case_when(
      # variable == "H013001" ~ 0, # total
      variable == "H013002" ~ 1,
      variable == "H013003" ~ 2,
      variable == "H013004" ~ 3,
      variable == "H013005" ~ 4,
      variable == "H013006" ~ 5,
      variable == "H013007" ~ 6,
      variable == "H013008" ~ 7
    ),
    hh_size = factor(hh_size)) %>%
  dplyr::select(-variable) %>%
  rename(estimate=value,
         summary_est = summary_value) %>%
  filter(!is.na(hh_size)) %>% 
  data.table()

censusDF[, prop := estimate/summary_est]

mergeDF <- censusDF %>%
  dplyr::select(-NAME)

plotDF <- rbind(plotDF, mergeDF, fill = T) %>%
  mutate(Source = ifelse(is.na(moe), "Census", "ACS"),
             hi = prop + prop_moe,
             lo = prop - prop_moe)
##############################
## Plot % hh size over time ##
##############################

# As a proportion of the total number of households
p1 <- plotDF %>%
  ggplot(aes(x = Year, y = prop, ymin = lo, ymax = hi, 
             group = hh_size, shape = Source)) +
  geom_line(data = plotDF %>% filter(Source == "ACS"),aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Households in King County grouped by household size (as a %)") +
  guides(fill=FALSE)

ggsave(filename = "/Users/adrienallorant/Documents/UW/PHI2021/output/KC_prop_time.png",
       height = 9, width = 6, plot = p1)
# Or as an absolute number

plotDF <- plotDF %>%
  mutate(
    hi = estimate+ moe,
    lo = estimate- moe
  )

p2 <- plotDF %>%
  ggplot(aes(x = Year, y = estimate, ymin = lo, ymax = hi, 
           group = hh_size, shape = Source)) +
  geom_line(data = plotDF %>% filter(Source == "ACS"),aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Households in King County grouped by household size (as a %)") +
  guides(fill=FALSE)

ggsave(filename = "/Users/adrienallorant/Documents/UW/PHI2021/output/KC_absnumber_time.png",
       height = 9, width = 6, plot = p2)

####################################
## How is it evolving in Seattle? ##
####################################
years <- 2009:2019
# We loop through years of the ACS 5-year data and bind the rows together
acs5DF <- bind_rows(lapply(years, function(x){
  get_acs(
    "tract",
    table = "B11016",
    summary_var = "B11016_001",
    state = "WA",
    county = "King",
    year = x,
    survey = "acs5",
    moe = 95,
    cache_table = TRUE) %>%
    mutate(Year = x)}))

hhDF <- acs5DF  %>%
  mutate(hh_size = case_when(
    variable == "B11016_001" ~ 0, # total
    variable == "B11016_003" ~ 2,
    variable == "B11016_004" ~ 3,
    variable == "B11016_005" ~ 4,
    variable == "B11016_006" ~ 5,
    variable == "B11016_007" ~ 6,
    variable == "B11016_008" ~ 7,
    variable == "B11016_010" ~ 1,
    variable == "B11016_011" ~ 2,
    variable == "B11016_012" ~ 3,
    variable == "B11016_013" ~ 4,
    variable == "B11016_014" ~ 5,
    variable == "B11016_015" ~ 6,
    variable == "B11016_016" ~ 7,
  ),
  hh_size = factor(hh_size)) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hh_size)) %>% 
  data.table()

# Census data

census00DF <- get_decennial("tract",
                            table = c("H013"),
                            summary_var = "H013001",
                            year = 2000,
                            state = "WA",
                            geometry = FALSE)
census10DF <- get_decennial("tract",
                            table = c("H013"),
                            summary_var = "H013001",
                            year = 2010,
                            state = "WA",
                            geometry = FALSE)
censusDF <- census00DF %>%
  mutate(Year = 2000) %>%
  rbind(census10DF %>%
          mutate(Year = 2010)) %>%
  mutate(
    hh_size = case_when(
      # variable == "H013001" ~ 0, # total
      variable == "H013002" ~ 1,
      variable == "H013003" ~ 2,
      variable == "H013004" ~ 3,
      variable == "H013005" ~ 4,
      variable == "H013006" ~ 5,
      variable == "H013007" ~ 6,
      variable == "H013008" ~ 7
    ),
    hh_size = factor(hh_size)) %>%
  dplyr::select(-variable) %>%
  rename(estimate=value,
         summary_est = summary_value) %>%
  filter(!is.na(hh_size)) %>% 
  data.table()

##################
## Seattle only ##

library(tigris)
options(tigris_use_cache = TRUE)

# load places in Washington
wa <- places("WA", year = 2019, class = "sf")
# get the water bodies of King County
king_water <- area_water("WA", "King", class = "sf") 
# filter to Seattle
seattle <- 
  wa %>%
  filter(NAME == "Seattle")

# we identify the census tracts included in Seattle
seattle_tracts <- 
  tract_hh_size %>%
  st_as_sf() %>%
  st_buffer(1e-5) %>%
  # cut the outline of Seattle
  st_intersection(seattle) %>%
  # remove water areas from the map
  st_difference(st_union(king_water)) %>% 
  dplyr::select(GEOID) %>%
  distinct()

# we subset from the overall KC tracts dataset, the one located in Seattle
seattleDF <- hhDF %>%
  filter(GEOID %in% seattle_tracts$GEOID)

seattleDFcensus <- censusDF %>%
  filter(GEOID %in% seattle_tracts$GEOID)

# We calculate the proportion of households in each household-size category,
# and its associated margin of error for Seattle in general - regardless of census tracts
seattleDF <- seattleDF[, list(estimate = sum(estimate),
                      moe = moe_sum(moe,estimate)), by = 'Year,hh_size']
seattleDFcensus <- seattleDFcensus[, list(estimate = sum(estimate)), by = 'Year,hh_size']

seattleDFcensus <- seattleDFcensus %>%
  left_join(
  seattleDFcensus %>%
  group_by(Year) %>%
  summarize(summary_est = sum(estimate))) %>%
  mutate(
    prop = estimate/summary_est
  )

plotDF <- seattleDF %>%
  filter(hh_size == 0) %>%
  rename(summary_est=estimate,
         summary_moe = moe) %>%
  dplyr::select(-hh_size) %>%
  full_join(seattleDF) %>%
  mutate(
    prop = estimate/summary_est,
    prop_moe = moe_prop(estimate, summary_est, 
                         moe, summary_moe),
    hi = prop + prop_moe,
    lo = prop - prop_moe
  ) %>%
  filter(hh_size != 0) 

plotDF <- rbind(plotDF, seattleDFcensus, fill = T) %>%
  mutate(Source = ifelse(is.na(moe), "Census","ACS"))
# As a proportion of the total number of households
p3 <- plotDF %>%
  ggplot(aes(x = Year, y = prop, ymin = lo, ymax = hi, 
             group = hh_size, shape = Source)) +
  geom_line(data = plotDF %>% filter(Source == "ACS"),aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Households in Seattle grouped by household size (as a %)") +
  guides(fill=FALSE)

ggsave(filename = "/Users/adrienallorant/Documents/UW/PHI2021/output/Seattle_prop_time.png",
       height = 9, width = 6, plot = p3)

plotDF <- plotDF %>%
  mutate(
    hi = estimate+ moe,
    lo = estimate- moe) 

p4 <- plotDF %>%
  ggplot(aes(x = Year, y = estimate, ymin = lo, ymax = hi, 
             group = hh_size, shape = Source)) +
  geom_line(data = plotDF %>% filter(Source == "ACS"),aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Households in Seattle grouped by household size (as a %)") +
  guides(fill=FALSE)

ggsave(filename = "/Users/adrienallorant/Documents/UW/PHI2021/output/Seattle_absnumber_time.png",
       height = 9, width = 6, plot = p4)
# Or as an absolute number

plotDF %>%
  mutate(
    hi = estimate+ moe,
    lo = estimate- moe) %>%
  ggplot(aes(x = Year, y = estimate, ymin = lo, ymax = hi, 
             group = hh_size)) +
  geom_line(aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Households in Seattle grouped by household size (as a %)") +
  guides(fill=FALSE)

#####################
## Average hh size ##
#####################

years <- 2009:2019
# We loop through years of the ACS 5-year data and bind the rows together
averagehhsize <- bind_rows(lapply(years, function(x){
  get_acs(
    "tract",
    var = "B25010_001",
    state = "WA",
    county = "King",
    year = x,
    survey = "acs5",
    moe = 95,
    cache_table = TRUE) %>%
    mutate(Year = x)}))

geom <- get_acs(
  "tract",
  var = "B25010_001",
  state = "WA",
  county = "King",
  year = 2019,
  survey = "acs5",
  moe = 95,
  geometry = TRUE) %>%
  dplyr::select(GEOID)


averagehhsize %>%
  left_join(geom) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate), size = .25) +
  facet_wrap(~ Year) +
  scale_fill_viridis_c() + 
  theme_void()

