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

# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKEY)

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
                    year = 2018,
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
acsDF <- bind_rows(lapply(years, function(x){
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
hhDF <- acsDF  %>%
  mutate(hh_size = case_when(
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
                      moe = sum(moe)), by = 'GEOID,Year,hh_size,summary_est,summary_moe']
plotDF <- plotDF[, prop := estimate/summary_est]
plotDF <- plotDF[, prop_moe := moe_prop(estimate, summary_est, 
                                    moe, summary_moe)]

#head(plotDF)

##############################
## Plot % hh size over time ##
##############################

# As a proportion of the total number of households
plotDF %>%
  mutate(
    hi = prop + prop_moe,
    lo = prop - prop_moe) %>%
  ggplot(aes(x = Year, y = prop, ymin = lo, ymax = hi, 
             group = hh_size)) +
  geom_line(aes(color = hh_size)) +
  geom_point(aes(color = hh_size)) +
  geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2005,2010,2015,2020)) +
  labs(y="%", color = "Household size",
       title = "Households in King County grouped by household size (as a %)") +
  guides(fill=FALSE)

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
       title = "Households in King County grouped by household size (as a %)") +
  guides(fill=FALSE)

###########
## TO-do ##
###########

# replicate this analysis at a more granular geographic level (census tracts)
# using ACS 5-year tabulated data

