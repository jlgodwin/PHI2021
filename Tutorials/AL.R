# Libraries ####

library(tidycensus)
library(tidyverse)
library(sf)
library(stringr)
library(ipumsr)
library(gridExtra)

## tidycensus key ####
census_api_key(myKey) 


# ACS 2015-2019
### Variable Dictionary ####
var_df <- load_variables(2018, "acs5", cache = TRUE)

al <- get_acs("county",
              table = "B01001",
              year = 2018,
              state = "AL",
              geometry = TRUE, 
              survey = "acs5")


al_total <- al %>%
  filter(variable == "B01001_001") %>%
  left_join(var_df,
            by = c("variable" = "name"))
al_total$label <- gsub("Estimate!!", "", al_total$label)


gg_total <- al_total %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate), size = .25) +
  facet_wrap(vars(label)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Total",
                       breaks = c(0, 10000, 50000,
                                  100000, 250000, 500000, 675000),
                       labels =   c("0", "10K","50K",
                                    "100K", "250K", "500K", "675K")) + 
  ggtitle("All ages") +
  theme_void()

al_adult <- al[(al$variable %in%
                  c(paste0("B01001_0", c(10:25, 29:49)),
                    paste0("B01001_00", c(5:9)))),] %>%
  left_join(var_df,
            by = c("variable" = "name"))
al_adult$sex <- ifelse(grepl("Male", al_adult$label),
                       "Male","Female")

al_adult$label <- gsub("Estimate!!Total!!Male!!", "", al_adult$label)
al_adult$label <- gsub("Estimate!!Total!!Female!!", "", al_adult$label)

al_adult_nums <- al_adult %>%
  as.data.frame() %>%
  select(-c("geometry",
            "variable")) %>%
  pivot_wider(names_from = "sex",
              values_from = c("estimate", "moe")) %>%
  mutate(estimate_Total = estimate_Male + estimate_Female,
         moe_Total = moe_Male + moe_Female)

al_adult_total <- al_adult_nums %>%
  group_by(GEOID) %>%
  summarise(estimate_Male = sum(estimate_Male),
            estimate_Female = sum(estimate_Female),
            estimate_Total = sum(estimate_Total),
            moe_Male = sum(moe_Male),
            moe_Female = sum(moe_Female),
            moe_Total = sum(moe_Total))

al_total <- al_total %>%
  left_join(al_adult_total) 

gg_adult <- al_total %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = estimate_Total/estimate), size = .25) +
  facet_wrap(vars(label)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) +
  ggtitle("Proportion above age 10") +
  theme_void()


gg_child <- al_total %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = 1- (estimate_Total/estimate)), size = .25) +
  facet_wrap(vars(label)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) +
  ggtitle("Proportion below age 10") +
  theme_void()

sum(al_adult_total$estimate_Total)/
  sum(al_total$estimate)


## 
al_map <- get_estimates("county",
                        product = "population",
                        geometry = TRUE,
                        state = "AL") %>%
  filter(variable == "POP")

al_census <- get_estimates("county",
                           product = "characteristics",
                           breakdown = c("AGEGROUP"),
                           breakdown_labels = TRUE,
                           geometry = FALSE,
                           state = "AL")
al_census_race <-  get_estimates("county",
                                 product = "characteristics",
                                 breakdown = c("AGEGROUP", "RACE"),
                                 breakdown_labels = TRUE,
                                 geometry = FALSE,
                                 state = "AL")

children.ages <- c("Age 0 to 4 years",
                   "Age 5 to 9 years",
                   "Age 10 to 14 years")
adults.ages <- c(paste("Age",
                       seq(15, 80, 5),
                       "to",
                       seq(19,84,5),
                       "years", sep = " "),
                 "Age 85 years and older")

al_census_race$all.ages.id <- ifelse(al_census_race$AGEGROUP == "All ages", 1, 0)
al_census$all.ages.id <- ifelse(al_census$AGEGROUP == "All ages", 1, 0)
al_census_race$child.id <-  ifelse(al_census_race$AGEGROUP %in% children.ages, 1, 0)
al_census$child.id <- ifelse(al_census$AGEGROUP %in% children.ages, 1, 0)
al_census_race$adult.id <- ifelse(al_census_race$AGEGROUP %in% adults.ages, 1, 0)
al_census$adult.id <- ifelse(al_census$AGEGROUP %in% adults.ages, 1, 0)
al_census_race$white.id <- ifelse(al_census_race$RACE == "White alone", 1, 0)
al_census_race$black.id <- ifelse(al_census_race$RACE %in% c("Black alone"),
                                  1,0)
al_census_race$black2.id <- ifelse(al_census_race$RACE %in% c("Black alone or in combination"),
                                   1,0)


al_vaxx <- read.csv("ALVax.csv") 
al_vaxx <- al_vaxx[!(al_vaxx$County %in% c("Alabama", "Unknown")),]
al_vaxx$County <- paste0(al_vaxx$County, " County, Alabama")

al_census_allrace <- al_census %>%
  group_by(GEOID) %>%
  mutate(total = value*all.ages.id,
         total_child = value*child.id,
         total_adult = value*adult.id) %>%
  group_by(GEOID, NAME) %>%
  summarise(total = sum(total),
            total_child = sum(total_child),
            total_adult = sum(total_adult))

al_census_white <- al_census_race %>%
  group_by(GEOID) %>%
  mutate(total = value*all.ages.id*white.id,
         total_child = value*child.id*white.id,
         total_adult = value*adult.id*white.id) %>%
  group_by(GEOID, NAME) %>%
  summarise(total = sum(total),
            total_child = sum(total_child),
            total_adult = sum(total_adult)) %>%
  left_join(al_census_allrace,
            by = c("GEOID"= "GEOID",
                   "NAME" = "NAME"),
            suffix = c("", "_all"))


al_census_black <- al_census_race %>%
  group_by(GEOID) %>%
  mutate(total = value*all.ages.id*black.id,
         total_child = value*child.id*black.id,
         total_adult = value*adult.id*black.id) %>%
  group_by(GEOID, NAME) %>%
  summarise(total = sum(total),
            total_child = sum(total_child),
            total_adult = sum(total_adult)) %>%
  left_join(al_census_allrace,
            by = c("GEOID"= "GEOID",
                   "NAME" = "NAME"),
            suffix = c("", "_all"))

al_census_black2 <- al_census_race %>%
  group_by(GEOID) %>%
  mutate(total = value*all.ages.id*black2.id,
         total_child = value*child.id*black2.id,
         total_adult = value*adult.id*black2.id) %>%
  group_by(GEOID, NAME) %>%
  summarise(total = sum(total),
            total_child = sum(total_child),
            total_adult = sum(total_adult)) %>%
  left_join(al_census_allrace,
            by = c("GEOID"= "GEOID",
                   "NAME" = "NAME"),
            suffix = c("", "_all"))


gg_total <- al_map %>%
  left_join(al_census_allrace) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Total",
                       breaks = c(0, 10000, 50000,
                                  100000, 250000, 500000, 675000),
                       labels =   c("0", "10K","50K",
                                    "100K", "250K", "500K", "675K")) + 
  ggtitle("Total Population") +
  theme_void()

gg_total_white <- al_map %>%
  left_join(al_census_white) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Total",
                       breaks = c(0, 10000, 50000,
                                  100000, 250000, 500000, 675000),
                       labels =   c("0", "10K","50K",
                                    "100K", "250K", "500K", "675K")) + 
  ggtitle("Total Population: White") +
  theme_void()


gg_total_black <- al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Total",
                       breaks = c(0, 10000, 50000,
                                  100000, 250000, 500000, 675000),
                       labels =   c("0", "10K","50K",
                                    "100K", "250K", "500K", "675K")) + 
  ggtitle("Total Population: Black") +
  theme_void()

gg_total_black2 <- al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Total",
                       breaks = c(0, 10000, 50000,
                                  100000, 250000, 500000, 675000),
                       labels =   c("0", "10K","50K",
                                    "100K", "250K", "500K", "675K")) + 
  ggtitle("Total Population: Black + Black, multiracial") +
  theme_void()

gg_prop_white <- al_map %>%
  left_join(al_census_white) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) +
  ggtitle("Proportion White") +
  theme_void()

gg_prop_black <- al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) +
  ggtitle("Proportion Black") +
  theme_void()

gg_prop_black2 <- al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) +
  ggtitle("Proportion Black + Black, multiracial") +
  theme_void()

gg_prop_adult <- al_map %>%
  left_join(al_census_allrace) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_adult/total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) +
  ggtitle("Proportion Adult") +
  theme_void()


gg_prop_adult_white <- al_map %>%
  left_join(al_census_white) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_adult/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) +
  ggtitle("Proportion Adult: White") +
  theme_void()


gg_prop_adult_black <- al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_adult/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) +
  ggtitle("Proportion Adult: Black") +
  theme_void()



gg_prop_adult_black2 <- al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_adult/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) +
  ggtitle("Proportion Adult: Black + Black, multiracial") +
  theme_void()


gg_prop_child <- al_map %>%
  left_join(al_census_allrace) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_child/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion Children") +
  theme_void()


gg_prop_child_white <- al_map %>%
  left_join(al_census_white) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_child/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion Children: White") +
  theme_void()

gg_prop_child_black <- al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_child/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion Children: Black") +
  theme_void()

gg_prop_child_black2 <- al_map %>%
  left_join(al_census_black2) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_child/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion Children: Black + Black, multiracial") +
  theme_void()



gg_prop_unvaxxed_const <-  al_map %>%
  left_join(al_census_allrace) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1-.33)*total_adult)/total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion: Unvaxxed",
          subtitle = "Using 33% for all counties") +
  theme_void()

gg_prop_unvaxxed <-  al_map %>%
  left_join(al_census_allrace) %>%
  left_join(al_vaxx %>% 
              select(County, OneDose, Vaxxed),
            by = c("NAME" = "County")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1- (Vaxxed/total))*total_adult)/total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion: Unvaxxed",
          subtitle = "Using County Vax Rates") +
  theme_void()


gg_prop_unvaxxed_const_white <-  al_map %>%
  left_join(al_census_white) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1-.33)*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion: Unvaxxed, White",
          subtitle = "Using 33% for all counties") +
  theme_void()


gg_prop_unvaxxed_white <-  al_map %>%
  left_join(al_census_white) %>%
  left_join(al_vaxx %>% 
              select(County, OneDose, Vaxxed),
            by = c("NAME" = "County")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1-(Vaxxed/total_adult_all))*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,1)) + 
  ggtitle("Proportion: Unvaxxed, White",
          subtitle = "Using County Vax Rates") +
  theme_void()

gg_prop_unvaxxed_const_black <-  al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1-.33)*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion: Unvaxxed, Black",
          subtitle = "Using 33% for all counties") +
  theme_void()

gg_prop_unvaxxed_black <-  al_map %>%
  left_join(al_census_black) %>%
  left_join(al_vaxx %>% 
              select(County, OneDose, Vaxxed),
            by = c("NAME" = "County")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1-(Vaxxed/total_adult_all))*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion: Unvaxxed, Black",
          subtitle = "Using County Vax Rates") +
  theme_void()

gg_prop_unvaxxed_const_black2 <-  al_map %>%
  left_join(al_census_black2) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1-.33)*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion: Unvaxxed, Black + Black, multiracial",
          subtitle = "Using 33% for all counties") +
  theme_void()

gg_prop_unvaxxed_black2 <-  al_map %>%
  left_join(al_census_black2) %>%
  left_join(al_vaxx %>% 
              select(County, OneDose, Vaxxed),
            by = c("NAME" = "County")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (total_child + (1-(Vaxxed/total_adult_all))*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) +
  ggtitle("Proportion: Unvaxxed, Black + Black, multiracial",
          subtitle = "Using County Vax Rates") +
  theme_void()

gg_prop_vaxxed_const <-  al_map %>%
  left_join(al_census_allrace) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (.33*total_adult)/total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion Vaxxed") +
  theme_void()

gg_prop_vaxxed <-  al_map %>%
  left_join(al_census_allrace) %>%
  left_join(al_vaxx %>% 
              select(County, OneDose, Vaxxed),
            by = c("NAME" = "County"))
st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = ((Vaxxed/total_adult_all)*total_adult)/total)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion Vaxxed") +
  theme_void()

gg_prop_vaxxed_const_white <-  al_map %>%
  left_join(al_census_white) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (.33*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion: Vaxxed, White") +
  theme_void()

gg_prop_vaxxed_const_black <-  al_map %>%
  left_join(al_census_black) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (.33*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion: Vaxxed, Black") +
  theme_void()

gg_prop_vaxxed_const_black2 <-  al_map %>%
  left_join(al_census_black2) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = (.33*total_adult)/total_all)) +
  scale_fill_viridis_c(direction = -1,
                       name = "Proportion",
                       limits = c(0,.7)) + 
  ggtitle("Proportion: Vaxxed, Black + Black, multiracial") +
  theme_void()


