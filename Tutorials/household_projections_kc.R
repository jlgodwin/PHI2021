
rm(list=ls())
library(ggplot2)
library(INLA)
library(raster)
library(sf)
library(RColorBrewer)

# constants
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")
code_dir <- paste0(main_dir, "PHI2021/household_size/")

source(paste0(main_dir, "PHI2021/Tutorials/map_main_theme.R"))

## loading dataframes and shapefile
hra <- shapefile(paste0(data_dir,"HRA_2010Block_Clip"))


hhrDF <- readRDS(file = paste0(out_dir, "hrr_by_age_sex_race.RDS")) %>%
  mutate(Age5 = 
           case_when(agegp == "[0,5)" ~ "0-4",
                     agegp == "[5,10)" ~ "5-9",
                     agegp == "[10,15)" ~ "10-14",
                     agegp == "[15,20)" ~ "15-19",
                     agegp == "[20,25)" ~ "20-24",
                     agegp == "[25,30)" ~ "25-29",
                     agegp == "[30,35)" ~ "30-34",
                     agegp == "[35,40)" ~ "35-39",
                     agegp == "[40,45)" ~ "40-44",
                     agegp == "[45,50)" ~ "45-49",
                     agegp == "[50,55)" ~ "50-54",
                     agegp == "[55,60)" ~ "55-59",
                     agegp == "[60,65)" ~ "60-64",
                     agegp == "[65,70)" ~ "65-69",
                     agegp == "[70,75)" ~ "70-74",
                     agegp == "[75,80)" ~ "75-79",
                     agegp == "[80,85)" ~ "80-84",
                     agegp == "[85,105)" ~ "85+"),
         Sex = case_when(
           sex == 1 ~ "Male",
           sex == 2 ~ "Female"
         ),
         Year = case_when(
           YEAR == 2009 ~ 2010,
           YEAR == 2014 ~ 2015,
           YEAR == 2019 ~ 2020
         ))

# hhrDF %>%
#   filter(sex == 1 & race == "Non-Hispanic White") %>%
#   ggplot(aes(x=YEAR, y = hhr, color = agegp)) +
#   geom_point() +
#   geom_line()

## Do it with observed years (hhr for 2009, 2014, 2019, and pop proj for 2010, 2015, 2020)

hra_proj <- read.table(paste0(data_dir, "hra_age5_race_sex_proj_2000_2045.txt"),
                       sep = ",",
                       header = TRUE)

hra_proj <- hra_proj %>%
  data.table()

hra_proj[,list(pop = sum(value)), by = 'Year']
hra_proj$GEOID[hra_proj$GEOID == "Fed Way-Dash Pt"] <- "Fed Way-Dash Point/Woodmont"

pastDF <- hhrDF %>%
  rename(Race = race) %>%
  dplyr::select(-c(YEAR, sex, pop, hhp, agegp)) %>%
  left_join(hra_proj) %>%
  data.table()

pastDF[, households := value*hhr]

pop_hh_by_HRA <- pastDF[, list(pop = sum(value),
                               hh = sum(households)), by = 'Year,GEOID']

pop_hh_by_HRA[, average_hhsize := pop/hh]

## Predict for 2020-2045

projDF <- hhrDF %>%
  rename(Race = race) %>%
  filter(Year == 2020) %>% # using 2019 hhr to project, i.e assuming it constant for future
  dplyr::select(-c(YEAR, Year, sex, pop, hhp, agegp)) %>%
  left_join(hra_proj %>%
              filter(Year > 2019)) %>%
  data.table()

projDF[, households := value*hhr]

pop_hh_by_HRA <- projDF[, list(pop = sum(value),
                               hh = sum(households)), by = 'Year,GEOID']

# projDF[, list(pop = sum(value),
#               hh = sum(households)), by = 'Year']

pop_hh_by_HRA[, average_hhsize := pop/hh]

write_csv(as.data.frame(pop_hh_by_HRA),
          paste0(out_dir,"household_forecasts.csv"))

plot.df <- st_as_sf(hra) %>%
  right_join(pop_hh_by_HRA %>%
              filter(Year %in% c(2025,2035,2045)) %>%
              rename(HRA2010v2_ = GEOID))

p1 <- ggplot(plot.df) + 
  geom_sf(aes(fill=hh)) +
  ggtitle("Projected number of households") +
  labs(fill = "Number of households") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "Blues") +
  facet_wrap(~ Year, nrow = 2)

ggsave(p1, filename = paste0(code_dir, "Report_plots/Projected_number_hh_2025-2045.png"),
       width = 9, height = 6)

## Difference in number of households
plot.df <- st_as_sf(hra) %>%
  full_join(pop_hh_by_HRA %>%
  filter(Year == 2025) %>%
  rename(hh2025=hh) %>%
  dplyr::select(GEOID, hh2025) %>%
  left_join(pop_hh_by_HRA %>%
              filter(Year == 2045)) %>%
    mutate(
      diff_hh = hh - hh2025
    ) %>%
  rename(HRA2010v2_ = GEOID))


p2 <- ggplot(plot.df) + 
  geom_sf(aes(fill=diff_hh)) +
  ggtitle("Difference in projected number of households in 2025-2045") +
  labs(fill = "Number of households") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  map_theme_main + 
  scale_fill_distiller(palette = "RdBu")

ggsave(p2, filename = paste0(code_dir, "Report_plots/Difference_in_number_hh_2025-2045.png"),
       width = 9, height = 6)

