rm(list=ls())

#####################################
# -- load packages and functions -- #
#####################################
library(tidyverse)
library(data.table)

# constants
main_dir <- "/Users/adrienallorant/Documents/UW/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "Code/PHI2021/household_size/")
plotDF <- read_csv(paste0(out_dir,"All_KC_hh_size_total.csv")) %>%
  data.table() %>%
  dplyr::select(year,hh_type,tenure,
    prevalence.7_person_hh,
     prevalence.6_person_hh,
     prevalence.5_person_hh,
     prevalence.4_person_hh,
     prevalence.3_person_hh,
     prevalence.2_person_hh,
     prevalence.1_person_hh
  ) %>%
  melt.data.table(id.vars = c('year','hh_type','tenure')) %>%
  mutate(
    hh_size  = case_when(
      variable == "prevalence.7_person_hh" ~ 7,
      variable == "prevalence.6_person_hh" ~ 6,
      variable == "prevalence.5_person_hh" ~ 5,
      variable == "prevalence.4_person_hh" ~ 4,
      variable == "prevalence.3_person_hh" ~ 3,
      variable == "prevalence.2_person_hh" ~ 2,
      variable == "prevalence.1_person_hh" ~ 1
    ),
    hh_size = factor(hh_size)
  ) 

plotDF <- plotDF %>%
  filter( year > 1999)
p1 <- plotDF %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  ggplot(aes(x = year, y = value,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = plotDF %>%  
              filter(tenure != "ALL" & hh_type != "ALL"),aes(color = hh_size),
            size = 1.2) +
  geom_point(aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  facet_wrap(~hh_type) +
  labs(y="Number of households", x = "Year", color = "Size-of-household", shape = "Tenure",
       title = "Number of households in King County by size-of-household, type, and tenure") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent")

ggsave(p1, filename = paste0(code_dir, "Report_plots/hh_by_size_tenure_type.png"), width = 11, height = 6)

diffplot <- plotDF %>%
  filter(year == 2000) %>%
  rename(value2000 = value) %>%
  dplyr::select(hh_type, tenure, hh_size, value2000) %>%
  left_join(plotDF) %>%
  mutate(diff2000 = value - value2000) 

p2 <- diffplot %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  ggplot(aes(x = year, y = diff2000,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = diffplot %>%  
              filter(tenure != "ALL" & hh_type != "ALL"),aes(color = hh_size),
            size = 1.2) +
  geom_point(aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  facet_wrap(~hh_type) +
  labs(y="Number of households", x = "Year", color = "Size-of-household", shape = "Tenure",
       title = "Difference in number of households by household size since 2000 in King County") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent")

ggsave(p2, filename = paste0(code_dir, "Report_plots/diff_since_2000_hh_by_size_tenure_type.png"), width = 11, height = 6)

