########################################################
###
### Plots households KC

rm(list=ls())
###################
# -- Libraries -- #
###################

library(dplyr)
library(survey)
library(tidyverse)
library(data.table)

# constants
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")
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
  ) %>%
  distinct()  %>%
  mutate(hh_pop = value*as.numeric(hh_size),
         hh_type = case_when(
           hh_type == "family" ~ "Family",
           hh_type == "non-family" ~ "Non-family"
         ),
         tenure = case_when(
           tenure == "owner" ~ "Owner",
           tenure == "renter" ~ "Renter"
         )) 

plotDF <- plotDF %>%
  filter( year > 1999)

#############################
## Number of hh by hh size ##
#############################

p1 <- plotDF %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  filter(hh_type != "Family" | hh_size != 1) %>%
  ggplot(aes(x = year, y = value,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = plotDF %>%  
              filter(tenure != "ALL" & hh_type != "ALL")%>%
              filter(hh_type != "Family" | hh_size != 1) ,aes(color = hh_size),
            size = 1.2) +
  geom_point(aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
  facet_wrap(~hh_type) +
  labs(y="Number of households", x = "Year", color = "Household size", shape = "Tenure",
       title = "Number of households in King County by household size, type, and tenure") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent") +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text( 
          size=10),
        axis.text.x = element_text(size=10),
        # legend.justification = c("right", "bottom"),
        # legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave(p1, filename = paste0(code_dir, "Report_plots/hh_by_size_tenure_type.png"), width = 11, height = 6)

diffplot <- plotDF %>%
  filter(year == 2000) %>%
  rename(value2000 = value) %>%
  dplyr::select(hh_type, tenure, hh_size, value2000) %>%
  left_join(plotDF) %>%
  mutate(diff2000 = value - value2000) 

p2 <- diffplot %>%  
  # filter(diff2000 > 5000) %>%
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  filter(hh_type != "Family" | hh_size != 1) %>%
  ggplot(aes(x = year, y = diff2000,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = diffplot %>% filter(hh_type != "Family" | hh_size != 1) %>%
              filter(tenure != "ALL" & hh_type != "ALL") %>%
              filter(diff2000 > 5000),aes(color = hh_size),
            size = 1.2) +
  geom_point(data = diffplot %>% filter(hh_type != "Family" | hh_size != 1) %>%
               filter(tenure != "ALL" & hh_type != "ALL") %>%
               filter(diff2000 > 5000),aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020), limits = c(2000,2020)) +
  scale_y_continuous(limits = c(0,40000)) +
  facet_wrap(~hh_type) +
  labs(y="Number of new households since 2000", x = "Year", color = "Household size", shape = "Tenure",
       title = "Difference in number of households by household size, type, and tenure since 2000 in King County") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent") +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text( 
          size=10),
        axis.text.x = element_text(size=10),
        # legend.justification = c("right", "bottom"),
        # legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave(p2, filename = paste0(code_dir, "Report_plots/diff_since_2000_hh_by_size_tenure_type.png"), width = 11, height = 6)

#######################
## HH pop by hh size ##
#######################
popDF <- plotDF %>%
  filter(tenure != "ALL" & hh_type != "ALL")

p3 <- popDF %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  filter(hh_type != "Family" | hh_size != 1) %>%
  ggplot(aes(x = year, y = hh_pop,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = popDF %>%  
              filter(tenure != "ALL" & hh_type != "ALL")%>%
              filter(hh_type != "Family" | hh_size != 1)%>%
              filter(hh_pop > 5000) ,aes(color = hh_size),
            size = 1.2) +
  geom_point(aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020), limits = c(2000,2020)) +
  facet_wrap(~hh_type) +
  labs(y="Household population", x = "Year", color = "Household size", shape = "Tenure",
       title = "Household population in King County by household size, type, and tenure") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent") +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text( 
          size=10),
        axis.text.x = element_text(size=10),
        # legend.justification = c("right", "bottom"),
        # legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave(p3, filename = paste0(code_dir, "Report_plots/hh_pop_by_size_tenure_type.png"), width = 11, height = 6)

diffplot <- popDF %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  filter(hh_type != "Family" | hh_size != 1)%>%
  filter(year == 2000) %>%
  rename(pop2000 = hh_pop) %>%
  dplyr::select(hh_type, tenure, hh_size, pop2000) %>%
  left_join(popDF%>%  
              filter(tenure != "ALL" & hh_type != "ALL") %>%
              filter(hh_type != "Family" | hh_size != 1)) %>%
  mutate(diff2000 = hh_pop - pop2000) 

p4 <- diffplot %>%  
  # filter(diff2000 > 5000) %>%
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  filter(hh_type != "Family" | hh_size != 1) %>%
  ggplot(aes(x = year, y = diff2000,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = diffplot %>% filter(hh_type != "Family" | hh_size != 1) %>%
              filter(tenure != "ALL" & hh_type != "ALL") %>%
              filter(diff2000 > 5000),aes(color = hh_size),
            size = 1.2) +
  geom_point(data = diffplot %>% filter(hh_type != "Family" | hh_size != 1) %>%
               filter(tenure != "ALL" & hh_type != "ALL") %>%
               filter(diff2000 > 5000),aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020), limits = c(2000,2020)) +
  scale_y_continuous(limits = c(0,60000)) +
  facet_wrap(~hh_type) +
  labs(y="Household population gains since 2000", x = "Year", color = "Household size", shape = "Tenure",
       title = "Difference in household population since 2000 in King County, by household size, type, and tenure") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent") +
  theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
        axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text( 
          size=10),
        axis.text.x = element_text(size=10),
        # legend.justification = c("right", "bottom"),
        # legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), legend.title = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave(p4, filename = paste0(code_dir, "Report_plots/diff_since_2000_hh_pop_by_size_tenure_type.png"), width = 11, height = 6)

diffplot2 <- popDF %>%  
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  filter(hh_type != "Family" | hh_size != 1)%>%
  filter(year == 2000) %>%
  rename(pop2000 = hh_pop) %>%
  dplyr::select(hh_type, tenure, hh_size, pop2000) %>%
  left_join(popDF %>%  
              filter(tenure != "ALL" & hh_type != "ALL") %>%
              filter(hh_type != "Family" | hh_size != 1)%>%
              filter(year == 2019) %>%
              rename(pop2019 = hh_pop) %>%
              dplyr::select(hh_type, tenure, hh_size, pop2019)) %>%
  mutate(diff_2019_2000 = pop2019 - pop2000) %>%
  filter(abs(diff_2019_2000) > 5000) %>%
  dplyr::select(hh_type, tenure, hh_size, pop2000) %>%
  left_join(popDF%>%  
              filter(tenure != "ALL" & hh_type != "ALL") %>%
              filter(hh_type != "Family" | hh_size != 1)%>%
              dplyr::select(year,hh_type, tenure, hh_size, hh_pop)) %>%
  mutate(diff2000 = hh_pop - pop2000) 

p5 <- diffplot %>%  
  # filter(diff2000 > 5000) %>%
  filter(tenure != "ALL" & hh_type != "ALL") %>%
  filter(hh_type != "Family" | hh_size != 1) %>%
  ggplot(aes(x = year, y = diff2000,
             group = interaction(hh_size,tenure), shape = tenure)) +
  geom_line(data = diffplot %>% filter(hh_type != "Family" | hh_size != 1) %>%
              filter(tenure != "ALL" & hh_type != "ALL"),aes(color = hh_size),
            size = 1.2) +
  geom_point(data = diffplot %>% filter(hh_type != "Family" | hh_size != 1) %>%
               filter(tenure != "ALL" & hh_type != "ALL"),aes(color = hh_size, size = 1.5)) +
  # geom_ribbon(aes(fill = hh_size), alpha = .4) +
  theme_classic() +
  scale_x_continuous(breaks = c(2000,2005,2010,2015,2020), limits = c(2000,2020)) +
  scale_y_continuous(limits = c(0,60000)) +
  facet_wrap(~hh_type) +
  labs(y="Household population gains since 2000", x = "Year", color = "Household size", shape = "Tenure",
       title = "Difference in number of households by household size, type, and tenure since 2000 in King County") +
  guides(fill=FALSE,size = FALSE) + scale_color_brewer(palette = "Accent")

ggsave(p5, filename = paste0(code_dir, "Report_plots/diff_since_2000_hh_pop_by_size_tenure_type_only_category_with_more_than_5000_abs_change_over_2000-2019.png"), width = 11, height = 6)
