
rm(list=ls())
library(ggplot2)
library(INLA)
# constants
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")

predDF <- readRDS(paste0(out_dir,"Preds/_preds.rds")) %>%
  filter(indicator != "logit_hh_size") %>%
  mutate(
    indicator = case_when(indicator == "logit_prevalence.1_person_hh" ~ "prevalence.1_person_hh",
                          indicator == "logit_prevalence.2_person_hh" ~ "prevalence.2_person_hh",
                          indicator == "logit_prevalence.3_person_hh" ~ "prevalence.3_person_hh",
                          indicator == "logit_prevalence.4_person_hh" ~ "prevalence.4_person_hh",
                          indicator == "logit_prevalence.5_person_hh" ~ "prevalence.5_person_hh",
                          indicator == "logit_prevalence.6_person_hh" ~ "prevalence.6_person_hh",
                          indicator == "logit_prevalence.7_person_hh" ~ "prevalence.7_person_hh",
                          indicator == "logit_prevalence.1_person_hh" ~ "prevalence.1_person_hh")
  )%>%
  filter(!is.na(indicator)) %>%
  mutate_at(c("mean","low","up"), function(x) x*100) %>%
  rbind(
    readRDS(paste0(out_dir,"Preds/_preds.rds")) %>%
      filter(indicator == "hh_size"))
########################
## Loading direct estimates ##
## for comparisons ##
#####################

# Subset to indicator of interest

directEst <- read_csv(paste0(out_dir,"All_KC_hh_size.csv"))
directDF <- directEst %>%
      dplyr::select(year, hh_type, tenure, prevalence.1_person_hh, se.prevalence.1_person_hh,survey) %>%
      mutate(mean = prevalence.1_person_hh,
             low = prevalence.1_person_hh - 2*se.prevalence.1_person_hh,
             up = prevalence.1_person_hh + 2*se.prevalence.1_person_hh,
             indicator = 'prevalence.1_person_hh') %>%
      dplyr::select(-c(prevalence.1_person_hh, se.prevalence.1_person_hh)) %>%
  rbind(
    directEst %>%
      dplyr::select(year, hh_type, tenure, prevalence.2_person_hh, se.prevalence.2_person_hh,survey) %>%
      mutate(mean = prevalence.2_person_hh,
             low = prevalence.2_person_hh - 2*se.prevalence.2_person_hh,
             up = prevalence.2_person_hh + 2*se.prevalence.2_person_hh,
             indicator = 'prevalence.2_person_hh') %>%
      dplyr::select(-c(prevalence.2_person_hh, se.prevalence.2_person_hh))
  ) %>%
  rbind(
    directEst %>%
      dplyr::select(year, hh_type, tenure, prevalence.3_person_hh, se.prevalence.3_person_hh,survey) %>%
      mutate(mean = prevalence.3_person_hh,
             low = prevalence.3_person_hh - 2*se.prevalence.3_person_hh,
             up = prevalence.3_person_hh + 2*se.prevalence.3_person_hh,
             indicator = 'prevalence.3_person_hh') %>%
      dplyr::select(-c(prevalence.3_person_hh, se.prevalence.3_person_hh))
  ) %>%
  rbind(
    directEst %>%
      dplyr::select(year, hh_type, tenure, prevalence.4_person_hh, se.prevalence.4_person_hh,survey) %>%
      mutate(mean = prevalence.4_person_hh,
             low = prevalence.4_person_hh - 2*se.prevalence.4_person_hh,
             up = prevalence.4_person_hh + 2*se.prevalence.4_person_hh,
             indicator = 'prevalence.4_person_hh') %>%
      dplyr::select(-c(prevalence.4_person_hh, se.prevalence.4_person_hh))
  )%>%
  rbind(
    directEst %>%
      dplyr::select(year, hh_type, tenure, prevalence.5_person_hh, se.prevalence.5_person_hh,survey) %>%
      mutate(mean = prevalence.5_person_hh,
             low = prevalence.5_person_hh - 2*se.prevalence.5_person_hh,
             up = prevalence.5_person_hh + 2*se.prevalence.5_person_hh,
             indicator = 'prevalence.5_person_hh') %>%
      dplyr::select(-c(prevalence.5_person_hh, se.prevalence.5_person_hh))
  ) %>%  rbind(
    directEst %>%
      dplyr::select(year, hh_type, tenure, prevalence.6_person_hh, se.prevalence.6_person_hh,survey) %>%
      mutate(mean = prevalence.6_person_hh,
             low = prevalence.6_person_hh - 2*se.prevalence.6_person_hh,
             up = prevalence.6_person_hh + 2*se.prevalence.6_person_hh,
             indicator = 'prevalence.6_person_hh') %>%
      dplyr::select(-c(prevalence.6_person_hh, se.prevalence.6_person_hh))
  )%>%  rbind(
    directEst %>%
      dplyr::select(year, hh_type, tenure, prevalence.7_person_hh, se.prevalence.7_person_hh,survey) %>%
      mutate(mean = prevalence.7_person_hh,
             low = prevalence.7_person_hh - 2*se.prevalence.7_person_hh,
             up = prevalence.7_person_hh + 2*se.prevalence.7_person_hh,
             indicator = 'prevalence.7_person_hh') %>%
      dplyr::select(-c(prevalence.7_person_hh, se.prevalence.7_person_hh))
  )%>%
  mutate_at(c("mean","low","up"), function(x) x*100) %>%
  mutate(
    low = ifelse(low < 0, 0, low),
    up = ifelse(up > 100, 100, up)
  ) %>%
  rbind(directEst %>%
  dplyr::select(year, hh_type, tenure, hh_size, se.average_hh_size,survey) %>%
  mutate(mean = hh_size,
         low = hh_size - 2*se.average_hh_size,
         up = hh_size + 2*se.average_hh_size,
         indicator = 'hh_size') %>%
  dplyr::select(-c(hh_size, se.average_hh_size)))

################################
## Select overall indicators  ##
################################
indicators<-c("hh_size","prevalence.1_person_hh","prevalence.2_person_hh",
              "prevalence.3_person_hh","prevalence.4_person_hh",
              "prevalence.5_person_hh","prevalence.6_person_hh",
              "prevalence.7_person_hh")

hh_type<-c("ALL","family","non-family")
tenure<-c("ALL","owner","renter")

groups<-expand.grid(outcome=indicators,hh_type=hh_type, tenure = tenure)
groups_all<-data.frame(outcome=indicators,hh_type="ALL",tenure="ALL")#),mga="ALL"

results<-rbind(groups_all,groups)

for (j in results) {

  df <- filter(predDF,indicator == results$outcome[j]) #& hh_type == results$hh_type[j] & tenure == results$tenure[j])
  est <- filter(directDF,indicator == results$outcome[j]) #& hh_type == results$hh_type[j] & tenure == results$tenure[j])

  gg <- ggplot(df, aes(x = year, y = mean, group = hh_type, color = hh_type)) +
    geom_line(size = 1) +
    # scale_y_continuous(breaks = c(0,20,40,60,80,100), expand = c(0, 0), limits = c(0, 100)) +
    # scale_x_continuous(breaks = c(2013, 2015, 2017)) +
    geom_ribbon(data = filter(predDF,indicator == results$outcome[j]),
                aes(ymin=low, ymax=up, group = hh_type, color = hh_type, fill = hh_type), linetype=2, alpha=0.1) +
    geom_point(data = filter(directDF, indicator == results$outcome[j]), aes(x=year, y=mean,  color = hh_type))+
    geom_segment(data = filter(directDF, indicator == results$outcome[j] ),
                 aes(x=year, y=low, xend=year, yend=up, color = hh_type, group = hh_type))+
    # geom_rug(data=df) +
    labs(x ='Year', y = 'Score (%)',
         title = "") +
    theme_bw() +
    facet_wrap(~tenure) +
    scale_color_manual(values=c("black","#FFA500", "#800080")) +
    guides(fill=FALSE) +
    theme(text= element_text(family= "Serif"), panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.y = element_text( 
            size=14),
          axis.text.x = element_text(size=14),
          legend.position = c(.95, .75),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6), legend.title = element_blank())
  print(gg)
  ggsave(plot = gg, filename = paste0(country.file,"output/paper/national/time_series_",k,"_",".png"))
  
  
} #k

