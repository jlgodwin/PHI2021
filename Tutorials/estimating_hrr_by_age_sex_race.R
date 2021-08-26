
rm(list=ls())
library(ggplot2)
library(INLA)
# constants
main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")

code_dir <- paste0(main_dir, "PHI2021/household_size/")

indDF <- readRDS(file = paste0(code_dir, "ind_level_data_ACS5.RDS"))

data <- lapply(c(2009,2014,2019), function(year) {
  cat(paste0(year, "\n")); flush.console()
  
  
  totDF <- indDF %>%
    filter(YEAR == year)
  data <- data.table(totDF)
  data <- data[, .(YEAR, COUNTYFIP, SERIAL, PERNUM, PERWT, STRATA, AGE, SEX, RACE,
                   HISPAN, RELATED)]
  
  pop <- data[,list(pop = sum(PERWT))]
  data[, id := as.integer(paste(SERIAL, PERNUM, sep =""))]
  data[, agegp := cut(AGE, breaks = c(seq(0,85,5),105), right = FALSE)]
  data[, sex := ifelse(SEX == 1, 1L, 2L)]
  data[, HHP := 0]
  
  data[RELATED %in% 9996:9999, HHP := NA]
  data[RELATED == 101, HHP := 1]

  
  # format race
  # seven mutually exclusive race and  ethnic groups: 
  # Non-Hispanic White, Non-Hispanic Black, Non-Hispanic Asian, 
  # Non-Hispanic  American Indian and Alaskan Native (AIAN), 
  #Non-Hispanic Native Hawaiian and Other Pacific  Islander (NHOPI), 
  # Non-Hispanic Multirace (Two or more races), and Hispanic
  data[HISPAN != 0 & HISPAN != 9, race := "Hispanic"]
  data[HISPAN == 0 & RACE == 1, race := "White"]
  data[HISPAN == 0 & RACE == 2, race := "Black"]
  data[HISPAN == 0 & (RACE == 4 | RACE == 5), race := "Asian"]
  data[HISPAN == 0 & RACE == 3, race := "AIAN"]
  data[HISPAN == 0 & RACE == 6, race := "NHOPI"]
  data[HISPAN == 0 & (RACE == 7 | RACE == 8 | RACE == 9), race := "Two or More Races"]
  
  data <- data[, list(pop = sum(PERWT),
                      hhp = sum(HHP*PERWT)), by = 'YEAR,sex,agegp,race']
})
data <- rbindlist(data, use.names = T)
data[, hhr := hhp/pop]

saveRDS(data, paste0(out_dir, "hrr_by_age_sex_race.RDS"))
