
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
  data <- data[, .(YEAR, COUNTYFIP, SERIAL, PERNUM, PERWT, STRATA, AGE, SEX, 
                   RELATED)]
  
  pop <- data[,list(pop = sum(PERWT))]
  data[, id := as.integer(paste(SERIAL, PERNUM, sep =""))]
  data[, agegp := cut(AGE, breaks = c(seq(0,85,5),105), right = FALSE)]
  data[, sex := ifelse(SEX == 1, 1L, 2L)]
  data[, HHP := 0]
  
  data[RELATED %in% 9996:9999, HHP := NA]
  data[RELATED == 101, HHP := 1]
  # data[, HHP := factor(HHP, levels = c(0,1))]
  
  data <- data[, list(pop = sum(PERWT),
                      hhp = sum(HHP*PERWT)), by = 'YEAR,sex,agegp']
})
data <- rbindlist(data, use.names = T)
data[, hhr := hhp/pop]

dcast.data.table(data, hhr ~ year + sex)
dcast.data.table(data[, round(sum(pop)/1000000, 1), by = 'year,sex'], sex ~ year)
