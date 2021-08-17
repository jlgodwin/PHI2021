########################################################
###
### First Analysis of ACS data
### Updated July 21st, 2021

rm(list=ls())
###################
# -- Libraries -- #
###################

library(tidycensus)
library(sf)
library(spdep)
library(raster)
library(mapview)
library(data.table)
library(tidyverse)
library(lme4)
library(INLA)
library(ggplot2)
library(gridExtra)

main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")
# set api key to access tidycensusdata
myKey <- "98068953d7f457e6b1166ff2218f263faefa119e"
census_api_key(myKey)

# Download the Variable dictionary
var_df <- load_variables(2010, "sf1", cache = TRUE)

##################
## Spatial data ##
##################
shape <- shapefile(paste0(data_dir, 
                          "2010_Census_Tracts_for_King_County_-_Conflated_to_Parcels___tracts10_area.shp"))
nb.r <- poly2nb(shape, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) 

#####################
# -- Census 2010 -- #
#####################


###############################################
# -- reproducing analysis from Jarosz 2021 -- #
##############################################
tractpopDF <- get_decennial("tract",
                          table = c("H01"),
                          summary_var = "H010001",
                          year = 2010,
                          state = "CA",
                          geometry = FALSE)

hhsizeDF <- get_decennial("tract",
                    table = c("H013"),
                    summary_var = "H013001",
                    year = 2010,
                    state = "CA",
                    geometry = FALSE)

hhaveragesizeDF <- get_decennial("tract",
                          table = c("H012"),
                          summary_var = "H012001",
                          year = 2010,
                          state = "CA",
                          geometry = FALSE)

length(unique(tractpopDF$GEOID))
length(unique(hhsizeDF$GEOID))
length(unique(hhaveragesizeDF$GEOID)) # 79 extra tracts compared to Jarosz paper ...

tractpopDF %>%
  filter(variable == "H010001") %>%
  rename(totpop = value) %>%
  dplyr::select(-c(variable,summary_value)) %>%
  left_join(hhaveragesizeDF %>%
              filter(variable == "H012001") %>%
              rename(averagesize = value)) %>%
  summarize(
    average_per_tract = sum(totpop*averagesize)/sum(totpop)
  ) # 3.06

tmp <- hhsizeDF %>%
  mutate(
    hhsize = case_when(
      # variable == "H013001" ~ 0, # total
      variable == "H013002" ~ 1,
      variable == "H013003" ~ 2,
      variable == "H013004" ~ 3,
      variable == "H013005" ~ 4,
      variable == "H013006" ~ 5,
      variable == "H013007" ~ 6,
      variable == "H013008" ~ 7
    ),
    hhsize = factor(hhsize)) %>%
  dplyr::select(-variable) %>%
  filter(!is.na(hhsize)) %>% 
  data.table()

tmp[, list(value = sum(value)), by = 'GEOID']
tmp[, prop := value/total]

###################
## Poisson model ##
###################

years <- c(2000,2010)

censusDF <- bind_rows(lapply(years, function(x){
  get_decennial(
    "tract",
    variables = c("H001001"),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(housing_units = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H010001"),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(population_total = value) %>%
      dplyr::select(-variable)
  )

censusDF$NAME[which(censusDF$population_total==0)] #"Census Tract 9901, King County, Washington"
key <- data.frame(GEOID = unique(censusPred$GEOID), id = 1:length(unique(censusPred$GEOID)))
censusPred <- censusDF %>%
  filter(population_total != 0) %>%
  left_join(key)
# Simplest Poisson regression
mod0 <- glm(population_total ~ Year + offset(log(housing_units)),
            family = "quasipoisson", data = filter(censusDF, population_total != 0))
summary(mod0)


# Poisson regression with iid RE by tracts, using lme4
mod1 <- glmer(population_total ~ Year + (1|id) + offset(log(housing_units)),
              family = "poisson", data = censusPred)
summary(mod1)


# Poisson regression with iid RE by tracts, using INLA
mod2 <- inla(population_total ~ f(id, model="iid") + Year,
              family = "poisson", 
              offset = log(housing_units),
             control.compute = list(config = TRUE,dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE),
             data = censusPred)
summary(mod2)

# 1. add spatial models smoothing CAR --> need consolidated list of tracts
##### TO DO ############
# censusDF$tract_id <- match(shape@data$GEO_ID_TRT, censusDF$id)
# Poisson regression with spatially correlated RE by tracts based on adjacency, using INLA
# mod2 <- inla(population_total ~ f(id, model="bym2",graph=mat) + Year,
#              family = "poisson", 
#              offset = log(housing_units), 
#              data = filter(censusDF, population_total != 0))
# summary(mod2)
censusPred$mean <- mod2$summary.fitted.values[, "mean"]/censusPred$housing_units
censusPred$LL <- mod2$summary.fitted.values[, "0.025quant"]/censusPred$housing_units
censusPred$UL <- mod2$summary.fitted.values[, "0.975quant"]/censusPred$housing_units
censusPred$widthCI <- censusPred$UL - censusPred$LL
censusMap <- st_as_sf(censusPred)
# 2. map of KC with census, mean estimates and RE
p1 <- ggplot(censusMap) + geom_sf(aes(fill = mean)) +
  scale_fill_gradient2(
    midpoint = 2, low = "blue", mid = "white", high = "red"
  ) +
  facet_wrap(~ Year) +
labs(fill = "Mean") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

p2 <- ggplot(censusMap) + geom_sf(aes(fill = widthCI)) +
  scale_fill_gradient2(
    midpoint = .1, low = "blue", mid = "white", high = "red"
  ) +
  facet_wrap(~ Year) +
  labs(fill = "Width CI") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

ggsave(grid.arrange(p1,p2, nrow = 2),
       filename = paste0(out_dir,"predAvgsize-tract.png"),
       width = 9, height = 6)

censusMap$re[censusMap$Year == 2000] <- mod2$summary.random$id[which(mod2$summary.random$id$ID %in% censusMap$id[censusMap$Year == 2000]),'mean']
censusMap$re[censusMap$Year == 2010] <- mod2$summary.random$id[which(mod2$summary.random$id$ID %in% censusMap$id[censusMap$Year == 2010]),'mean']

re<-ggplot(censusMap) + geom_sf(aes(fill = re)) +
  scale_fill_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red"
  ) +
  labs(fill = "Random effects") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

ggsave(re,
       filename = paste0(out_dir,"predAvgsize-tract-re.png"),
       width = 9, height = 6)

# 3. Posterior distribution of Poisson and how many hh in each bin
samplePars <- function(model, n){
  drawList <- inla.posterior.sample(n, model)
  npars <- nrow(drawList[[1]]$latent)
  betaDraws <- sapply(drawList, function(x){
    x$latent[(npars+1-length(model$names.fixed)):npars,]
  })
  commIDX <- startsWith(row.names(drawList[[1]]$latent), "id")
  idDraws <- sapply(drawList, function(x){
    x$latent[commIDX,]
  })
  if(class(idDraws) == "list"){
    idDraws <- NULL
  }
  list(beta=betaDraws, id=idDraws)
}

pars <- samplePars(mod2, 10)
ff <- update(mod2$.args$formula[-2], ~ . - f(id, model="iid"))
DF <- censusPred
X <- model.matrix(ff, DF)
linPred <- X %*% pars$beta
if("id" %in% names(DF) & !is.null(pars$id)){
  commIDX <- paste0("id:", DF$id)
  linPred <- linPred + pars$id[commIDX,]
}
if(prob){
  linPred <- arm::invlogit(linPred)
}

hhsizePred <- matrix(nrow = length(linPred), ncol = 1000)
for (i in 1:length(linPred)) {
  hhsizePred[i,] <- rpois(lambda = apply(exp(linPred),1,median)[i]-1, n = 1000)+1
}



rpois(lambda = apply(exp(linPred),1,median)-1, n = 1000)
set.seed(1985)
post<-inla.posterior.sample(1000,mod2)
names(post)

stable_tracts <- unique(censusPred$id)[unique(censusPred$id) %in% 
                                            censusMap$id[censusMap$Year == 2000] &
                                            unique(censusPred$id) %in% 
                                            censusMap$id[censusMap$Year == 2010]]
sampsDiff<-matrix(nrow=length(stable_tracts),ncol=1000)

samps2000<-matrix(nrow=sum(unique(censusPred$id) %in% 
                                censusMap$id[censusMap$Year == 2000]),ncol=1000)
row.names(samps2000) <- unique(censusPred$id)[unique(censusPred$id) %in% 
  censusMap$id[censusMap$Year == 2000]]

samps2010<-matrix(nrow=sum(unique(censusPred$id) %in% 
                             censusMap$id[censusMap$Year == 2010]),ncol=1000)
row.names(samps2010) <- unique(censusPred$id)[unique(censusPred$id) %in% 
                                                   censusMap$id[censusMap$Year == 2010]]

for(jj in 1:1000){
  samps2000[,jj] <- (exp(post[[jj]]$latent[which(unique(censusPred$id) %in% 
                                             censusMap$id[censusMap$Year == 2000])]))
  samps2010[,jj] <- (exp(post[[jj]]$latent[which(unique(censusPred$id) %in% 
                                                  censusMap$id[censusMap$Year == 2010])]))
}

post_hhsize2000 <- apply(samps2000, 1, mean)
post_avehhsize2000 <- post_hhsize2000/censusPred$housing_units[censusPred$Year==2000]

ggplot(data = data.frame(post_avehhsize2000), aes(x=post_avehhsize2000)) +
  geom_histogram()
# 4. Compared to observed distribution across bin size
# 5. start thinking about ordered model
# 6. check for 
# map of census tracts by total population to see which ones are above 65,000 inhabitants
# check 
