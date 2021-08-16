censusDF <- readRDS(paste0(out_dir,"census_data_for_hum.RDS"))


years <- c(2000,2010)
hh_by_size_Pois <- NULL
for (t in years) {
  tmp <- filter(censusDF, Year==t)
  n_ct <- length(unique(tmp$GEOID)) # number of census tracts in year k
  
 hh.Pois.prop <- matrix(nrow = n_ct, ncol = 7)
 hh.Pois.num <- matrix(nrow = n_ct, ncol = 7)
 
  for (j in 1:n_ct) {
    if (tmp$household_population[j] == 0)
      break
    
    if(tmp$hhs[j] == 1)
      lambda <- 1
    else if(tmp$hhs[j] == 0)
      lambda <- 0
    else
      lambda <- tmp$hhs[j] - 1
    
    fact <- 1
    factot <- 0
    factot1 <- 0
    
    for (k in 0:6) {
      fact <- fact*k
      if(fact == 0)
        fact <- 1
      
      # use the poisson distribution to get the initial ct proportions
      
     hh.Pois.prop[j,k+1]<-(lambda^k)*exp(-lambda)/factorial(k) 
      
      factot <- factot +hh.Pois.prop[j,k+1]
    } # end k
    # normalize to 1.0 for distributions
    for (k in 0:5) {
    
     hh.Pois.prop[j,k+1] <-hh.Pois.prop[j,k+1]/factot
      factot1 <- factot1 +hh.Pois.prop[j,k+1]
    } # end k
    
    
   hh.Pois.prop[j,7] <- 1 - factot1; # fill the last as residual
    if (hh.Pois.prop[j,7] < 0)   # constrain to 0
     hh.Pois.prop[j,7] <- 0
    
    for (k in 0:6){
      # now get the first ct (row) estimate as proportion * hh
      hh.Pois.num[j,k+1] <- hh.Pois.prop[j,k+1] * tmp$hh[j]   # derive floating point value
      hh.Pois.num[j,k+1] <-  round(hh.Pois.num[j,k+1] + .5)# round up
      
    } # end k
    
  } # end j
 row.names(hh.Pois.num) <- tmp$GEOID
 hh_by_size_Pois <-  rbind(hh_by_size_Pois,data.frame(hh.Pois.num, Year = t))

} # end t

checkDF <- merge(censusDF, hh_by_size_Pois, by.x = c("GEOID","Year"),
                 by.y = c("row.names","Year"))
