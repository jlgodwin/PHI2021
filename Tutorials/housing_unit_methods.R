main_dir <- "C:/Users/allorant/OneDrive - UW/Shared with Everyone/UW/4thYear/PHI2021/"
data_dir <- paste0(main_dir, "data/")
out_dir <- paste0(main_dir, "output/")
###################
## Poisson model ##
###################

years <- c(2000,2010)

censusDF <- bind_rows(lapply(years, function(x){
  get_decennial(
    "tract",
    variables = c("P001001"# tot pop
                  ),
    state = "WA",
    county = "King",
    year = x,
    cache_table = TRUE) %>%
    mutate(Year = x)})) %>%
  rename(total_population = value) %>%
  dplyr::select(-variable) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H001001" # hs
                      ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(housing_units = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H010001" # hhp
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(household_population = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H003001" 
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(total_occ = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H003002" # hhp
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(occ = value) %>%
      dplyr::select(-variable)
  )  %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H003003"
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(vac = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H012001" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013001" # total # hh
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hh = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013002" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.1 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013003" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.2 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013004" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.3 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013005" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.4 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013006" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.5 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013007" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.6 = value) %>%
      dplyr::select(-variable)
  ) %>%
  left_join(
    bind_rows(lapply(years, function(x){
      get_decennial(
        "tract",
        variables = c("H013008" #hhs
        ),
        state = "WA",
        county = "King",
        year = x,
        cache_table = TRUE,
        geometry = TRUE) %>%
        mutate(Year = x)})) %>%
      rename(hhs.7 = value) %>%
      dplyr::select(-variable)
  ) 

checkpopDF <- censusDF %>%
  mutate(vac_rate = vac/total_occ,
         tot_pop_hum = housing_units*(1-vac_rate)*hhs,
         error_hum = household_population-tot_pop_hum)
saveRDS(censusDF, file = paste0(out_dir,"census_data_for_hum.RDS"))

