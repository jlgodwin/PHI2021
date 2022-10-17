# Libraries ####
library(shiny)


# Load Data ####

## Download ####

### Tract .json ####
# check if required files exist; if not, download the files and save locally
# Updated as of 9.9
# King County census tract GIS data
if (!file.exists("./data/kc_tract.json")) {
    download_kc_tract()
    while (!file.exists("./data/kc_tract.json")) {
        Sys.sleep(1)
    }
}

### KC public clinics ,json ####

if (!file.exists("./data/kc_public_clinics.json")) {
    download_kc_public_clinics()
    while (!file.exists("./data/kc_public_clinics.json")) {
        Sys.sleep(1)
    }
    
}

### KC school sites .json ####
if (!file.exists("./data/kc_schools.json")) {
    download_kc_schools()
    while (!file.exists("./data/kc_schools.json")) {
        Sys.sleep(1)
    }
    
}

## Load ####

### KC tracts .json ####
kc_tract_spdf <- readOGR("./data/kc_tract.json")

### KC HRA .json ####
kc_hra_spdf <- readOGR("./data/kc_hra.json")

### Transit lines .json ####
kc_tl_2040 <- readOGR("./data/kc_tl_2040.json")
while (!exists("kc_tl_2040")) {
    Sys.sleep(1)
}

### Tract-level Pop Proj, .csv ####
message("Read tract-level population projections .csv.\n")
tract_proj <- read.csv(
    file = "./data/tract_age5_race_sex_proj_2000_2045.csv",
    colClasses = c("GEOID" = "character")
)

tract_proj <- tract_proj %>% 
    rename("Age" = "Age5") %>% 
    as.data.frame()
while (!exists("tract_proj")) {
    Sys.sleep(1)
}

### Tract-level Med Inc .csv ARA ####
message("Read tract-level median income .csv.\n")
tract_inc <- read.csv(
    file = "./data/med_inc_tract.csv",
    colClasses = c("GEOID" = "character", "value" = "double")
)
while (!exists("tract_inc")) {
    Sys.sleep(1)
}

### Tract-level Education .csv ARA ####
message("Read tract-level educational attainment .csv.\n")
tract_edu <- read.csv(
    file = "./data/tract_edu_attainmentv2.csv",
    colClasses = c("GEOID" = "character", "value" = "double")
)
while (!exists("tract_edu")) {
    Sys.sleep(1)
}

### Tract-level Rent Burden .csv ARA ####
message("Read tract-level rent burden .csv.\n")
tract_burden <- read.csv(
    file = "./data/tract_rent_burden_pop.csv",
    colClasses = c("GEOID" = "character", "value" = "double")
)
while (!exists("tract_burden")) {
    Sys.sleep(1)
}

### HRA-level Rent Burden .csv ####
message("Read HRA-level rent burden .csv.\n")
hra_burden <- read.csv(
    file = "./data/burden_hra.csv",
    colClasses = c("HRA2010v2_" = "character", "value" = "double")
)
hra_burden <- hra_burden %>% 
    rename("GEOID" = "HRA2010v2_",
           "short_label" = "burden")

### Tract-level Housing Tenure .csv ARA ####
message("Read tract-level tenure .csv.\n")
tract_RO <- read.csv(
    file = "./data/tract_RO_population.csv",
    colClasses = c("GEOID" = "character", "value" = "double")
)
while (!exists("tract_RO")) {
    Sys.sleep(1)
}

### Tract-level Age & Transpo .csv ARA ####
message("Read tract-level median age by transportation mode .csv.\n")
tract_transp <- read.csv(
    file = "./data/tract_transp_age.csv", 
    colClasses = c("GEOID" = "character", "value" = "double")
)

### Tract-level Median Rent .csv ####
message("Read tract-level median rent .csv.\n")
tract_med_rent <- read.csv(
    file = "./data/med_rent_tract.csv", 
    colClasses = c("GEOID" = "character", "value" = "double")
)

### HRA-level Pop Proj .csv####
message("Read HRA-level population projections .csv.\n")
hra_proj <- read.csv(
    file = "./data/hra_age5_race_sex_proj_2000_2045.csv"
)

hra_proj <- hra_proj %>% 
    rename("Age" = "Age5") %>% 
    as.data.frame()
while (!exists("hra_proj")) {
    Sys.sleep(1)
}


# Server Function ####
server <- function(input, output, session) {
    
    ## var_reactive ####
    # ARA_ Choose variable
    var_reactive <- reactive({
        input$var
    })
    
    ## geo_reactive ####
    # return TRUE when census tract level is selected
    geo_reactive <- reactive({
        # input$geo_level == "Health Reporting Area (HRA)"
        input$geo_level == "Census Tract"
    })
    
    ## straight_pipe_hh ####
    straight_pipe_hh <- reactive({
        input$var == "Household Size" &&
            input$geo_level == "Health Reporting Area (HRA)"
    })
    
    
    ## measure_reactive  ####
    measure_reactive <- reactive({
        input$measure_type
    })
    
    pal_reactive <- reactive({
        measure <- input$measure_type
        pal <- "Blues"
        
        if(measure != "Count"){
            pal <- "YlGnBu"
        }
        pal
    })
    
    ## year_reactive ####
    year_reactive <- reactive({
        var <- input$var
        if(var == "Population"){
            as.numeric(strsplit(input$year, "-")[[1]][1])
        }else if(var %in% c("Education Level", "Rent Burden")){
            as.numeric(strsplit(input$year, "-")[[1]][2])
        }else if(var == "Household Size"){
            as.numeric(strsplit(input$year, "-")[[1]][2])
        }
    })
    
    ## race_reactive ####
    race_reactive <- reactive({
        if (input$race == "American Indian and\nAlaska Native (AIAN)") {
            "AIAN"
        } else if (input$race == "Native Hawaiian or\nOther Pacific Islander (NHOPI)") {
            "NHOPI"
        } else if (input$race == "All") {
            c("AIAN", "Asian", "Black", "Hispanic",
              "NHOPI", "Two or More Races", "White")
        } else {
            input$race
        }
    })
    
    ## size_reactive ####
    size_reactive <- reactive({
        if(input$size == "4+") {
            4
        } else {
            input$size
        }
    })
    
    
    ## sex_reactive ####
    sex_reactive <- reactive({
        if (input$sex == "Both") {
            c("Female", "Male")
        } else {
            input$sex
        }
    })
    
    ## age_reactive ####
    age_reactive <- reactive({
        upper <- 90
        
        if (input$age[2] != "85+") {
            upper <- as.integer(input$age[2])
        }
        
        if (input$age[1] == "85+") {
            c()
        } else {
            lower <- as.integer(input$age[1])
            age_list <- c(
                "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                "70-74", "75-79", "80-84", "85+"
            )
            
            if (upper != lower) {
                age_list[(lower / 5 + 1):(upper / 5)]
            } else {
                c()
            }
        }
    })
    
    
    
    
    
    ## edu_reactive  ####
    # ARA_edu: Choose variable
    edu_reactive <- reactive({
        input$edu
    })
    
    ## rent_reactive ####
    # ARA_rent: Choose variable
    
    rent_reactive <- reactive({
        input$burden
    })
    
    ## tenure_reactive  ####
    tenure_reactive <- reactive({
        input$tenure
    })
    
    ## trans_reactive ####
    #Race/counts only/not inclcuding median age 
    transp_reactive <- reactive({
        input$mode_transp
    })
    
    ## all_selected ####
    # return TRUE if "Prevalence" and the whole population are selected; 
    # this is for generating the warning message
    all_selected <- reactive({
        input$measure_type == "Prevalence" &&
            length(sex_reactive()) == 2 &&
            length(age_reactive()) == 18 &&
            length(race_reactive()) == 7
    })
    
    ## warning_reactive  ####
    # warning message when "Prevalence" and the whole population are selected
    warning_text_reactive <- reactive({
        if (all_selected()) {
            ""
            
        } else {
            ""
        }
    })
    
    ## age_warning_reactive ####
    # warning message when the no age interval is selected
    age_warning_reactive <- reactive({
        if (length(age_reactive()) == 0) {
            "Please select a VALID age range!"
        }
    })
    
    ## geo_year_df_reactive ####
    
    # selected geographic level and then selected age groups
    # ARA_: conditional branch to differentiate selected df based off of var selection
    # ARA_: I added demographic filters as it originally defaulted 
    ## to do it later on but not evrey var holds demographic info
    ## geo_reactive == TRUE when Tract_level
    
    geo_df_reactive <- reactive({
        
        var <- input$var
        geo <- (input$geo_level == "Census Tract")
        ### Population ####
        if (var == "Population"){
            if (geo) {
                geo_df <- tract_proj %>%
                    mutate(moe = 0)
            }else{
                geo_df <- hra_proj %>%
                    mutate(moe = 0) 
            }
            
        }
        ## Median Income ####
        else if(var == "Median Income"){
            if (geo) {
                geo_df <- tract_inc 
            }else{
                
            }
        }
        ## Education ####
        else if (var == "Education Level"){
            if(geo){
                geo_df <- tract_edu 
            }else{
                
            }                
        }   
        ## Rent Burden ####
        else if (var == "Rent Burden"){
            if(geo){
                geo_df <- tract_burden
            }else{
                geo_df <- hra_burden 
            }                
        }
        ## Household Size ####
        else if (var == "Household Size") {
            if(geo){
                geo_df <- tract_RO 
            }else{
                
            }
        }
        ## Transportation & Age ####
        else if (var == "Methods of Transportation to Work") {
            if(geo){
                geo_df <- tract_transp 
            }else{
                
            }
        }
        ## Median Rent ####
        else if (var == "Median Gross Rent"){
            if(geo){
                geo_df <- tract_med_rent 
            }else{
                
            }
        }
        
        
        ### Rename geo_year_df for HH Smoothed ####
        # relies on measure_reactive value
        if(straight_pipe_hh()) {
            if(input$measure_type == "Count"){
                names(geo_df)[9] <- 'value'
                names(geo_df)[10] <- 'upper'
                names(geo_df)[11] <- 'lower'
                
            } else if (input$measure_type == "Prevalence"){
                names(geo_df)[3] <- 'value'
                names(geo_df)[4] <- 'upper'
                names(geo_df)[5] <- 'lower'
            } else if (input$measure_type  == "Distribution"){
                names(geo_df)[13] <- 'value'
                names(geo_df)[14] <- 'upper'
                names(geo_df)[15] <- 'lower'
            }
        }    
        
        geo_df
    })
    
    geo_year_df_reactive <- reactive({
        
        geo_df <- geo_df_reactive() 
        year <- year_reactive()
        measure <- measure_reactive()
        
        geo_year_df <- geo_df %>% 
            filter(Year %in% year) %>%
            group_by(GEOID) %>%
            mutate(
                prev_total = sum(value)
            ) %>%
            ungroup() 
        
        
        geo_year_df
    })
    
    ## selected_df_reactive ####
    ### Variable Filters ####      
    #ARA: These filter by user-inputted variable specific demographics
    #### Population ####
    selected_df_reactive <- reactive({
        geo_year_df <- geo_year_df_reactive()
        var <- var_reactive()
        if (var == "Population"){
            selected_df <- geo_year_df %>%
                filter(Sex %in% sex_reactive(),
                       Race %in% race_reactive(),
                       Age %in% age_reactive())%>%
                group_by(GEOID) %>%
                summarize(prev_total = unique(prev_total),
                          value = sum(value),
                          moe = sum(moe))
            
        }
        #### Education ####
        else if (var == "Education Level"){
            selected_df <- geo_year_df %>%
                filter(short_label %in% edu_reactive(),
                       Sex %in% sex_reactive(),
                       Race %in% race_reactive()) %>%
                group_by(GEOID) %>%
                summarize(# prev_total = unique(prev_total),
                          value = sum(value),
                          moe = sum(moe))
            
        }           
        #ARA FIX: for HRA, might need to group by HRA 
        #### Rent Burden ####
        else if (var == "Rent Burden"){
            selected_df <- geo_year_df %>%
                
                filter(short_label %in% rent_reactive())%>%
                group_by(GEOID) %>%
                summarize(# prev_total = unique(prev_total),
                          value = sum(value),
                          moe = sum(moe)) 
            
        }
        #### straight pipe hh ####
        else if (straight_pipe_hh()){
            selected_df <- geo_year_df %>%
                filter(hh_size %in% size_reactive(),
                       tenure %in% tenure_reactive())%>%
                group_by(GEOID) %>%
                summarize(value = sum(value))
        } 
        #### Household Size ####
        else if (var == "Household Size") {
            selected_df <- geo_year_df %>%
                filter(short_label %in% tenure_reactive()) %>%
                group_by(GEOID) %>%
                summarize(# prev_total = unique(prev_total),
                          value = sum(value),
                          moe = sum(moe))
        }
        #### Transportation & Age ####
        #Need to Add Race to csv in order to filter by it/maybe add median agee too or just establish another pipeline for an age-specific csv
        else if (var == "Methods of Transportation to Work"){
            selected_df <- geo_year_df %>%
                filter(short_label %in% transp_reactive()) %>% 
                group_by(GEOID) # %>% 
            # summarize(value = sum(value),
            #           moe = sum(moe))
        }
        
        #HRA Var Reactives go here
        else if (var == "Rent Burden"){
            selected_df <- geo_year_df %>%
                filter(short_label %in% rent_reactive())%>%
                group_by(GEOID) %>%
                summarize(# prev_total = unique(prev_total),
                          value = sum(value),
                          moe = sum(moe))
            
        }
        selected_df
    })
    # 
    
    ## measure_df_reactive ####
    
    measure_df_reactive <- reactive({
        selected_df <- selected_df_reactive()
        
        measure <- input$measure_type
        ### Measures ####
        if(!straight_pipe_hh()) {
            ## Measures for HH Size Smoothed 
            # I think this means we need Prevalence to be default
            # measure for HH size smoothed? 
            
            if (measure == "Value") {
                measure_df <- selected_df %>%
                    mutate(
                        value = round(value, 1),
                        SE = round(moe/qnorm(.95), 1)
                    ) %>%
                    ungroup() 
            } 
            else if (measure == "Count") {
                measure_df <- selected_df %>%
                    ungroup() %>% 
                    group_by(GEOID) %>% 
                    mutate(
                        # prev_total = unique(prev_total),
                        value = round(value, 0),
                        SE = round(moe/qnorm(.95), 0)
                    ) %>%
                    ungroup() 
            }
            
            else if (measure == "Prevalence") {
                measure_df <- selected_df %>%
                    group_by(GEOID) %>%
                    mutate(
                        prev_total = unique(prev_total),
                        value = value/prev_total,
                        value  = round(value * 10^4) / 10^2,
                        SE = round((moe/prev_total)/qnorm(.95) * 10^4) / 10^2
                    ) %>%
                    ungroup() 
            }
            else if (measure  == "Distribution"){
                measure_df <- selected_df %>%
                    mutate(
                        moe = round(moe / sum(value) *10^4) / 10^2,
                        value = round(value / sum(value) * 10^4) / 10^2,
                        SE = round(moe/qnorm(.95) * 10^4) / 10^2
                    ) %>%
                    ungroup()
            }
        } 
        
    })
    ## sp_reactive ####
    # generate an sp object with population data 
    # # and related geographic data based on user input
    sp_reactive <- reactive({
        
        measure_df <- measure_df_reactive()        
        geo <- geo_reactive()
        # if "Prevalence" and the whole population are selected, the calculation
        # will not always be 100% but have some small variances (around 0.5%)
        # the code belowe changes them all to 100% to avoid confusion
        # if (measure_reactive() == "Prevalence") {
        #     if (all_selected()) {
        #         selected_df <- selected_df %>%
        #             mutate(value = 100)
        #     }
        # }
        ### Add to Spatial Data ####
        # merge the dataframe with the corresponding geographic level GIS data and return
        if (geo) {
            merge_df_spdf(measure_df, kc_tract_spdf)
        } else {
            merge_df_spdf(measure_df, kc_hra_spdf)
        }
    })
    
    ## legend_title_reactive() ####
    #ARA_ Added branch for creating legend for other variables
    legend_title_reactive <- reactive({
        var <- input$var
        measure <- input$measure_type
        if (measure == "Count") {
            if(var == "Population"){
                "Population"
            }else if(var == "Education Level"){
                "Educational Attainment"
            }else if(var == "Rent Burden"){
                "Renters According to Burden"
            }else if(var == "Household Size"){
                "Households by Size and Tenure"
            }
        }else if (measure == "Prevalence") {
            "Prevalence (%)"
        }else if (measure == "Distribution"){
            "Distribution (%)"
        }else if (measure == "Value") {
            if (var == "Median Income") {
                "Median Income (Dollars)"
            }else if (var == "Methods of Transportation to Work"){
                "Median Age"
            }else if (var == "Median Gross Rent"){
                "Median Gross Rent (Dollars)"
            }
        }
    })
    
    ## Popup/Rollover Text ####
    popup_text_reactive <- reactive({
        var <- input$var
        geo_logical <- input$geo_level == "Census Tract"
        measure <- input$measure_type
        geo_prefix <- ifelse(geo_logical,
                             "Tract No.: ", "HRA: ")
        measure_prefix <- "No. Individuals: "
        if (measure == "Prevalence"){
            measure_prefix <- "Prevalence: "
        }else if (measure == "Distribution"){
            measure_prefix <- "Distribution: "
        }else if (measure  == "Value"){
            if (straight_pipe_hh()) {
                measure_prefix <-  "No. Households: "
            }else if (var == "Median Income"){
                measure_prefix <-  "Median Income: $"
            }else if (var == "Methods of Transportation to Work"){
                measure_prefix <-  "Median Age: "
            }else if (var == "Median Gross Rent"){
                measure_prefix <-   "Median Gross Rent: $"
            }
        }
        
        uncertainty <- "SE: <strong>%g</strong>"
        
        if(straight_pipe_hh()){
            uncertainty <- "(<strong>%g</strong>, <strong>%g</strong>)"
        } 
        
        paste0(geo_prefix,
               "<strong>%s</strong><br/>", # GEOID
               measure_prefix,
               "<strong>%g</strong><br/>", # value
               uncertainty)
    })
    
    ## unique_quantile_length_reactive ####
    
    unique_quant_length_reactive <- reactive({
        sp <- sp_reactive()
        length(unique(quantile(sp@data$value,
                               seq(0, 1, .2),
                               na.rm = TRUE)))
    })
    
    ## legend_values_reactive ####
    
    # calculate the values displayed in the legend
    legend_values_reactive <- reactive({
        sp <- sp_reactive()
        unique_quant_length <- unique_quant_length_reactive()
        
        if (unique_quant_length == 6) {
            quantile(sp@data$value, type = 5, 
                     probs = seq(0, 1, .2),
                     names = FALSE, na.rm = TRUE)
        }else if(unique_quant_length >= 2){
            quantile(sp@data$value, type = 5, 
                     probs = seq(0, 1, .5),
                     names = FALSE, na.rm = TRUE) 
        }else if(unique_quant_length == 1){
            quantile(sp@data$value, type = 5, 
                     probs = c(0,1),
                     names = FALSE, na.rm = TRUE) + c(-0.01, 0.01)
        }
    })
    col_pal_reactive <- reactive({ 
        sp <- sp_reactive()
        pal <- pal_reactive()
        legend_values <- legend_values_reactive()
        
        colorQuantile(
            palette = pal,
            domain = sp@data$value,
            n = length(legend_values) - 1,
            na.color = NA)
    })    
    # Output ####
    output$warning <- renderText({
        warning_text_reactive()
    })
    
    output$age_warning <- renderText({
        age_warning_reactive()
    })
    
    # render the initial basemap and the public facility layers
    ## Base Layers & Map ####
    output$map <- renderLeaflet({
        # load community rail station GIS data
        kc_cr_station_2040 <- readOGR("./data/kc_cr_station_2040.json")
        
        # load light rail station GIS data
        kc_lr_station_2040 <- readOGR("./data/kc_lr_station_2040.json")
        
        # load public clinics GIS data
        kc_public_clinics <- readOGR("./data/kc_public_clinics.json")
        
        # load Women, Infant and Children Services GIS data
        kc_wic <- readOGR("./data/kc_wic.json")
        
        # load Community Health Centers GIS data
        kc_chc <- readOGR("./data/kc_chc.json")
        
        # load school sites data
        kc_schools <- readOGR("./data/kc_schools.json")
        
        # replace code with the corresponding site type name
        kc_schools <- list(
            "School - Elementary" = kc_schools[kc_schools$CODE == "School - Elementary", ],
            "School - Junior High or Middle" = kc_schools[kc_schools$CODE == "School - Junior High or Middle", ],
            "School - High" = kc_schools[kc_schools$CODE == "School - High", ],
            "School - College or University" = kc_schools[kc_schools$CODE == "School - College or University", ],
            "School - Alternative" = kc_schools[kc_schools$CODE == "School - Alternative", ],
            "School - Other facility" = kc_schools[kc_schools$CODE == "School - Other facility", ],
            "School - K thru 12" = kc_schools[kc_schools$CODE == "School - K thru 12", ]
        )
        
        # remove the loading page and show the main content
        shinyjs::hideElement(id = "initializing_page")
        shinyjs::showElement(id = "main_content")
        
        # leaftlet() ####
        leaflet() %>%
            addProviderTiles(
                providers$CartoDB.Positron,
                options = providerTileOptions(
                    minZoom = 9,
                    maxZoom = 15
                )
            ) %>%
            setView(
                lng = -121.810721,
                lat = 47.412716,
                zoom = 9
            ) %>%
            setMaxBounds(
                -123.222921, 48.300822,
                -120.383728, 46.652146
            ) %>%
            # define z-indexes manually for the polygons, markers, lines added to the map
            # polygon(bottom), line(middle), marker(top)
            addMapPane(
                name = "layer1",
                zIndex = "411"
            ) %>%
            addMapPane(
                name = "layer2",
                zIndex = "412"
            ) %>%
            addMapPane(
                name = "layer3",
                zIndex = "413"
            ) %>%
            addMapPane(
                name = "layer4",
                zIndex = "414"
            ) %>%
            addMapPane(
                name = "layer5",
                zIndex = "415"
            ) %>%
            addMapPane(
                name = "layer6",
                zIndex = "416"
            ) %>%
            addMapPane(
                name = "layer7",
                zIndex = "417"
            ) %>%
            addMapPane(
                name = "layer8",
                zIndex = "418"
            ) %>%
            addMapPane(
                name = "layer9",
                zIndex = "419"
            ) %>%
            addMapPane(
                name = "layer10",
                zIndex = "420"
            ) %>%
            addMapPane(
                name = "layer11",
                zIndex = "421"
            ) %>%
            addMapPane(
                name = "layer12",
                zIndex = "422"
            ) %>%
            addMapPane(
                name = "layer13",
                zIndex = "423"
            ) %>%
            addMapPane(
                name = "layer14",
                zIndex = "424"
            ) %>%
            ## Add public facility layers ####
        addMarkers(
            data = kc_public_clinics,
            icon = makeIcon(
                iconUrl = "https://img.icons8.com/metro/26/000000/hospital.png",
                iconWidth = 15,
                iconHeight = 15
            ),
            group = "Public Health Clinics (2018)",
            popup = sprintf(
                "Name: <strong>%s</strong></br>Address: %s</br>Zip Code: %s",
                kc_public_clinics$NAME,
                kc_public_clinics$ADDRESS,
                kc_public_clinics$ZIPCODE
            ) %>%
                lapply(htmltools::HTML),
            options = pathOptions(pane = "layer3")
        ) %>%
            addMarkers(
                data = kc_wic,
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/hospital.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = "Women, Infant and Children Services (2020)",
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Address: %s</br>",
                    kc_wic$Name,
                    kc_wic$Address
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer4")
            ) %>%
            addMarkers(
                data = kc_chc,
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/hospital.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = "Community Health Centers (2020)",
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Address: %s</br>",
                    kc_chc$Name,
                    kc_chc$Address
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer5")
            ) %>%
            addMarkers(
                data = kc_schools[[1]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[1], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[1]]$NAME,
                    kc_schools[[1]]$CODE,
                    kc_schools[[1]]$DISTRICT,
                    kc_schools[[1]]$ADDRESS,
                    kc_schools[[1]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer6")
            ) %>%
            addMarkers(
                data = kc_schools[[2]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[2], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[2]]$NAME,
                    kc_schools[[2]]$CODE,
                    kc_schools[[2]]$DISTRICT,
                    kc_schools[[2]]$ADDRESS,
                    kc_schools[[2]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer7")
            ) %>%
            addMarkers(
                data = kc_schools[[3]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[3], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[3]]$NAME,
                    kc_schools[[3]]$CODE,
                    kc_schools[[3]]$DISTRICT,
                    kc_schools[[3]]$ADDRESS,
                    kc_schools[[3]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer8")
            ) %>%
            addMarkers(
                data = kc_schools[[4]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[4], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[4]]$NAME,
                    kc_schools[[4]]$CODE,
                    kc_schools[[4]]$DISTRICT,
                    kc_schools[[4]]$ADDRESS,
                    kc_schools[[4]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer9")
            ) %>%
            addMarkers(
                data = kc_schools[[5]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[5], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[5]]$NAME,
                    kc_schools[[5]]$CODE,
                    kc_schools[[5]]$DISTRICT,
                    kc_schools[[5]]$ADDRESS,
                    kc_schools[[5]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer10")
            ) %>%
            addMarkers(
                data = kc_schools[[6]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[6], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[6]]$NAME,
                    kc_schools[[6]]$CODE,
                    kc_schools[[6]]$DISTRICT,
                    kc_schools[[6]]$ADDRESS,
                    kc_schools[[6]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer11")
            ) %>%
            addMarkers(
                data = kc_schools[[7]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[7], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[7]]$NAME,
                    kc_schools[[7]]$CODE,
                    kc_schools[[7]]$DISTRICT,
                    kc_schools[[7]]$ADDRESS,
                    kc_schools[[7]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer12")
            ) %>%
            addMarkers(
                data = kc_cr_station_2040,
                group = "Commuter Rail Stations (2040)",
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/windows/32/000000/city-railway-station.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                popup = sprintf(
                    "Station Name: <strong>%s</strong>",
                    kc_cr_station_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer13")
            ) %>%
            addMarkers(
                data = kc_lr_station_2040,
                group = "Light Rail Stations (2040)",
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/windows/32/000000/city-railway-station.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                popup = sprintf(
                    "Station Name: <strong>%s</strong>",
                    kc_lr_station_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer14")
            ) %>%
            addPolylines(
                data = kc_tl_2040,
                group = "Transit Lines (2040)",
                color = "#62AC55",
                weight = 3,
                opacity = 0.2,
                popup = sprintf(
                    "Transit Line Name: <strong>%s</strong>",
                    kc_tl_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer2")
            ) %>%
            addLayersControl(
                overlayGroups = c(
                    "Public Health Clinics (2018)",
                    "Community Health Centers (2020)",
                    "Women, Infant and Children Services (2020)",
                    paste(names(kc_schools), "(2018)"),
                    "Commuter Rail Stations (2040)",
                    "Light Rail Stations (2040)",
                    "Transit Lines (2040)"
                ),
                options = layersControlOptions(collapsed = TRUE)
            ) %>%
            hideGroup(
                c(
                    "Public Health Clinics (2018)",
                    "Community Health Centers (2020)",
                    "Women, Infant and Children Services (2020)",
                    paste(names(kc_schools), "(2018)"),
                    "Commuter Rail Stations (2040)",
                    "Light Rail Stations (2040)",
                    "Transit Lines (2040)"
                )
            )
    })
    
    # this function updates the map based on user input
    # Begin shape observe() ####
    observe({
        shinyjs::showElement(id = 'loading')
        
        var <- var_reactive()
        geo <- geo_reactive()
        measure <- measure_reactive()
        
        year <- year_reactive()
        
        geo_df <- geo_df_reactive()
        geo_year_df <- geo_year_df_reactive()
        selected_df <- selected_df_reactive()
        measure_df <- measure_df_reactive()
        
        ## sp_reactive() ####
        sp <- sp_reactive()
        
        ## proxy_map ####
        leafletProxy(
            "map",
            data = sp
        ) %>%
            # clear the existing shapes and legend
            clearShapes() %>%
            clearControls() %>%
            # since polyline will also be removed by the clearShape() function
            # add the layer again here
            addPolylines(
                data = kc_tl_2040,
                group = "Transit Lines (2040)",
                color = "#62AC55",
                weight = 3,
                opacity = 0.2,
                popup = sprintf(
                    "Transit Line Name: <strong>%s</strong>",
                    kc_tl_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer2")
            )
        
    })
    
    # Begin color/legend observe() ####
    observe({  
        ## map aesthetics ####
        sp <- sp_reactive()
        pal <- pal_reactive()
        legend_title <- legend_title_reactive()
        popup <- popup_text_reactive()
        unique_quant_length <- unique_quant_length_reactive()
        legend_values <- legend_values_reactive()
        col_pal <- col_pal_reactive()
        
        # proxy_map old ####
        
        # Create all_selected ####
        # all_selected <- FALSE
        # if (all_selected()) {
            if (1>2) {
                #     ## Example map ####
                #     proxy_map <- proxy_map %>%
                #         addPolygons(
                #             layerId = ~GEOID,
                #             color = "#606060",
                #             weight = 1,
                #             smoothFactor = 0.5,
                #             opacity = 0.9,
                #             fillOpacity = 0.6,
                #             fillColor = ~ colorNumeric(
                #                 palette = "#08519C",
                #                 domain = c(100)
                #             )(value),
                #             highlightOptions = highlightOptions(
                #                 color = "white", weight = 2,
                #                 bringToFront = TRUE
                #             ),
                #             ### Pop up sprintf() call ####
                #             label = sprintf(
                #                 popup,
                #                 sp$GEOID,
                #                 sp$value,
                #                 sp$SE
                #             ) %>%
                #                 lapply(htmltools::HTML),
                #             labelOptions = labelOptions(
                #                 style = list("font-weight" = "normal", padding = "3px 8px"),
                #                 textsize = "15px",
                #                 direction = "auto"
                #             ),
                #             options = pathOptions(pane = "layer1")
                #         ) %>%
                #         addLegend(
                #             pal = colorNumeric(
                #                 palette = "#08519C",
                #                 domain = c(100)
                #             ),
                #             values = c(100),
                #             opacity = 0.7,
                #             title = legend_title_reactive(),
                #             position = "bottomright"
                #         )
            } else {
                ## Map ####
                ### Polygon Colors ####
                #define the color palette for filling the polygons based on population
                #ARA_ changed N from 5 to 2
                #ARA_ Added unique quant length for quantile 
                # unique_quant_length <- length(unique(quantile(sp@data$value,
                #                                               seq(0,1,.2), na.rm = TRUE)))
                # 
                
                
                ## Update map, sprintf() for rollover ####
                # add the new population data to the map
                if(straight_pipe_hh()){
                    ### HH Size (Smoothed) ####
                    proxy_map <- proxy_map %>%
                        addPolygons(
                            layerId = ~GEOID,
                            color = "#606060",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 0.9,
                            fillOpacity = 0.6,
                            fillColor = ~ col_pal(value),
                            highlightOptions = highlightOptions(
                                color = "white", weight = 2,
                                bringToFront = TRUE
                            ),
                            label = sprintf(
                                popup,
                                sp$GEOID,
                                sp$value,
                                sp$lower,
                                sp$upper
                            ) %>%
                                lapply(htmltools::HTML),
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"
                            ),
                            options = pathOptions(pane = "layer1")
                        )%>%
                        addLegend(
                            pal = col_pal,
                            values = ~value,
                            opacity = 0.7,
                            labFormat = {
                                function(type, cuts, p) {
                                    n <- length(cuts)
                                    lower <- as.integer(cuts)[-n]
                                    if (-n != 1) {
                                        lower <- as.integer(cuts)[-n] + 1
                                    }
                                    upper <- as.integer(cuts)[-1]
                                    paste0(lower, " - ", upper, " (", seq(0, 100, length.out = n)[-n], "th PCTL)")
                                }
                            },
                            title = legend_title,
                            position = "bottomright"
                        )
                }else{
                    
                    ## Not HH Size (Smoothed) ####
                    proxy_map <- leafletProxy(
                        "map",
                        data = sp) 
                    
                    proxy_map %>% 
                        clearShapes() %>% 
                        clearControls() %>% 
                        addPolygons(
                            layerId = ~GEOID,
                            color = "#606060",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 0.9,
                            fillOpacity = 0.6,
                            fillColor = ~col_pal(value), ## JLG COME BACK!
                            highlightOptions = highlightOptions(
                                color = "white", weight = 2,
                                bringToFront = TRUE
                            ),
                            label = sprintf(
                                popup,
                                sp$GEOID,
                                sp$value,
                                sp$SE
                            ) %>%
                                lapply(htmltools::HTML),
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"
                            ),
                            options = pathOptions(pane = "layer1")
                        )%>%
                        addLegend(
                            pal = col_pal,
                            values = ~value,
                            opacity = 0.7,
                            labFormat = {
                                function(type, cuts, p) {
                                    n <- length(cuts)
                                    lower <- as.integer(cuts)[-n]
                                    if (-n != 1) {
                                        lower <- as.integer(cuts)[-n] + 1
                                    }
                                    upper <- as.integer(cuts)[-1]
                                    paste0(lower, " - ", upper, " (", 
                                           seq(0, 100, length.out = n)[-n],
                                           "th PCTL)")
                                }
                            },
                            title = legend_title_reactive(),
                            position = "bottomright"
                        )
                }
            }
        
        
        Sys.sleep(1)
        
        shinyjs::hideElement(id = 'loading')
    })
    
    
    #-----------------plot----------
    #     clicked_geo <- reactiveValues(df = NULL)
    # 
    #     # if not clicking on a polygon, remove the clicked polygon info
    #     observeEvent(input$map_click,
    #                  {
    #                      print(paste("MAP", input$map_click))
    #                      clicked_geo$df <- NULL
    #                  },
    #                  priority = 100
    #     )
    # 
    #     # if clicking on a polygon, save the clicked polygon info
    #     observeEvent(input$map_shape_click,
    #                  {
    #                      print(paste("MAP SHAPE", input$map_shape_click))
    #                      if (!is.null(input$map_shape_click$id)) {
    #                         if (geo_reactive()) {
    #                             clicked_geo$df <- tract_proj %>%
    #                                 filter(GEOID == input$map_shape_click$id)
    #                         } else {
    #                             clicked_geo$df <- hra_proj %>%
    #                                 filter(GEOID == input$map_shape_click$id)
    #                         }
    #                     }
    #                  },
    #                  priority = 99
    #     )
    # 
    #     # generate a dataframe for the line chart
    #     df_reactive <- reactive({
    #         # used the clicked polygon
    #         df <- clicked_geo$df
    # 
    #         # if no polygon is clicked, use the county total data
    #         if (is.null(df)) {
    #             if (geo_reactive()) {
    #                 df <- tract_proj
    #             } else {
    #                 df <- hra_proj
    #             }
    #         }
    # # ARA: trying to comment out GEOID selection here so I can execute dist calc
    #  df <- df %>%
    #      select(-GEOID)
    # 
    #         # calculate the data based on user input; similar to the process in sp_reactive()
    #         if (measure_reactive() == "Count") {
    #             df <- df %>%
    #                 group_by(Year, Age5, Sex, Race) %>%
    #                 summarize(value = sum(value))
    #         } else if (measure_reactive() == "Prevalence") {
    # 
    #             df <- df %>%
    #                 group_by(Year, Age5, Sex, Race) %>%
    #                 summarize(value = sum(value)) %>%
    #                 group_by(Year) %>%
    #                 mutate(
    #                     Prevalence = round(value / sum(value) * 100, 2)
    #                 ) %>%
    #                 select(-value)
    #              colnames(df)[6] <- "value"
    #         }
    # 
    #         df <- df %>%
    #             filter(
    #                 Age5 %in% age_reactive(),
    #                 Sex %in% sex_reactive()
    #             ) %>%
    #             group_by(Year, Race) %>%
    #             summarize(value = sum(value))
    # 
    #         # add rows for the total population of all race and ethnicity categories
    #         df <- rbind(
    #             df %>%
    #                 mutate(Race = "Total") %>%
    #                 group_by(Year, Race) %>%
    #                 summarize(value = sum(value)) %>%
    #                 arrange(Year),
    #             df %>%
    #                 arrange(Race, Year)
    #         )
    # 
    #         df
    #     })
    # 
    # 
    #     output$plot <- renderPlotly({
    #         df <- df_reactive()
    # 
    #         yr = as.integer(year_reactive())
    # 
    # 
    #         P <- plot_ly(
    #             type = "scatter",
    #             mode = "lines"
    #         ) %>%
    #             # add the vertical dash line at 2020
    #             layout(
    #                 yaxis = list(rangemode = "tozero"),
    #                 shapes = list(
    #                     list(
    #                         type = "line",
    #                         y0 = 0,
    #                         y1 = 1,
    #                         yref = "paper",
    #                         x0 = 2020,
    #                         x1 = 2020,
    #                         line = list(
    #                             dash = "dash",
    #                             width = 2,
    #                             color = "black"
    #                         )
    #                     )
    #                 )
    #             )
    # 
    #         # pre-define the colors for drawing lines for different race and ethnicity categories
    #         col_pal <- c(
    #             c(
    #                 "rgba(1,1,1,1)",
    #                 "rgba(127,201,127,1)",
    #                 "rgba(190,174,212,1)",
    #                 "rgba(253,192,134,1)",
    #                 "rgba(255,255,153,1",
    #                 "rgba(56,108,176,1)",
    #                 "rgba(240,2,127,1)",
    #                 "rgba(191,91,23,1)"
    #             )
    #         )
    # 
    #         index <- NULL
    #         races <- unique(df$Race)
    # 
    #         selected_race <- "Total"
    # 
    #         if (length(race_reactive()) != 7) {
    #             selected_race <- race_reactive()
    #         }
    # 
    #         # draw the lines for the unselected race and ethnicity categories and hide them first
    #         for (i in 1:length(races)) {
    #             curr_race <- races[i]
    # 
    #             if (curr_race != selected_race) {
    #                 pop <- filter(df, Race == curr_race)$value
    # 
    #                 P <- add_trace(
    #                     P,
    #                     x = ~ unique(df$Year),
    #                     y = pop,
    #                     name = curr_race,
    #                     line = list(
    #                         color = col_pal[i],
    #                         width = 2
    #                     ),
    #                     visible = "legendonly"
    #                 )
    #             } else {
    #                 index <- i
    #             }
    #         }
    # 
    #         temp_yval <- filter(filter(df, Race == selected_race), Year == yr)[["value"]]
    # 
    #         # draw the line for the selected race/ethnicity
    #         P <- add_trace(
    #             P,
    #             x = ~ unique(df$Year),
    #             y = ~ filter(df, Race == selected_race)$value,
    #             name = selected_race,
    #             line = list(
    #                 color = col_pal[index],
    #                 width = 4
    #             )
    #         )
    # 
    #         # add titles
    #         P <- layout(
    #             P,
    #             title = ifelse(
    #                 is.null(clicked_geo$df),
    #                 "County-Level Population of the Selected Groups",
    #                 paste0(
    #                     "Population of the Selected Groups, Selected ",
    #                     ifelse(
    #                         geo_reactive(),
    #                         "Tract (GEOID: ",
    #                         "HRA ("
    #                     ),
    #                     clicked_geo$df$GEOID[1],
    #                     ")"
    #                 )
    #             ),
    #             xaxis = list(
    #                 title = "Year",
    #                 tickformat = "K"
    #             ),
    #             yaxis = list(
    #                 title = ifelse(
    #                     measure_reactive() == "Count",
    #                     "Population",
    #                     "Population (%)"
    #                 )
    #             )
    #         )
    # 
    #         P
    #     })
    # 
    #     # generate a button for going back to the county-level data if a polygon is clicked
    #     output$reset_chart_button <- renderUI(
    #         if (!is.null(clicked_geo$df)) {
    #             actionButton(
    #                 inputId = "reset_line_chart",
    #                 label = "Go Back to County-Level Data"
    #             )
    #         }
    #     )
    # 
    #     # generate a button for quickly selecting all age groups when not all age groups are selected
    #     output$all_age_button <- renderUI(
    #         if (length(age_reactive()) != 18) {
    #             actionButton(
    #                 inputId = "all_age",
    #                 label = "Select All Age Groups"
    #             )
    #         }
    #     )
    # 
    #     # if "Select All Age Groups" button is clicked, update the input
    #     observeEvent(input$all_age, {
    #         updateSliderTextInput(
    #             session,
    #             inputId = "age",
    #             label = "Age Range",
    #             choices = c(seq(0, 85, 5), "85+"),
    #             selected = c("0", "85+")
    #         )
    #     })
    # 
    #     # if "Go Back to County-Level Data" button is clicked, set the clicked polygon to NULL
    #     observeEvent(input$reset_line_chart, {
    #         clicked_geo$df <- NULL
    #     })
    # 
    #     # define the helper text the visualizations
    #     selected_charac_html_text <- reactiveValues()
    # 
    #     observe({
    #         sex <- "female and male"
    #         if (length(sex_reactive()) == 1) {
    #             sex <- tolower(sex_reactive())
    #         }
    # 
    #         race <- "population of all racial and ethnic groups"
    # 
    #         if (length(race_reactive()) == 1 ) {
    #             race <- paste0(race_reactive(), " population")
    #         }
    # 
    #         age <- "NA"
    #         lower <- "NA"
    #         upper <- "NA"
    # 
    #         l = length(age_reactive())
    #         if (l != 0) {
    #             lower <- str_split(age_reactive()[1], "-")[[1]][1]
    #             upper <- age_reactive()[l]
    #             if (upper == "85+") {
    #                 age <- paste0(
    #                     "[",
    #                     lower,
    #                     "-85+)"
    #                 )
    #             } else {
    #                 upper <- as.integer(str_split(age_reactive()[l], "-")[[1]][2]) + 1
    #                 age <- paste0(
    #                     "[",
    #                     lower,
    #                     "-",
    #                     upper,
    #                     ")"
    #                 )
    #             }
    #         }
    # 
    # 
    #         selected_charac_html_text$map <- paste0(
    #             "Currently displaying <strong> ",
    #             tolower(var_reactive()), " ",
    #             tolower(measure_reactive()), "s",
    #             "</strong> for the <strong>",
    #             sex, " ", race,
    #             "</strong> aged <strong>",
    #             age,
    #             "</strong> for the year <strong>",
    #             year_reactive(),
    #             "</strong> at the <strong>",
    #             ifelse(
    #                 geo_reactive(),
    #                 "census tract",
    #                 "HRA"
    #             ),
    #             "</strong> level."
    #         )
    # 
    #         selected_charac_html_text$plot <- paste0(
    #             "Currently displaying <strong>population ",
    #             tolower(measure_reactive()), "s",
    #             "</strong> for the <strong>",
    #             sex, " ", race,
    #             "</strong> aged <strong>",
    #             age,
    #             "</strong>."
    #         )
    # 
    #         if (length(age_reactive()) == 18) {
    #             selected_charac_html_text$age <- "All age groups are selected"
    #         } else {
    #             if (upper == "85+") {
    #                 selected_charac_html_text$age <- paste0(
    #                     "Selected Age Range: ",
    #                     age,
    #                     "</br>(i.e. ",
    #                     lower,
    #                     " ≤ Selected Ages)"
    #                 )
    #             } else {
    #                 selected_charac_html_text$age <- paste0(
    #                     "Selected Age Range: ",
    #                     age,
    #                     "</br>(i.e. ",
    #                     lower,
    #                     " ≤ Selected Ages < ",
    #                     upper,
    #                     ")"
    #                 )
    #             }
    # 
    #         }
    #     })
    # 
    #     observe({
    #         addTooltip(
    #             session,
    #             id = "age",
    #             title = selected_charac_html_text$age,
    #             placement = "top",
    #             options = list(html = TRUE)
    #         )
    #     })
    # 
    #     observe({
    #         addTooltip(
    #             session,
    #             id = "map",
    #             title = selected_charac_html_text$map,
    #             placement = "right",
    #             options = list(html = TRUE)
    #         )
    #     })
    # 
    #     observe({
    #         addTooltip(
    #             session,
    #             id = "plot",
    #             title = selected_charac_html_text$plot,
    #             placement = "right",
    #             options = list(html = TRUE)
    #         )
    #     })
    
    
    # outputOptions() ####
    # preload the visualization once the website is opened
    outputOptions(output, "map", suspendWhenHidden = FALSE)
    # outputOptions(output, "plot", suspendWhenHidden = FALSE)
}