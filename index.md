---
title: PHI 2021
layout: default
---

# Table of Contents

* [Demography](#demography)
* [Demographic Data](#census-and-demographic-data)
* [Statistics](#statistics)
* [Housing and households](#housing-and-households)
* [Disaster and Resiliency](#disaster-and-resiliency)
* [COVID-19](#covid-19)

# Demography

## Population Growth and Fertility Transitions

**Introduction to Demography: WPP data and R graphics**
  * [Slides](https://jlgodwin.github.io/PHI2021/Lectures/IntrotoDem_20210629.pdf) | [Annotated Slides](https://jlgodwin.github.io/PHI2021/Lectures/IntrotoDem_20210629_Annotated.pdf) | [Recording](https://washington.zoom.us/rec/share/2N6cPR41MsDSz3q7l3DDvy4R1fq560sLAzfA_VL9QIxoND1EAqnwml7TcERONWfP.nK4Yo3_xgVECg1SR?startTime=1625066602000) | [Slides.Rmd](https://jlgodwin.github.io/PHI2021/Lectures/IntrotoDem_20210629.Rmd)


**The Population "Bomb"**
   * [Global warming and population growth](https://jlgodwin.github.io/PHI2021/Readings/BongaartsONeill2018.pdf)
   * [How we survived the population bomb](https://jlgodwin.github.io/PHI2021/Readings/Lam2011SurvivingPopBomb.pdf), [Have we survived?](https://jlgodwin.github.io/PHI2021/Readings/BeckerReplytoLam2013.pdf)
   * [World stabilization unlikely in this century](https://jlgodwin.github.io/PHI2021/Readings/Gerland2014.pdf)
   
**Population Projections**
   * [Slides](https://jlgodwin.github.io/PHI2021/Lectures/Population_Projections_Overview.pdf) | [Recording--Cohort Change Ratios and Hamilton-Perry with Crystal Yu](https://washington.zoom.us/rec/share/JrhvDjcIVRAARF1Wbrzx9Mk6XynPM6uSWziziBYEDf4CtJjzhXvKbmgPdCH5jFVL.dwSvG-R3FDiqVaYb?startTime=1625077413000)

**King County/Seattle growth**
  * [Tech clusters](https://jlgodwin.github.io/PHI2021/Readings/Kerr2020.pdf)

## Census and Demographic data

**Demographic Data Sources**
  * [Slides by Neal Marquez + Me](https://jlgodwin.github.io/PHI2021/Lectures/DemographicDataSources.pdf)
  * [PUMS Data Dictionary](https://jlgodwin.github.io/PHI2021/Readings/PUMS_Data_Dictionary_20152019.pdf) | [ACS MOE documentation](https://jlgodwin.github.io/PHI2021/Readings/ACS_Error.pdf)

**R Tutorials**
   * [tidycensus by Neal Marquez and Connor Gilroy](https://jlgodwin.github.io/PHI2021/Tutorials/tidycensus-tutorial.rmd) | [Household size by Adrien Allorant](https://jlgodwin.github.io/PHI2021/Tutorials/0_ACS_data_inspection.R)
   * [IPUMS-NHGIS](https://jlgodwin.github.io/PHI2021/Tutorials/NHGIS.html) | [NHGIS.Rmd](https://jlgodwin.github.io/PHI2021/Tutorials/NHGIS.Rmd) | [IPUMS-NHGIS extract codebook](https://jlgodwin.github.io/PHI2021/Tutorials/nhgis0002_ts_nominal_county_codebook.txt)
   * IPUMSUSA | [IPUMSUSA.Rmd](https://jlgodwin.github.io/PHI2021/Tutorials/IPUMSUSA.Rmd) | [Extract codebook xml](https://jlgodwin.github.io/PHI2021/Tutorials/usa_00001.xml) | [Extract codebook printed to pdf](https://jlgodwin.github.io/PHI2021/Tutorials/usa_00001.pdf) 
   * [tract_to_HRA](https://jlgodwin.github.io/PHI2021/Tutorials/tract_to_HRA.html) | [tract_to_HRA.Rmd](https://jlgodwin.github.io/PHI2021/Tutorials/tract_to_HRA.Rmd) | [tract_to_hra.rda](https://jlgodwin.github.io/PHI2021/Data/tracts_to_hra.rda) | [Example Plot: Pop age 70-75, 2019](https://jlgodwin.github.io/PHI2021/PopPlots/ACS5_2019_age70.pdf)

# Statistics

**Intro to Survey Statistics**
  * [Slides](https://jlgodwin.github.io/PHI2021/Lectures/SurveyStatistics_20210707.pdf) | [Recording I: up to design-based (long)](https://washington.zoom.us/rec/play/mMe0TIT2WGp-uJ5tO0QkfnZ02DdHavr7KAA_EHHCmgiSNtY48sUnvgQxmxSHXEPNBRf0wwPMtyzR12QO.-5sGPdjCRj7tpff2) [Recording II: design-based and model-based (short)](https://washington.zoom.us/rec/play/X6bKP7a24MO2oQ9EeMhWpO6mcqbQTQNkJScZyEObyhko23pnP9gu0ccV9cpuYzUmnBLlGvo3XenyIVRf.BW-WU4q44gs1yUtG) | [Annotated Slides](https://jlgodwin.github.io/PHI2021/Lectures/SurveyStatistics_20210707_Annotated.pdf)

**Small Area Estimation**
  * [Forecasting and SAE](https://jlgodwin.github.io/PHI2021/Readings/Wilson2021_Article_MethodsForSmallAreaPopulationF.pdf)

**Replicate weights**
 
# Housing and households

**Household formation and size**
  * [Joint and stem families](https://jlgodwin.github.io/PHI2021/Readings/Ruggles2010.pdf) | [Intergenerational Ties](https://jlgodwin.github.io/PHI2021/Readings/IntergenerationalTies_2020.pdf) | [Proximity to kin and mobility](https://jlgodwin.github.io/PHI2021/Readings/Spring_Kin_Mobility_2017.pdf) | [Grandfamilies and the Opioid crisis](https://jlgodwin.github.io/PHI2021/Readings/Grandfamilies_opioid_2021.pdf)
  * [Global fertility preferences](https://jlgodwin.github.io/PHI2021/Readings/BongaartsCasterline2018.pdf) | [US and divorce](https://jlgodwin.github.io/PHI2021/Readings/Ruggles2014.pdf) | [Demographic perspectives on familiy change](https://jlgodwin.github.io/PHI2021/Readings/Bianchi2014.pdf)

**Evictions**
  * [Looming Eviction Cliff](https://jlgodwin.github.io/PHI2021/Readings/TheLoomingEvictionCliff.pdf) | [US 2020 Eviction Trends](https://jlgodwin.github.io/PHI2021/Readings/USEvictionFilingPatternsin2020.pdf) | [Right to Counsel-NYC](https://jlgodwin.github.io/PHI2021/Readings/PreliminaryEffectsofRighttoCounselonEvictions.pdf) | [Predictors of Eviction](https://jlgodwin.github.io/PHI2021/Readings/PredictorsofEviction.pdf)

**Race and housing burden**
  * [Foreclosure migration and segregation](https://jlgodwin.github.io/PHI2021/Readings/Hall_Foreclosures_Race_2018.pdf) | [Race and Housing Costs](https://jlgodwin.github.io/PHI2021/Readings/Hess_Race_HousingBurden_2021.pdf)

**R**
  * [Household size by Adrien Allorant](https://jlgodwin.github.io/PHI2021/Tutorials/0_ACS_data_inspection.R)
 
# Disaster and Resiliency

* [Making Healthy Places-Resiliency to Disaster](https://jlgodwin.github.io/PHI2021/Readings/MakingHealthyPlaces_ResiliencytoDisaster_Beatley2011.pdf) | [Resilience and disaster risk reduction](https://jlgodwin.github.io/PHI2021/Readings/ResilienceandDisaster_Alexander_2013.pdf) | [Disaster Resiliency-Concepts, Measures, and Critques](https://jlgodwin.github.io/PHI2021/Readings/Ch7_DisasterResilience_Tierney_2019.pdf) 
* [Neigborhoods, Race, Environmental Hazards, Inequality](https://jlgodwin.github.io/PHI2021/Readings/Crowder_MigrationRaceEnvironmentalHazards_2010.pdf) | [Family, Mobility, Environmental Inequality](https://jlgodwin.github.io/PHI2021/Readings/Downey_Family_Mobility_EnvironmentalInequality_2017.pdf) 
* [Global warming and population growth](https://jlgodwin.github.io/PHI2021/Readings/BongaartsONeill2018.pdf) | [Population and climate change](https://jlgodwin.github.io/PHI2021/https://github.com/jlgodwin/PHI2021/Readings/vanDalen2021.pdf) 

# COVID-19
* [COVID19 and the built environment](https://jlgodwin.github.io/PHI2021/Readings/Frumkin_2021_COVID_BuiltEnvironment.pdf) | [COVID19 and the built environment: King County case study](https://jlgodwin.github.io/PHI2021/Readings/Liu_KC_Covid_BuiltEnvironment.pdf)
* [Eviction Moratoria and COVID Mortality](https://jlgodwin.github.io/PHI2021/Readings/EvictionMoratoriumMortality_2021.pdf)
*  [Global COVID orphanhood](https://jlgodwin.github.io/PHI2021/Readings/COVID_orphanhood_global_2021.pdf) | [Supporting orphaned youth](https://jlgodwin.github.io/PHI2021/Readings/OrphanedYouthCOVID_Lancet_2021.pdf)
