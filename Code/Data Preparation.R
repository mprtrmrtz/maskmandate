
#*******************************************************************************
# Housekeeping Items ===========================================================
#*******************************************************************************

rm(list = ls())

# This is just a test to see if it worked fine with G Drive
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
library(ggmap)
library(sf)
library(data.table)
library(bit64)
library(readxl)


setwd("G:/My Drive/Research/Project/")
getwd()





#*******************************************************************************
# Data Importation   ===========================================================
#*******************************************************************************




mask_use <- fread("Data/mask-use-by-county.csv")
COVID_closures <- fread("Data/US County Level Summaries.csv")
FIPS_master <- fread("Data/county_fips_master.csv")
Education_level <- fread("Data/Education level by Country 1970 - 2019.csv")
Election_decisions <- fread("Data/election decisions by fips country.csv")
Pop_estimates <- fread("Data/PopulationEstimates per country 2019.csv")
Poverty_estimate <- fread("Data/PovertyEstimates per country 2019.csv")
Presidential_elections <- fread("Data/presidential election results per fips county.csv")
Unemployment_Income <- readxl::read_excel("Data/Unemployment.xlsx")
FIPS_to_longlat <- fread("Data/2020_Gaz_counties_national.txt")
movement_range <- fread("Data/movement-range-2021-05-23.txt")
Population_density <- fread("Data/Population_density.csv")
Rural_percentage <- fread("Data/Rureal_Percentages_csv.csv") 
# crime <- fread("Data/crime_data_w_population_and_crime_rate.csv")
Fed_Investment <- fread("Data/IV Regression/merged_FIPS.csv")
# mixed_movement_data <- fread("Data/mixed_movement_data.csv")

# COVID Rate Data 
COVID_essential_workers <- fread("Data/COVID_Data/data/context_essential_workers_acs-2021-05-29.csv") 
COVID_testing_clinics <- fread("Data/COVID_Data/data/COVID_testing_clinics_PYTHON.csv") 
COVID_hospital_locations <- fread("Data/COVID_Data/data/COVID_Hospitals_PYTHON.csv") 
COVID_confirmed_cases <- fread("Data/COVID_Data/data/covid_confirmed_cdc-2021-05-29.csv") 
COVID_death_cases <- fread("Data/COVID_Data/data/covid_deaths_cdc-2021-05-29.csv")
COVID_testing_counts <- fread("Data/COVID_Data/data/covid_testing_cdc-2021-05-29.csv") 
COVID_testing_capacity <- fread("Data/COVID_Data/data/covid_tcap_cdc-2021-05-29.csv") 
COVID_testing_positivity <- fread("Data/COVID_Data/data/covid_wk_pos_cdc-2021-05-29.csv") 
Vaccine_master <- fread("Data/COVID_Data/data/COVID-19_Vaccinations_in_the_United_States_County (3).csv")

#*******************************************************************************
# Data Manipulation   ==========================================================
#*******************************************************************************


# Making sure we have the same FIPS code in all dataset with type double 

## mask_use ----
str(mask_use)
colnames(mask_use) <- paste("Mask_Usage", colnames(mask_use), sep = "_")
mask_use$FIPS <- as.double(mask_use$Mask_Usage_COUNTYFP)
mask_use$Mask_Usage_COUNTYFP <- NULL
mask_use$Mask_Usage_Combined <- mask_use[, c(1)] * 0 + mask_use[, c(2)] * 2 +
                                  mask_use[, c(3)] * 3 + mask_use[, c(4)] * 4 +
                                  mask_use[, c(5)] * 5



# Crime
# FIPS_master$county_name <- paste(FIPS_master$county_name, FIPS_master$state_abbr, sep = ", ")
# Crime <- merge(FIPS_master, crime, on = "county_name")
# Crime <- Crime[, -c(1, 3:13, 15:36)]
# colnames(Crime) <- c('FIPS', 'Crime_per_100K')


##  FIPS_master -----
FIPS_master$FIPS <- as.double(FIPS_master$fips)
FIPS_master$fips <- NULL
FIPS_master <- FIPS_master[, -c(4:12)]




## Education Level ----
colnames(Education_level) <- paste("Education_Levels", colnames(Education_level), sep = "_")
Education_level$FIPS <- as.double(Education_level$`Education_Levels_FIPS Code`)
Education_level$`Education_Levels_FIPS Code` <- NULL
Education_level <- Education_level[, -c(0:42)]

colnames(Education_level) <- c('Percent_Less_HighSchool', 'Percent_HighSchool', 
                               'Percent_Less_College', 'Percent_Higher_College', 'FIPS')

Education_level$Percent_Less_HighSchool <- Education_level$Percent_Less_HighSchool/100
Education_level$Percent_HighSchool <- Education_level$Percent_HighSchool/100
Education_level$Percent_Less_College <- Education_level$Percent_Less_College/100
Education_level$Percent_Higher_College <- Education_level$Percent_Higher_College/100




## Federal Education Investment ----
Fed_Investment <- Fed_Investment[, -c(1:3, 5:6)]
Fed_Investment$FIPS <- as.double(Fed_Investment$FIPS)
Fed_Investment <- Fed_Investment %>% 
  group_by(FIPS) %>%
  summarize(Total_Federal_Investment = sum(Total_Federal_Investment))




# COVID Closures
# colnames(COVID_closures) <- paste("COVID_Closures", colnames(COVID_closures), sep = "_")
# COVID_closures$FIPS <- as.double(COVID_closures$COVID_Closures_FIPS)
# COVID_closures$COVID_Closures_FIPS <- NULL



##  Election Decisions  ----
#str(Election_decisions)
#View(Election_decisions)
Election_decisions <- Election_decisions %>%
                        filter(race == "President") %>%
                          filter(lname == "Trump" | lname == "Biden")
colnames(Election_decisions) <- paste("Election", colnames(Election_decisions), sep = "_")
Election_decisions$FIPS <- as.double(Election_decisions$Election_fips5)
Election_decisions <- Election_decisions[, -c(2, 3, 4, 5, 11, 12 )]
Election_decisions$Election_Trump <- ifelse(Election_decisions$Election_lname == "Trump", 1, 0)
Election_decisions$Election_Biden <- ifelse(Election_decisions$Election_lname == "Biden", 1, 0)
Election_decisions$Trump_Votes <- Election_decisions$Election_votes * Election_decisions$Election_Trump
Election_decisions$Biden_Votes <- Election_decisions$Election_votes * Election_decisions$Election_Biden

Election_decisions_1 <- Election_decisions %>% 
  group_by(FIPS) %>%
  summarize(Winner_Votes = max(Election_votes), 
            Trump_Votes = max(Trump_Votes), 
            Biden_Votes = max(Biden_Votes), 
            Vote_Diff = Biden_Votes - Trump_Votes)

#View(Election_decisions_1)

Election_decisions_1$Trump_Win <- ifelse(Election_decisions_1$Trump_Votes > Election_decisions_1$Biden_Votes, 
                                         1, 0)
Election_decisions_1$Biden_Win <- ifelse(Election_decisions_1$Trump_Votes < Election_decisions_1$Biden_Votes, 
                                         1, 0)
#Election_decisions_1 <- Election_decisions_1[-c(2:4)]
Election_decisions <- Election_decisions_1





##  FIPS_to_longlat ----
FIPS_to_longlat$FIPS <- FIPS_to_longlat$GEOID
FIPS_to_longlat$GEOID <- NULL
FIPS_to_longlat$LAT <- FIPS_to_longlat$INTPTLAT
FIPS_to_longlat$LON <- FIPS_to_longlat$INTPTLONG
FIPS_to_longlat <- FIPS_to_longlat[, -c(0:9)]


##  Population Density -----
colnames(Population_density) <- paste("Pop_Density", colnames(Population_density), sep = "_")
Population_density$FIPS <- as.double(Population_density$Pop_Density_GEOID)
Population_density$Population_Density <- as.double(Population_density$Pop_Density_B01001_calc_PopDensity)
Population_density <- Population_density[, -c(1:23)]




##  Rural Population  -----
colnames(Rural_percentage) <- paste("Rural_Percent", colnames(Rural_percentage), sep = "_")
Rural_percentage$FIPS <- Rural_percentage[, c(1)]
Rural_percentage <- Rural_percentage[, -c(1:4)]
Rural_percentage_1 <- Rural_percentage
colnames(Rural_percentage_1) <- c('Total_Population', 'Urban_Population', 'Rural_Population', 'Percent_Rural', 'FIPS')
Rural_percentage_1$Percent_Rural <- Rural_percentage_1$Percent_Rural / 100
Rural_percentage <- Rural_percentage_1
Rural_percentage$Urban_Population <- NULL
Rural_percentage$Rural_Population <- NULL
Rural_percentage$Total_Population <-  as.numeric(str_replace(Rural_percentage$Total_Population, ",", ""))







##   Unemployment + Income -----

colnames(Unemployment_Income) <- Unemployment_Income[4, ]
Unemployment_Income <- Unemployment_Income[-c(1:4), ]
Unemployment_Income <- Unemployment_Income[, c("FIPS_Code", "Median_Household_Income_2019", "Unemployed_2019")]
names(Unemployment_Income)[1] <- "FIPS" 
Unemployment_Income$FIPS <- as.double(Unemployment_Income$FIPS)
Unemployment_Income$Median_Household_Income_2019 <- as.numeric(Unemployment_Income$Median_Household_Income_2019)
Unemployment_Income$Unemployed_2019 <- as.numeric(Unemployment_Income$Unemployed_2019)



##   Poverty Estimate -----
colnames(Poverty_estimate) <- paste("Poverty_Estimates", colnames(Poverty_estimate), sep = "_")
Poverty_estimate$FIPS <- as.double(Poverty_estimate$Poverty_Estimates_FIPStxt)
keep <- c('POVALL_2019')
Poverty_estimate <- subset(Poverty_estimate, Poverty_Estimates_Attribute %in% keep)
Poverty_estimate <- Poverty_estimate[, -c(1:4)]


##  Movement Range ------
movement_range$date <- movement_range$ds
movement_range$ds <- NULL
movement_range$country_code <- movement_range$country
movement_range$country <- NULL
movement_range$polygon_source <- NULL
movement_range$polygon_name <- NULL
movement_range$change_in_movement <- movement_range$all_day_bing_tiles_visited_relative_change
movement_range$all_day_bing_tiles_visited_relative_change <- NULL
movement_range$proportion_intouch_users <- movement_range$all_day_ratio_single_tile_users
movement_range$all_day_ratio_single_tile_users <- NULL
movement_range$baseline_calc_time <- movement_range$baseline_name
movement_range$baseline_name <- NULL
movement_range$baseline_calc_how <- movement_range$baseline_type
movement_range$baseline_type <- NULL
movement_range = filter(movement_range, movement_range$country_code == "USA")
movement_range$date <- as.Date(movement_range$date, format = "%Y-%m-%d")
movement_range = subset(movement_range, date > "2020-07-1" & date < "2020-07-15")
colnames(movement_range) <- paste("Movement_Range", colnames(movement_range), sep = "_")
movement_range$FIPS <- as.double(movement_range$Movement_Range_polygon_id)
movement_range$Movement_Range_polygon_id <- NULL
movement_range <- movement_range[, -c(2, 5, 6)]

movement_range <- movement_range %>% 
  group_by(FIPS) %>%
  summarize(Change_in_Movement = mean(Movement_Range_change_in_movement),
            Intouch_users_Proportion = mean(Movement_Range_proportion_intouch_users))





# SCI
# colnames(SCI) <- paste("SCI", colnames(SCI), sep = "_")
# SCI$FIPS <- as.double(SCI$SCI_user_loc)
# SCI$SCI_user_loc <- NULL
# SCI$second_FIPS <- as.double(SCI$SCI_fr_loc)
# SCI$SCI_fr_loc <- NULL
# SCI$Multiply <- SCI$FIPS * SCI$second_FIPS * SCI$SCI_scaled_sci
# SCI_Unique <- distinct(SCI, Multiply, .keep_all = TRUE)
# SCI <- SCI_Unique
# SCI$Multiply <- NULL











## COVID Data -----


### Vaccination -----

vaccine_extractor <- function(date, vaccine_master){
  
                Vaccine <- Vaccine_master[, c("FIPS", "Date", "Series_Complete_Pop_Pct" ,
                                "Series_Complete_5Plus" , "Series_Complete_5PlusPop_Pct" , "Series_Complete_12Plus"   ,
                                "Series_Complete_12PlusPop_Pct" , "Series_Complete_18Plus" ,"Series_Complete_18PlusPop_Pct" ,
                                "Series_Complete_65Plus" , "Series_Complete_65PlusPop_Pct",
                                "Census2019"  , "Census2019_5PlusPop"    , "Census2019_12PlusPop",
                                "Census2019_18PlusPop"  , "Census2019_65PlusPop")]
  
                
                Vaccine$Calculated_Completed <-  rowMeans(Vaccine[, c(5, 7, 9, 11)], na.rm = TRUE)
                
                Vaccine <- Vaccine %>%
                        filter(Date == date) 
                
                Vaccine$FIPS <- as.double(Vaccine$FIPS)
                Vaccine$Calculated_Completed  <- Vaccine$Calculated_Completed  / 100
                Vaccine <- Vaccine[, c("FIPS", "Calculated_Completed")]
                
                return(Vaccine)
}


vaccine_march_2022 <- vaccine_extractor("03/01/2022", vaccine_master = Vaccine_master)
vaccine_march_2021 <- vaccine_extractor("03/01/2021", vaccine_master = Vaccine_master)
vaccine_sep_2021 <- vaccine_extractor("09/01/2021", vaccine_master = Vaccine_master)


vaccine_merged <- left_join(vaccine_march_2021, vaccine_sep_2021, by = "FIPS")
vaccine_merged <- left_join(vaccine_merged, vaccine_march_2022, by = "FIPS")

colnames(vaccine_merged) <- c("FIPS", "March 2021", "Sep 2021", "March 2022")
vaccine_merged <- filter(vaccine_merged,  !is.na(FIPS))

#View(vaccine_merged)



### percent of essential workers -----
colnames(COVID_essential_workers) <- paste("COVID_Essential_Workers", colnames(COVID_essential_workers), sep = "_")
COVID_essential_workers$FIPS <- COVID_essential_workers$COVID_Essential_Workers_fips
COVID_essential_workers$COVID_Essential_Workers_fips <- NULL

### COVID Testing Clinics ----
colnames(COVID_testing_clinics) <- paste("COVID_Clinics", colnames(COVID_testing_clinics), sep = "_")
COVID_testing_clinics$COVID_Clinic_Testing <- ifelse(COVID_testing_clinics$'COVID_Clinics_testing_status' == "Yes", 1, 0)
COVID_testing_clinics <- COVID_testing_clinics[, -c(1:7, 9:10)]
COVID_testing_clinics$FIPS <- COVID_testing_clinics$COVID_Clinics_FIPS
COVID_testing_clinics <- COVID_testing_clinics[, -c(1:2)]



### COVID Hospital Location -------
colnames(COVID_hospital_locations) <- paste("COVID_Hospital", colnames(COVID_hospital_locations), sep = "_")
COVID_hospital_locations <- COVID_hospital_locations[, -c(1:11 ,17:25)]
COVID_hospital_locations[is.na(COVID_hospital_locations)] <- 0
COVID_hospital_locations$COVID_Hospital_All_Beds <- rowSums(COVID_hospital_locations[, c(1,  3)])
COVID_hospital_locations$COVID_Hospital_Occupancy_Rate <- rowMeans(COVID_hospital_locations[, c(4, 5)])
COVID_hospital_locations$FIPS <- COVID_hospital_locations$COVID_Hospital_FIPS
COVID_hospital_locations <- COVID_hospital_locations[, -c(1:6)]
COVID_hospital_locations <- COVID_hospital_locations %>% 
  group_by(FIPS) %>%
  summarize(COVID_Hospital_Occupancy_Rate = mean(COVID_Hospital_Occupancy_Rate), 
            COVID_Hospital_All_Beds = sum(COVID_Hospital_All_Beds))




### COVID Confirmed Cases ------
colnames(COVID_confirmed_cases) <- paste("COVID_Confirmed", colnames(COVID_confirmed_cases), sep = "_")
COVID_confirmed_cases$FIPS <- as.double(COVID_confirmed_cases$COVID_Confirmed_fips_code)
COVID_confirmed_cases <- COVID_confirmed_cases[, -c(2:163, 177:493)]
COVID_confirmed_cases$COVID_Average_Confirmed <- rowMeans(COVID_confirmed_cases[, c(2:14)])
COVID_confirmed_cases$FIPS <- COVID_confirmed_cases$COVID_Confirmed_fips_code
COVID_confirmed_cases <- COVID_confirmed_cases[, -c(1:14)]

###  COVID Death Cases -----
colnames(COVID_death_cases) <- paste("COVID_Death", colnames(COVID_death_cases), sep = "_")
COVID_death_cases<- COVID_death_cases[, -c(2:163, 177:492)]
COVID_death_cases$COVID_Average_Death <- rowMeans(COVID_death_cases[, c(2:14)])
COVID_death_cases$FIPS <- COVID_death_cases$COVID_Death_fips_code
COVID_death_cases <- COVID_death_cases[, -c(1:14)]

### COVID Testing Counts ----- 
colnames(COVID_testing_counts) <- paste("COVID_TestCounts", colnames(COVID_testing_counts), sep = "_")
COVID_testing_counts <- COVID_testing_counts[, -c(2:163, 177:492)]
COVID_testing_counts$COVID_Average_Testing <- rowMeans(COVID_testing_counts[, c(2:14)])
COVID_testing_counts$FIPS <- COVID_testing_counts$COVID_TestCounts_fips_code
COVID_testing_counts <- COVID_testing_counts[, -c(1:14)]

###  COVID Testing Capacity -----
colnames(COVID_testing_capacity) <- paste("COVID_TestCap", colnames(COVID_testing_capacity), sep = "_")
COVID_testing_capacity <- COVID_testing_capacity[, -c(2:163, 177:492)]
COVID_testing_capacity$COVID_Average_TestCapacity <- rowMeans(COVID_testing_capacity[, c(2:14)])
COVID_testing_capacity$FIPS <- COVID_testing_capacity$COVID_TestCap_fips_code
COVID_testing_capacity <- COVID_testing_capacity[, -c(1:14)]





#*******************************************************************************

# Data Exploration    ==========================================================

#*******************************************************************************


head(mask_use)

head(FIPS_master[FIPS_master$state_name == "Delaware"])

head(Education_level)

head(Election_decisions, 10)

head(FIPS_to_longlat)

head(Unemployment_Income)

head(movement_range, 5)

#head(SCI)

head(Population_density)

head(Rural_percentage)

head(COVID_closures)

head(COVID_confirmed_cases)  # 7-day rolling average of new confirmed cases of Covid-19

head(Poverty_estimate)

head(COVID_death_cases) # 7-day rolling average of new deaths attributed to Covid-19.

head(COVID_essential_workers)

head(COVID_hospital_locations)

head(COVID_testing_capacity) # 7-day rolling average of tests completed per 100k population in the county

head(COVID_testing_clinics)

head(COVID_testing_counts) # 7-day rolling average of total tests completed

# head(Vaccine)
# 
# head(Vaccine_test)
# 
# head(mixed_movement_data)

head(vaccine_merged)


#*******************************************************************************
# Data Integration     =========================================================
#*******************************************************************************


df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(mask_use, Education_level, Election_decisions, Unemployment_Income, 
                                                        Poverty_estimate, Population_density, COVID_confirmed_cases, 
                                                        COVID_death_cases, vaccine_merged, movement_range, 
                                                        COVID_essential_workers, Fed_Investment, 
                                                        COVID_hospital_locations, 
                                                        COVID_testing_counts, Rural_percentage))   






str(df)


df <- df[, -c("Percent_Less_HighSchool","Percent_HighSchool", 
              "Winner_Votes", "Trump_Win","Total_Population",
              "March 2021", "March 2022")] 

colnames(df) <- c('FIPS', 'C19-MN', 'C19-MR', 'C19-MS', 'C19-MF', 'C19-MA', 
                  'C19-MC',
                  'EL-LC', 'EL-MC', 
                  'PP-RE', 'PP-DE', 'PP-DL', "PP-DW",
                  'EC-IN', 'EC-UN', 'EC-PO', 
                  'PS-PD', 
                  'C19-CC', 'C19-DD', 'C19-VC' , 'C19-MO', 'C19-PT', 
                  'HC-PW', 'EL-IN', 
                  'HC-HO', 'HC-HB', 'C19-TT', 
                  #"Total_Population",
                  'PS-RL')



write_csv(df, "Data/ToMerge/df_unscaled.csv")

#*******************************************************************************
# Data Scaling         =========================================================
#*******************************************************************************

# Columns to rescale: Unemployement, Poverty, Income, Federal Investment, Hospital All beds, COVID_Average Testing

# Unemployment
df$Unemployed_2019 <- df$Unemployed_2019 / df$Total_Population

# Poverty
df$Poverty_Estimates_Value <- df$Poverty_Estimates_Value / df$Total_Population

# Income, Federal Investment, Hospital All Beds, Population Density, COVID Death, confirmed, average testing

myscaler <- function(dataset){
  df <- dataset
  for (i in 1:ncol(df)){
    if (colnames(df)[i] == "Total_Federal_Investment"){
      
    #  print(df$Total_Federal_Investment)
      df$"Total_Federal_Investment" <- (df$"Total_Federal_Investment" - min(df$"Total_Federal_Investment", na.rm = T)) /
                                   (max(df$"Total_Federal_Investment", na.rm = T) - min(df$"Total_Federal_Investment", na.rm = T))
    }
    else if (colnames(df)[i] == "Median_Household_Income_2019"){
      
      print(df$"Median_Household_Income_2019")
      df$"Median_Household_Income_2019" <- (df$"Median_Household_Income_2019" - min(df$"Median_Household_Income_2019", na.rm = T)) /
        (max(df$"Median_Household_Income_2019", na.rm = T) - min(df$"Median_Household_Income_2019", na.rm = T))
    }
    else if (colnames(df)[i] == "COVID_Hospital_All_Beds"){
      
     # print(df$Total_Federal_Investment)
      df$"COVID_Hospital_All_Beds" <- (df$"COVID_Hospital_All_Beds" - min(df$"COVID_Hospital_All_Beds", na.rm = T)) /
        (max(df$"COVID_Hospital_All_Beds", na.rm = T) - min(df$"COVID_Hospital_All_Beds", na.rm = T))
    }
    else if (colnames(df)[i] == "Mask_Usage_Combined"){
      
      # print(df$Total_Federal_Investment)
      df$"Mask_Usage_Combined" <- (df$"Mask_Usage_Combined" - min(df$"Mask_Usage_Combined", na.rm = T)) /
        (max(df$"Mask_Usage_Combined", na.rm = T) - min(df$"Mask_Usage_Combined", na.rm = T))
    }
    else if (colnames(df)[i] == "COVID_Average_Confirmed"){
      
      # print(df$Total_Federal_Investment)
      df$"COVID_Average_Confirmed" <- (df$"COVID_Average_Confirmed" - min(df$"COVID_Average_Confirmed", na.rm = T)) /
        (max(df$"COVID_Average_Confirmed", na.rm = T) - min(df$"COVID_Average_Confirmed", na.rm = T))
    }
    else if (colnames(df)[i] == "COVID_Average_Death"){
      
      # print(df$Total_Federal_Investment)
      df$"COVID_Average_Death" <- (df$"COVID_Average_Death" - min(df$"COVID_Average_Death", na.rm = T)) /
        (max(df$"COVID_Average_Death", na.rm = T) - min(df$"COVID_Average_Death", na.rm = T))
    }
    else if (colnames(df)[i] == "COVID_Average_Testing"){
      
      # print(df$Total_Federal_Investment)
      df$"COVID_Average_Testing" <- (df$"COVID_Average_Testing" - min(df$"COVID_Average_Testing", na.rm = T)) /
        (max(df$"COVID_Average_Testing", na.rm = T) - min(df$"COVID_Average_Testing", na.rm = T))
    }
    else if (colnames(df)[i] == "Population_Density"){
      
      # print(df$Total_Federal_Investment)
      df$"Population_Density" <- (df$"Population_Density" - min(df$"Population_Density", na.rm = T)) /
        (max(df$"Population_Density", na.rm = T) - min(df$"Population_Density", na.rm = T))
    }
  }
  
  print(df$Median_Household_Income_2019)
  return(df)
}

# , "Median_Household_Income_2019", 
# "COVID_Hospital_All_Beds"

df <- myscaler(df)







# FIPS to Character
df <- filter(df, !is.na(df$FIPS))
df$FIPS <- as.character(df$FIPS)

df <- filter(df, nchar(df$FIPS) == 4 | nchar(df$FIPS) == 5)
df$FIPS <- ifelse(nchar(df$FIPS) == 4, paste("0" , as.character(df$FIPS), sep = "") , df$FIPS)


df <- filter(df, !is.na(Mask_Usage_Combined))


## Getting the min and max of each column

minMax <- function(dataset){
  for (i in colnames(dataset)){
    
    print(paste0(i, ", Minimum is: ", min(dataset[[c(i)]], na.rm = T), 
                    ", Maximum is: ", max(dataset[[c(i)]], na.rm = T)))
    
  }
}

minMax(df)



# Data Finalization ============================================================

df <- df[, -c("Percent_Less_HighSchool","Percent_HighSchool", 
              "Winner_Votes", "Trump_Win","Total_Population",
              "March 2021", "March 2022")] 
         
colnames(df) <- c('FIPS', 'C19-MN', 'C19-MR', 'C19-MS', 'C19-MF', 'C19-MA', 
                  'C19-MC',
                  'EL-LC', 'EL-MC', 
                  'PP-RE', 'PP-DE', 'PP-DL', "PP-DW",
                  'EC-IN', 'EC-UN', 'EC-PO', 
                  'PS-PD', 
                  'C19-CC', 'C19-DD', 'C19-VC' , 'C19-MO', 'C19-PT', 
                  'HC-PW', 'EL-IN', 
                  'HC-HO', 'HC-HB', 'C19-TT', 
                  #"Total_Population",
                  'PS-RL')

#*******************************************************************************
# Data Exportation     =========================================================
#*******************************************************************************

write_csv(df, "Data/ToMerge/df.csv")

















