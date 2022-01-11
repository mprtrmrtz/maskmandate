


rm(list = ls())


library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
library(ggmap)
library(sf)
library(data.table)
library(bit64)



setwd("C:/Users/momaleki/Desktop/Project")
getwd()

print("Finished Operation!")





#*******************************************************************************

# Movement    Data =============================================================

#*******************************************************************************

old_movement_range <- fread("Data/movement-range-2021-05-23.txt")

movement_range <- old_movement_range

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
movement_range = subset(movement_range, date > "2020-07-01" & date < "2020-07-15")
colnames(movement_range) <- paste("Movement_Range", colnames(movement_range), sep = "_")
movement_range$FIPS <- as.double(movement_range$Movement_Range_polygon_id)
movement_range$Movement_Range_polygon_id <- NULL
movement_range <- movement_range[, -c(2, 5, 6)]

movement_range <- movement_range %>% 
  group_by(FIPS) %>%
  summarize(Change_in_Movement = mean(Movement_Range_change_in_movement),
            Intouch_users_Proportion = mean(Movement_Range_proportion_intouch_users))

old_movement_range <- movement_range

head(old_movement_range)


new_movement_range <- fread("Feedback from Monica Implemented/Data/new_movement_range.txt")
movement_range <- new_movement_range

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
movement_range = subset(movement_range, date > "2021-07-01" & date < "2021-07-15")
colnames(movement_range) <- paste("Movement_Range", colnames(movement_range), sep = "_")
movement_range$FIPS <- as.double(movement_range$Movement_Range_polygon_id)
movement_range$Movement_Range_polygon_id <- NULL
movement_range <- movement_range[, -c(2, 5, 6)]

movement_range <- movement_range %>% 
  group_by(FIPS) %>%
  summarize(Change_in_Movement = mean(Movement_Range_change_in_movement),
            Intouch_users_Proportion = mean(Movement_Range_proportion_intouch_users))

new_movement_range <- movement_range

head(new_movement_range)


mixed_movement_data <- NULL

mixed_movement_data <- merge(old_movement_range, new_movement_range, 
                             by = "FIPS", 
                             all = TRUE)


head(mixed_movement_data, 20)
nrow(mixed_movement_data)



mixed_movement_data <- mixed_movement_data %>% 
  mutate(Change_in_Movement = Change_in_Movement.x - Change_in_Movement.y)

mixed_movement_data <- mixed_movement_data[, -c(2:5)]
mixed_movement_data$Updated_Change_in_Movement <- mixed_movement_data$Change_in_Movement
mixed_movement_data$Change_in_Movement <- NULL

head(mixed_movement_data)
nrow(mixed_movement_data)


write_csv(mixed_movement_data, "Data/mixed_movement_data.csv")



#*******************************************************************************

# Vaccination Data =============================================================

#*******************************************************************************


rm(list = ls())


library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
library(ggmap)
library(sf)
library(data.table)
library(bit64)



setwd("C:/Users/momaleki/Desktop/Project")
getwd()

print("Finished Operation!")


mask_use <- fread("Data/mask-use-by-county.csv")
COVID_closures <- fread("Data/US County Level Summaries.csv")
FIPS_master <- fread("Data/county_fips_master.csv")
Education_level <- fread("Data/Education level by Country 1970 - 2019.csv")
Election_decisions <- fread("Data/election decisions by fips country.csv")
Pop_estimates <- fread("Data/PopulationEstimates per country 2019.csv")
Poverty_estimate <- fread("Data/PovertyEstimates per country 2019.csv")
Presidential_elections <- fread("Data/presidential election results per fips county.csv")
Unemployment <- fread("Data/Unemployment per country 2000 -2019.csv")
FIPS_to_longlat <- fread("Data/2020_Gaz_counties_national.txt")
SCI <- fread(file = "Data/county_county_aug2020.tsv", sep = '\t', header = TRUE)
movement_range <- fread("Data/movement-range-2021-05-23.txt")
Population_density <- fread("Data/Population_density.csv")
Rural_percentage <- fread("Data/Rureal_Percentages_csv.csv") 
crime <- fread("Data/crime_data_w_population_and_crime_rate.csv")
Fed_Investment <- fread("Data/IV Regression/merged_FIPS.csv")
mixed_movement_data <- fread("Data/mixed_movement_data.csv")

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

print("Finished Operation!")



### Time series analysis with the vaccination data =============================





Vaccine <- Vaccine_master %>%
  group_by(FIPS) %>%
  summarize(Vaccine_completeness_pct = mean(Completeness_pct))

Vaccine$FIPS <- as.double(Vaccine$FIPS)
Vaccine$Vaccine_completeness_pct <- Vaccine$Vaccine_completeness_pct / 100

Vaccine_test <- Vaccine_master %>% filter(Vaccine_master$Date == Vaccine_master$Date[1])

Vaccine_test




Vaccine <- Vaccine_master %>%
  group_by(FIPS) %>%
  summarize(Vaccine_completeness_pct = mean(Completeness_pct))

Vaccine$FIPS <- as.double(Vaccine$FIPS)
Vaccine$Vaccine_completeness_pct <- Vaccine$Vaccine_completeness_pct / 100

head(Vaccine)




### Using the most up to date vaccination data =================================















### Compare beginning and present of vaccination ===============================





















