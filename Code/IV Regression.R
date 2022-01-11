
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Housekeeping Items ===========================================================

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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
library(corrplot)
library(ivreg)
library(stargazer)

setwd("C:/Users/momaleki/Desktop/Project")
getwd()




gaze.lines.ivreg.diagn <- function(x, col="p-value", row=1:3, digits=2){
  stopifnot(is.list(x))
  out <- lapply(x, function(y){
    stopifnot(class(y)=="summary.ivreg")
    y$diagnostics[row, col, drop=FALSE]
  })
  out <- as.list(data.frame(t(as.data.frame(out)), check.names = FALSE))
  for(i in 1:length(out)){
    out[[i]] <- c(names(out)[i], round(out[[i]], digits=digits))
  }
  return(out)
}





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Data Importing and Manipulation  =============================================

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



df <- fread("Data/ToMerge/df.csv")

print("Finished Operation!")





df <- df[, -c(8:11, 32:34)]



colnames(df) <- c('FIPS', 'NEVER', 'RARELY', 'SOMETIMES', 'FREQUENTLY', 'ALWAYS', 
                  'Mask_Combined', 'Percent_Less_HS', 'Percent_HS', 'Percent_Less_College', 'Percent_Higher_College', 
                  'Trump_Win', 'Biden_Win', 'Unemployment', 'Poverty', 'Pop_density', 'COVID_Confirmed', 
                  'COVID_Death', 'Crime', 'Vaccine', 'Movement_Change', 'Percent_InTouch', 'Percent_Ess_Workers', 
                  'Federal_Investment', 'Hospital_Occ_rate', 'Hospital_AllBeds', 'COVID_Testing', 'Percent_Rural', 
                  "Updated_Movement_Change")

df$Education_combined <- df$Percent_Less_HS * 1 + 
  df$Percent_HS * 2 + 
  df$Percent_Less_College * 3 +
  df$Percent_Higher_College * 4
df <- df %>%
  group_by(FIPS) %>%
  summarise_each(list(mean))

head(df)



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# IV Regressions        ========================================================

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




### Mask Usage =================================================================

regression_C1 <- ivreg(data = df, 
                       df$Mask_Combined ~   
                         df$Percent_Higher_College | df$Federal_Investment)

summary(regression_C1)



regression_C2 <- ivreg(data = df, 
                       df$Mask_Combined ~   
                         df$Percent_Higher_College + df$Pop_density | df$Federal_Investment + df$Pop_density)

summary(regression_C2)




regression_C3 <- ivreg(data = df, 
                       df$Mask_Combined ~   
                         df$Percent_Higher_College + df$Pop_density + df$Poverty | 
                         df$Federal_Investment + df$Pop_density + df$Poverty)

summary(regression_C3)



regression_C4 <- ivreg(data = df, 
                       df$Mask_Combined ~   
                         df$Pop_density + df$Percent_Higher_College + df$Biden_Win 
                       #+# df$Unemployment + 
                       #  df$COVID_Confirmed + df$Percent_Ess_Workers #+ df$Hospital_Occ_rate 
                       
                       |
                         
                         df$Federal_Investment + df$Pop_density + df$Biden_Win
                       #  + #df$Unemployment + 
                       #   df$COVID_Confirmed + df$Percent_Ess_Workers
) #+ df$Hospital_Occ_rate)

summary(regression_C4)




iv.fit <- mget(paste0("regression_C", 1:4))


stargazer(regression_C1, regression_C2, regression_C3, regression_C4, 
          dep.var.labels = c("Mask Mandate Adoption"), align = TRUE, no.space = TRUE,
          row.sep.width = "5pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2)
)




### Movemet Change =================================================================



df_updated_move <- filter(df, is.na(df$Updated_Movement_Change) == FALSE)
head(df_updated_move)



regression_UM1 <- ivreg(data = df_updated_move, 
                        df_updated_move$Updated_Movement_Change ~   
                          df_updated_move$Percent_Higher_College  |  df_updated_move$Federal_Investment )

summary(regression_UM1)




regression_UM2 <- ivreg(data = df_updated_move, 
                        df_updated_move$Updated_Movement_Change ~   
                          df_updated_move$Percent_Higher_College + df_updated_move$Pop_density
                        |  df_updated_move$Federal_Investment + df_updated_move$Pop_density)



summary(regression_UM2)



regression_UM3 <- ivreg(data = df_updated_move, 
                        df_updated_move$Updated_Movement_Change ~   
                          df_updated_move$Percent_Higher_College + df_updated_move$Pop_density + 
                          df_updated_move$Poverty
                        |  df_updated_move$Federal_Investment + df_updated_move$Pop_density + 
                          df_updated_move$Poverty)

summary(regression_UM3)



regression_UM4 <- ivreg(data = df_updated_move, 
                        df_updated_move$Updated_Movement_Change ~   
                          df_updated_move$Percent_Higher_College + df_updated_move$Pop_density + 
                          #  df_updated_move$Poverty + df_updated_move$COVID_Confirmed + 
                          # df_updated_move$Percent_Ess_Workers 
                          + df_updated_move$Biden_Win
                        
                        |  
                          
                          df_updated_move$Federal_Investment + df_updated_move$Pop_density + 
                          #    df_updated_move$Poverty + df_updated_move$COVID_Confirmed + 
                          # df_updated_move$Percent_Ess_Workers 
                          + df_updated_move$Biden_Win)


summary(regression_UM4)





iv.fit <- mget(paste0("regression_UM", 1:4))


stargazer(regression_UM1, regression_UM2, regression_UM3, regression_UM4, 
          dep.var.labels = c("Updated Change in Movement IV Regression"), align = TRUE, no.space = TRUE,
          row.sep.width = "5pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2)
)




# Vaccine Usage ================================================================

regression_V1 <- ivreg(data = df, 
                       df$Vaccine ~   
                         df$Percent_Higher_College | df$Federal_Investment )

summary(regression_V1)





regression_V2 <- ivreg(data = df, 
                       df$Vaccine ~   
                         df$Percent_Higher_College + df$Pop_density  | df$Federal_Investment + df$Pop_density )

summary(regression_V2)



regression_V3 <- ivreg(data = df, 
                       df$Vaccine ~   
                         df$Percent_Higher_College + df$Pop_density + df$Poverty  | df$Federal_Investment 
                       + df$Pop_density + df$Poverty)

summary(regression_V3)




regression_V4 <- ivreg(data = df, 
                       df$Vaccine ~   
                         df$Pop_density + df$Percent_Higher_College + df$Biden_Win #+# df$Unemployment + 
                       # df$COVID_Confirmed + df$Percent_Ess_Workers #+ df$Hospital_Occ_rate 
                       
                       |
                         
                         df$Federal_Investment + df$Pop_density + df$Biden_Win# + #df$Unemployment + 
                       # df$COVID_Confirmed + df$Percent_Ess_Workers) #+ df$Hospital_Occ_rate
)

summary(regression_V4)



iv.fit <- mget(paste0("regression_V", 1:4))



stargazer(regression_V1, regression_V2, regression_V3, regression_V4, 
          dep.var.labels = c("Vaccination Completeness Percent IV Regression"), align = TRUE, no.space = TRUE,
          row.sep.width = "5pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2)
)





























