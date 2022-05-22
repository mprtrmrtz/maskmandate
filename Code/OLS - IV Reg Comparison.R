
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

setwd("G:/My Drive/Research/Project/")
getwd()










#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Data Importing and Manipulation  =============================================

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


df <- fread("Data/ToMerge/df.csv")

head(df)
colnames(df)


df <- df[, -c("FIPS","Percent_Less_HighSchool","Percent_HighSchool", 
              "Winner_Votes", "Trump_Win","Total_Population",
              "March 2021", "March 2022"
) ]
head(df)
colnames(df)


colnames(df) <- c('C19-MN', 'C19-MR', 'C19-MS', 'C19-MF', 'C19-MA', 
                  'C19-MC',
                  'EL-LC', 'EL-MC', 
                  'PP-RE', 'PP-DE', 'PP-DL', "PP-DW",
                  'EC-IN', 'EC-UN', 'EC-PO', 
                  'PS-PD', 
                  'C19-CC', 'C19-DD', 'C19-VC' , 'C19-MO', 'C19-PT', 
                  'HC-PW', 'EL-IN', 
                  'HC-HO', 'HC-HB', 'C19-TT', 
                  'PS-RL'
)



head(df)







#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# IV Regressions        ========================================================
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


attach(df)

### Mask Usage =================================================================


Mask_1 <- lm(formula = `C19-MC`  ~   `PS-PD`  + 
                      `EL-MC`  + 
                      `PP-DW`  + 
                      `C19-CC`  + 
                      `HC-PW`  + 
                      `EC-PO`  + 
                      `HC-HO`  + 
                      `EC-UN`
                    , data = df)

summary(Mask_1)

Mask_2 <- ivreg(data = df, 
                         `C19-MC` ~   
                          `PS-PD`  + 
                          `EL-MC`  + 
                          `PP-DW`  + 
                          `C19-CC`  + 
                          `HC-PW`  + 
                          `EC-PO`  + 
                          `HC-HO`  + 
                          `EC-UN`       | `EL-IN` + 
                  
                          `PS-PD`  + 
                          `EL-MC`  + 
                          `PP-DW`  + 
                          `C19-CC` + 
                          `HC-PW`  + 
                          `EC-PO`  + 
                          `HC-HO`  + 
                          `EC-UN`  ) 

summary(Mask_2)


### Movement Change =================================================================

Move_1 <- lm(formula =  `C19-MO`   ~    `PS-PD`  + 
                                         `EL-MC`  + 
                                         `PP-DW`  + 
                                         `C19-CC`  + 
                                         `HC-PW`  + 
                                         `EC-PO`  + 
                                         `HC-HO`  + 
                                         `EC-UN`
                     
                     , data = df)



summary(Move_1)



Move_2 <- ivreg(data = df, 
                       `C19-MO`  ~   
                                  `PS-PD`  + 
                                  `EL-MC`  + 
                                  `PP-DW`  + 
                                  `C19-CC`  + 
                                  `HC-PW`  + 
                                  `EC-PO`  + 
                                  `HC-HO`  + 
                                  `EC-UN`       | `EL-IN` + 
                                  
                                  `PS-PD`  + 
                                  `EL-MC`  + 
                                  `PP-DW`  + 
                                  `C19-CC` + 
                                  `HC-PW`  + 
                                  `EC-PO`  + 
                                  `HC-HO`  + 
                                  `EC-UN`  ) 

summary(Move_2)




# Vaccine Usage ================================================================

Vacc_1 <- lm(formula =  `C19-VC`  ~    `PS-PD`  + 
                                       `EL-MC`  + 
                                       `PP-DW`  + 
                                       `C19-CC`  + 
                                       `HC-PW`  + 
                                       `EC-PO`  + 
                                       `HC-HO`  + 
                                       `EC-UN` 
                    , data = df) 

 
summary(Vacc_1)

Vacc_2 <- ivreg(data = df, 
                       `C19-VC` ~   
                              `PS-PD`  + 
                              `EL-MC`  + 
                              `PP-DW`  + 
                              `C19-CC`  + 
                              `HC-PW`  + 
                              `EC-PO`  + 
                              `HC-HO`  + 
                              `EC-UN`       | `EL-IN` + 
                              
                              `PS-PD`  + 
                              `EL-MC`  + 
                              `PP-DW`  + 
                              `C19-CC` + 
                              `HC-PW`  + 
                              `EC-PO`  + 
                              `HC-HO`  + 
                              `EC-UN`  ) 

 
summary(Vacc_2)


















## Results =============
# 
# iv.fit <- mget(paste0("regression_V", 1:3))
# 
# 
# stargazer(regression_V1, regression_V2,  regression_V3, 
#           dep.var.labels = c("Vaccination Participation"), align = TRUE, no.space = TRUE,
#           row.sep.width = "15pt", 
#           add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
#                                                    summary, diagnostics=TRUE), row=1:2)
# )

stargazer( Mask_1, Mask_2,
          dep.var.labels = c( "Mask Mandate"), align = TRUE, no.space = F,
          row.sep.width = "30pt",
          column.sep.width = "10pt",
          type = "latex", omit.stat = c("adj.rsq", "rsq"), 
          covariate.labels = c("PS-PD", "EL-MC", "PP-DW", "C19-CC", "HC-PW", 
                               "EC-PO", "HC-HO", "EC-UN")
          )




























