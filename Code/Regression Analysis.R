

#*******************************************************************************
# Housekeeping Items ===========================================================
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
library(corrplot)
library(stargazer)


setwd("G:/My Drive/Research/Mask Mandate/")
getwd()

#*******************************************************************************
# Data Importing  ==============================================================
#*******************************************************************************

df <- fread("Data/ToMerge/df.csv")

#*******************************************************************************
# reg Analysis ==========================================================
#*******************************************************************************

attach(df)

### Mask Mandate =================================================================

reg_C1 <- lm(formula = `C19-MC`  ~ `PS-PD` + 
                       `PP-DW` + 
                       `EC-PO`
                    , data = df)

reg_C2 <- lm(formula = `C19-MC`  ~  `PS-PD` + 
                      `EL-MC` + 
                       `PP-DW` + 
                      `EC-PO`
                    , data = df)

reg_C3 <- lm(formula = `C19-MC`  ~   `PS-PD`  + 
                       `EL-MC`  + 
                        `PP-DW`  + 
                       `C19-CC`  + 
                       `HC-PW`  + 
                       `EC-PO`  + 
                       `HC-HO`  + 
                       `EC-UN`
                    , data = df)

stargazer(reg_C1, reg_C2, reg_C3,  
          dep.var.labels = c("Mask Adoption"), 
          align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "latex", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("PS-PD","EL-MC", "PP-DW", 
                               "C19-CC", "HC-PW", "EC-PO",
                               "HC-HO", "EC-UN"))

### Movement Change ============================================================

reg_M1 <- lm(formula =  `C19-MO`   ~   `PS-PD`  + 
                         `PP-DW`  + 
                        `EC-PO` 
                     , data = df)

reg_M2 <- lm(formula =  `C19-MO`   ~   `PS-PD`  + 
                        `EL-MC`  + 
                         `PP-DW`  + 
                        `EC-PO` 
                     , data = df)

reg_M3 <- lm(formula =  `C19-MO`   ~    `PS-PD`  + 
                       `EL-MC`  + 
                       `PP-DW`  + 
                       `C19-CC`  + 
                       `HC-PW`  + 
                       `EC-PO`  + 
                       `HC-HO`  + 
                       `EC-UN`
                     , data = df)

stargazer(reg_M1, reg_M2, reg_M3,  
          dep.var.labels = c("Mobility Restrictions"), # align = TRUE,
          align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "latex", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("PS-PD","EL-MC", "PP-DW", 
                               "C19-CC", "HC-PW", "EC-PO",
                               "HC-HO", "EC-UN"))

### Vaccine Participation ======================================================

reg_V1 <- lm(formula =  `C19-VC`   ~   `PS-PD`  + 
                      `PP-DW`  + 
                      `EC-PO` 
                    , data = df)

reg_V2 <- lm(formula =  `C19-VC`  ~   `PS-PD`  + 
                      `EL-MC`  + 
                      `PP-DW`  + 
                      `EC-PO`
                    , data = df)

# Remove Unemployment and add Poverty instead

reg_V3 <- lm(formula =  `C19-VC`  ~    
                      `PS-PD`  + 
                      `EL-MC`  + 
                      `PP-DW`  + 
                      `C19-CC` + 
                      `HC-PW`  + 
                      `EC-PO`  + 
                      `HC-HO`  + 
                      `EC-UN` 
                    , data = df) 

stargazer(reg_V1, reg_V2, reg_V3,
          dep.var.labels = c("Vaccine Participation"), # align = TRUE, 
          align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "latex", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("PS-PD","EL-MC", "PP-DW", 
                               "C19-CC", "HC-PW", "EC-PO",
                               "HC-HO", "EC-UN"))








































































































