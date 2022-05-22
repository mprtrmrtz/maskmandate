

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


setwd("G:/My Drive/Research/Project/")
getwd()

#*******************************************************************************
# Data Importing  ==============================================================
#*******************************************************************************


df <- fread("Data/ToMerge/df.csv")


#*******************************************************************************
# Correlation Analysis  ========================================================
#*******************************************************************************


df_cor <- df
df_cor <- df_cor[complete.cases(df_cor), ]
head(df_cor)
dim(df_cor)


M <- cor(df_cor)
par(ask = TRUE)
corrplot(M, method = "square", order = "hclust", addrect = 3, tl.col = "black", 
         rect.lwd = 3, tl.cex = 1, insig = c("label_sig"), cl.offset = 0.5,
         cl.pos = "r", cl.ratio = 0.1, type = "lower" )



write_csv(df, "Data/ToMerge/df_updated.csv")


#*******************************************************************************
# Regression Analysis ==========================================================
#*******************************************************************************

attach(df)

### Mask Mandate =================================================================



regression_C1 <- lm(formula = `C19-MC`  ~ `PS-PD` + 
                       `PP-DW` + 
                       `EC-PO`
                    , data = df)

summary(regression_C1)


regression_C2 <- lm(formula = `C19-MC`  ~  `PS-PD` + 
                      `EL-MC` + 
                       `PP-DW` + 
                      `EC-PO`
                    , data = df)

summary(regression_C2)



regression_C3 <- lm(formula = `C19-MC`  ~   `PS-PD`  + 
                       `EL-MC`  + 
                        `PP-DW`  + 
                       `C19-CC`  + 
                       `HC-PW`  + 
                       `EC-PO`  + 
                       `HC-HO`  + 
                       `EC-UN`
                    , data = df)

summary(regression_C3)



print("True1")
stargazer(regression_C1, regression_C2, regression_C3,  
          dep.var.labels = c("Mask Adoption Score"), 
         align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
         # single.row = T, 
          type = "latex", 
        # se = list(NULL, robust.se), 
         covariate.labels = c("EL-MC", "PP-DW", "PS-PD", 
                              "C19-CC", "HC-PW", "EC-PO", 
                              "HC-HO", "EC-UN"))




### Movement Change ============================================================



regression_UM1 <- lm(formula =  `C19-MO`   ~   `PS-PD`  + 
                         `PP-DW`  + 
                        `EC-PO` 
                     , data = df)

summary(regression_UM1)



regression_UM2 <- lm(formula =  `C19-MO`   ~   `PS-PD`  + 
                        `EL-MC`  + 
                         `PP-DW`  + 
                        `EC-PO` 
                     , data = df)

summary(regression_UM2)



regression_UM3 <- lm(formula =  `C19-MO`   ~    `PS-PD`  + 
                       `EL-MC`  + 
                       `PP-DW`  + 
                       `C19-CC`  + 
                       `HC-PW`  + 
                       `EC-PO`  + 
                       `HC-HO`  + 
                       `EC-UN`
                     
                     , data = df)



summary(regression_UM3)


print("True2")
stargazer(regression_UM1, regression_UM2, regression_UM3,  
          dep.var.labels = c("Change in Movement"), # align = TRUE,
          align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "text", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("PS-PD","EL-MC", "PP-DW",  
                               "C19-CC", "HC-PW", "EC-PO", 
                               "HC-HO", "EC-UN"))




s### Vaccine Participation ======================================================


regression_V1 <- lm(formula =  `C19-VC`   ~   `PS-PD`  + 
                      `PP-DW`  + 
                      `EC-PO` 
                    , data = df)

summary(regression_V1)



regression_V2 <- lm(formula =  `C19-VC`  ~   `PS-PD`  + 
                      `EL-MC`  + 
                      `PP-DW`  + 
                      `EC-PO`
                    , data = df)

summary(regression_V2)





# Remove Unemployment and add Poverty instead

regression_V3 <- lm(formula =  `C19-VC`  ~    `PS-PD`  + 
                      `EL-MC`  + 
                      `PP-DW`  + 
                      `C19-CC`  + 
                      `HC-PW`  + 
                      `EC-PO`  + 
                      `HC-HO`  + 
                      `EC-UN` 
                    , data = df) 

summary(regression_V3 )


# Scale the variables
# Use online tools
print("True3")
stargazer(regression_V1, regression_V2, regression_V3,
          dep.var.labels = c("Vaccine Participation"), # align = TRUE, 
          align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "latex", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("EL-MC", "PP-DW", "PS-PD", 
                               "C19-CC", "HC-PW", "EC-PO", 
                               "HC-HO", "EC-UN"))








































































































