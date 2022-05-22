
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
library(ivreg)
library(stargazer)

setwd("G:/My Drive/Research/Project/")
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





#*******************************************************************************
# Data Importing and Manipulation  =============================================
#*******************************************************************************

df <- fread("Data/ToMerge/df.csv")


#*******************************************************************************
# IV Regressions        ========================================================
#*******************************************************************************

attach(df)

### Mask Usage -----

regression_C1 <- ivreg(data = df, 
                       `C19-MC` ~   
                         `EL-MC` | `EL-IN`)

summary(regression_C1)



regression_C2 <- ivreg(data = df, 
                       `C19-MC` ~   
                         `EL-MC` + `PP-DW` | `EL-IN` + `PP-DW`)

summary(regression_C2)



regression_C3 <- ivreg(data = df, 
                       `C19-MC` ~   
                         `EL-MC` +  `PP-DW`  +   `PS-PD`+
                         `C19-CC`  + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN` | `EL-IN` + `PP-DW`  +   `PS-PD`+
                         `C19-CC`  + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN` ) 

summary(regression_C3)




iv.fit <- mget(paste0("regression_C", 1:3))


stargazer(regression_C1, regression_C2,  regression_C3, 
          dep.var.labels = c("Mask Mandate Adoption"),  align = TRUE, no.space = F,
          row.sep.width = "5pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2), 
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "latex", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("EL-MC", "PP-DW", "PS-PD", 
                               "C19-CC", "HC-PW", "EC-PO", 
                               "HC-HO", "EC-UN"), 
          omit.stat = c("adj.rsq", "rsq"))




### Movement Change -----


regression_M1 <- ivreg(data = df, 
                       `C19-MO` ~   
                         `EL-MC` | `EL-IN`)

summary(regression_M1)



regression_M2 <- ivreg(data = df, 
                       `C19-MO` ~   
                         `EL-MC` + `PP-DW`| `EL-IN` + `PP-DW`)

summary(regression_M2)






regression_M3 <- ivreg(data = df, 
                       `C19-MO` ~   
                         `EL-MC` + `PP-DW`  + `PS-PD`+
                         `C19-CC`  + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN` | `EL-IN` + `PP-DW`  +  `PS-PD`+
                         `C19-CC`  + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN` ) 

summary(regression_M3)




iv.fit <- mget(paste0("regression_M", 1:3))


stargazer(regression_M1, regression_M2,  regression_M3, 
          dep.var.labels = c("Change in Movement"), # align = TRUE,#  no.space = TRUE,
          row.sep.width = "15pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2), 
          align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "latex", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("EL-MC", "PP-DW", "PS-PD", 
                               "C19-CC", "HC-PW", "EC-PO", 
                               "HC-HO", "EC-UN"), 
          omit.stat = c("adj.rsq", "rsq"))





## Vaccine Usage -----

regression_IV_V1 <- ivreg(data = df, 
                       `C19-VC` ~   
                         `EL-MC` | `EL-IN`)

summary(regression_IV_V1)



regression_IV_V2 <- ivreg(data = df, 
                       `C19-VC` ~   
                         `EL-MC` + `PP-DW`| `EL-IN` + `PP-DW`)

summary(regression_IV_V2)




regression_IV_V3 <- ivreg(data = df, 
                       `C19-VC` ~   
                         `EL-MC` +  `PP-DW`  +   `PS-PD`+
                         `C19-CC`  + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN`  | `EL-IN` +  `PP-DW`  +   `PS-PD`+
                         `C19-CC`  + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN` ) 

summary(regression_IV_V3)






iv.fit <- mget(paste0("regression_IV_V", 1:3))


stargazer(regression_IV_V1, regression_IV_V2,  regression_IV_V3, 
          dep.var.labels = c("Vaccination Participation"), # align = TRUE, #no.space = TRUE,
          row.sep.width = "15pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2), 
          align = TRUE, #no.spa,
          row.sep.width = "30pt", 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "latex", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("EL-MC", "PP-DW", "PS-PD", 
                               "C19-CC", "HC-PW", "EC-PO", 
                               "HC-HO", "EC-UN"), 
          omit.stat = c("adj.rsq", "rsq")
)





























