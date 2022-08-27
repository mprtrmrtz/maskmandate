
#*******************************************************************************
# Housekeeping Items ===========================================================
#*******************************************************************************

# rm(list = ls())

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

setwd("G:/My Drive/Research/Mask Mandate/")
getwd()




gaze.lines.ivreg.diagn <- function(x, col="p-value", row=1:3, digits=2){
  stopifnot(is.list(x))
  out <- lapply(x, function(y){
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
# IV regs        ========================================================
#*******************************************************************************

attach(df)

### Mask Usage -----

reg_IV_C1 <- ivreg(data = df, 
                       `C19-MC` ~   
                         `EL-MC` | `EL-IN`)

reg_IV_C2 <- ivreg(data = df, 
                       `C19-MC` ~   
                         `EL-MC` + `PP-DW` | `EL-IN` + `PP-DW`)

reg_IV_C3 <- ivreg(data = df, 
                       `C19-MC` ~   
                         `EL-MC`  +`PP-DW` +`PS-PD`+
                         `C19-CC` + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN` | `EL-IN` + `PP-DW`+ `PS-PD`+
                         `C19-CC` + 
                         `HC-PW`  + 
                         `EC-PO`  + 
                         `HC-HO`  + 
                         `EC-UN` ) 

iv.fit <- mget(paste0("reg_IV_C", 1:3))

stargazer(reg_IV_C1, reg_IV_C2,  reg_IV_C3, 
          dep.var.labels = c("Mask Adoption"),  align = TRUE, no.space = F,
          row.sep.width = "5pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2), 
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "text", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("EL-MC", "PP-DW", "PS-PD", 
                               "C19-CC", "HC-PW", "EC-PO", 
                               "HC-HO", "EC-UN"), 
          omit.stat = c("adj.rsq", "rsq"))


### Movement Change -----


reg_IV_M1 <- ivreg(data = df, 
                       `C19-MO` ~   
                         `EL-MC` | `EL-IN`)

reg_IV_M2 <- ivreg(data = df, 
                       `C19-MO` ~   
                         `EL-MC` + `PP-DW`| `EL-IN` + `PP-DW`)

reg_IV_M3 <- ivreg(data = df, 
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

iv.fit <- mget(paste0("reg_IV_M", 1:3))

stargazer(reg_IV_M1, reg_IV_M2,  reg_IV_M3, 
          dep.var.labels = c("Mobility Restrictions"), # align = TRUE,#  no.space = TRUE,
          row.sep.width = "15pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2), 
          align = TRUE, #no.spa,
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "text", 
          # se = list(NULL, robust.se), 
          covariate.labels = c("EL-MC", "PP-DW", "PS-PD", 
                               "C19-CC", "HC-PW", "EC-PO", 
                               "HC-HO", "EC-UN"), 
          omit.stat = c("adj.rsq", "rsq"))



## Vaccine Usage -----

reg_IV_V1 <- ivreg(data = df, 
                       `C19-VC` ~   
                         `EL-MC` | `EL-IN`)

reg_IV_V2 <- ivreg(data = df, 
                       `C19-VC` ~   
                         `EL-MC` + `PP-DW`| `EL-IN` + `PP-DW`)

reg_IV_V3 <- ivreg(data = df, 
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

iv.fit <- mget(paste0("reg_IV_V", 1:3))


stargazer(reg_IV_M3, reg_IV_C3,  reg_IV_V3, 
          dep.var.labels = c( "Mobility Restrictions", "Mask Adoption",  "Vaccine Participation"), # align = TRUE, #no.space = TRUE,
          row.sep.width = "15pt", 
          add.lines= gaze.lines.ivreg.diagn(lapply(iv.fit[1:3], 
                                                   summary, diagnostics=TRUE), row=1:2), 
          align = TRUE, #no.spa,
          column.sep.width = "1pt", 
          # single.row = T, 
          type = "text", 
          # se = list(NULL, robust.se), 
          covariate.labels =  c( "Education Levels", "Political Preference", "Population Density","COVID-19 Cases",
                                 "Essential Workers", "Poverty Estimates", "Hospital Capacity", "Unemployment"), 
          
         
          
          omit.stat = c("adj.rsq", "rsq")
)





























