
#*******************************************************************************
# Housekeeping Items ===========================================================
#*******************************************************************************

rm(list = ls())

setwd("G:/My Drive/Research/Mask Mandate/")

source("Code/R/Regression Analysis.R")
source("Code/R/IV Regression.R")

## Results =============


iv.fit <- mget(c('reg_M3', 'reg_IV_M3', 
                 'reg_C3', 'reg_IV_C3', 
                 'reg_V3', 'reg_IV_V3'))




stargazer( 
  reg_M3,
  reg_IV_M3,
  reg_C3,
  reg_IV_C3,
  reg_V3,
  reg_IV_V3,
 dep.var.labels = c( "Mobility Restrictions", "Mask Adoption",  "Vaccine Participation"),
# align = TRUE, no.space = F,
# row.sep.width = "30pt",
# column.sep.width = "1pt",
 type = "text" , #omit.stat = c("rsq", 'f', "adj.rsq", 'ser')
omit.stat = "all", 
add.lines = list(c("Weak instruments", " ", 0, " ", 0, " ", 0), 
                 c("Wu-Hausman", " ", 0.21, " ", 0.04, " ", 0))
 # ,
 #         covariate.labels = c('PS-PD', 'EL-MC', 'PP-DW', 'C19-CC', 'HC-PW', 
 #                              'EC-PO', 'HC-HO', 'EC-UN')
          )


