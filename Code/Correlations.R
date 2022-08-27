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


df <- fread("Data/ToMerge/df_Unscaled.csv")


#*******************************************************************************
# Correlation Analysis  ========================================================
#*******************************************************************************

df <- df[, -c("FIPS", "PP-DE", "PP-RE", "PP-DL")]

attach(df)

df <- df %>%
  select("C19-TT", "C19-CC", "C19-DD", "C19-TC", "C19-MC", "C19-MA", "C19-MF", "C19-MS", 
         "C19-MR", "C19-MN", "C19-MO", "C19-PT", "C19-VC", 
         "EL-LC", "EL-MC", "EL-IN", 
         "HC-HB", "HC-HO", "HC-PW", 
         "EC-PO", "EC-UN", "EC-IN", 
         "PP-DW", 
         "PS-PD", "PS-RL")



df_cor <- df
df_cor <- df_cor[complete.cases(df_cor), ]


M <- cor(df_cor)
par(ask = TRUE)
corrplot(M, method = "square", order = "original", addrect = 3, tl.col = "black", 
         rect.lwd = 3, tl.cex = 1, insig = c("label_sig"), cl.offset = 0.5,
         cl.pos = "r", cl.ratio = 0.1, type = "lower" )









