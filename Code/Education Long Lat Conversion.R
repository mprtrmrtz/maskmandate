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

setwd("C:/Users/momaleki/Desktop/Research/Project")
getwd()



Uni_Info <-    fread("Data/InstitutionCampus.csv")
Uni_Info <- Uni_Info[ , -c(1:3, 5:7, 9:14)]
colnames(Uni_Info) <- c('Recipient', 'Address')

head(Uni_Info)


Fed_Investment <- fread("Data/Federal Education Investment.csv")
Fed_Investment <- Fed_Investment[, -c(2:7)]
head(Fed_Investment)


merged <- merge(Uni_Info, Fed_Investment, on = "Recipient")


head(merged)


library(ggmap)
register_google(key = "AIzaSyCyNXUpS7dLRlTULTGirJECrMe6MCjFcmY")

for (i in 1:nrow(merged)){
  
  gc <- geocode(merged$Address[i])
  merged$LON[i] <- gc$lon
  merged$LAT[i] <- gc$lat 
}

haed(merged)

write_csv(merged, "Data/IV Regression/merged.csv")































































