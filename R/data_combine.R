rm(list = ls())

library(fst)
library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)
require(readxl)
require(tidyverse)
library(lubridate)
library(splitstackshape)

#on my laptop
# setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/epa-equates-12km-us")

#on Hopper
setwd ("/projects/HAQ_LAB/mrasel/R/epa-equates-12km-us")

# data_dir <- './data/2019'

year <- c(2002:2014)
month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

#on hopper
# data_dir <- '/projects/HAQ_LAB/mrasel/data/EQUATES/2019'


# df2 <- read.table("./data/2019/Daily_EQUATES_CMAQv532_cb6r3_ae7_aq_STAGE_12US1_201901.csv", 
#                   sep = "\t", header = FALSE, skip = 4)

datalist = list()

for (j in 1:length(year)){
  

for (i in 1:length(month)) {
  
 df2 <-  read.table(paste0("/projects/HAQ_LAB/mrasel/data/EQUATES/", year[j],"/Daily_EQUATES_CMAQv532_cb6r3_ae7_aq_STAGE_12US1_",
                          year[j],month[i],".csv"), sep = "\t", header = FALSE, skip = 4)
 df2 <- cSplit(df2, "V1", ",")

old.names <- c ("V1_01", "V1_02", "V1_03", "V1_04", "V1_05", "V1_06", "V1_07", "V1_08", "V1_09", "V1_10",
                "V1_11", "V1_12", "V1_13", "V1_14", "V1_15", "V1_16", "V1_17", "V1_18", "V1_19", "V1_20",
                "V1_21", "V1_22", "V1_23", "V1_24", "V1_25", "V1_26")
new.names <- c ("column", "row", "longitude", "latitude", "Lambert_X", "LAMBERT_Y", "date", "O3_MDA8", "O3_AVG", 
                "CO_AVG", "NO_AVG", "NO2_AVG", "SO2_AVG", "CH2O_AVG", "PM10_AVG", "PM25_AVG",
                "PM25_SO4_AVG", "PM25_NO3_AVG", "PM25_NH4_AVG", "PM25_OC_AVG", "PM25_EC_AVG", "SO2_1HRMAX", "CO_1HRMAX", 
                "NO_1HRMAX", "NO2_1HRMAX","CO_MDA8")

																	
df2  <- df2%>% rename_at(vars(old.names), ~ new.names)

datalist[[i]] <- df2

}

spc.data = do.call(rbind, datalist)

write.fst(spc.data, paste0("./data/species.data.",year[j],".fst"))

}

