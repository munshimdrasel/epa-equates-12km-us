rm(list = ls())


library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)
require(readxl)
require(tidyverse)
library(lubridate)

#on my laptop
setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/epa-equates-12km-us")

#on Hopper
# setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/epa-equates-12km-us")

data_dir <- './data/'

data.files = list.files(data_dir, pattern = "*.csv")
read_csv_files <- function(x){
  df <- read.csv(path = paste(data_dir, x, sep = "/"))
  return(df)
}

df <- lapply(data.files, read_xlsx_files ) %>%
  bind_rows()

