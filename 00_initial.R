# Prepare directories/ folders

library(chron)
library(seas)
library(lubridate)
library(readxl)
library(tidyverse)

rm(list=ls())

baseDir <- "D:/WEMC/R_code/LD_RCodes_20180914/"
ECEMDataDir <- file.path(baseDir, "DATA/ECEM_DATA")
ECEM_outputDir <- "D:/WEMC/R_code/LD_RCodes_20180914/HISTORICAL"

scriptsDir <- file.path(baseDir,"SCRIPTS")
setwd(scriptsDir)

PATH_TO_ENTSOE = 'D:/WEMC/R_code/LD_RCodes_20180914/DATA/ENTSOE'
PATH_TO_DATA = 'D:/WEMC/R_code/LD_RCodes_20180914/DATA'
PATH_TO_ERA5 = 'D:/WEMC/R_code/LD_RCodes_20180914/DATA/ERA5'

PATH_OUTPUT = file.path(baseDir, 'OUTPUT')

# Read ECEM cluster & country info ----------

load (file.path(ECEMDataDir,"CONSTANT.RData"))
country_names <- as.matrix(read.csv(file.path(ECEMDataDir,"ECEM_countrynames.csv"), header=T))
ncountries <- nrow(country_names)
nclusters <- nrow(cluster_names) # nom de clusters
cnt_list <- sort(country_names[,2]) %>% print()

# Read ECEM combination of parameters for energy variables ----------

paramfile <- file.path(ECEMDataDir,"ECEM_parameters.csv")
parameters <- as.data.frame(read.table(paramfile, sep=";", header=TRUE, stringsAsFactors=FALSE))
