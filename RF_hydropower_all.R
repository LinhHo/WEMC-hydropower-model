# Random forest model - Linh Ho (8/10/2018)
# Target: Hydropower generation - run-of-river (HRO) or reservoir-based (HRE)
# Predictors: lag time of precipitation (tp), air temperature (t2m), +/- snow depth (sd)
# 2-round model: multiple lag time with preliminary and main RF rounds
# Seasonal model: with high and low production periods

# Section 1: Model validation
# Section 2: Model hindcast/ prediction

# PLOT [N] - refer to plot numbered N in the RF_plots.R file


rm(list = ls())
library(zoo)
library(reshape2)
library(gridExtra)
library(DALEX)
library(forecast)
library(cowplot)
library(chron)
library(seas)
library(lubridate)
library(readxl)
library(tidyverse)
select <- dplyr::select # avoid conflict with 'select' function from other packages

baseDir <- "D:/WEMC/R_code/LD_RCodes_20180914/WEMC-hydropower-model/" # Change to your own directory
setwd(baseDir)

source('getLagSequences.R')
source('getImportantLags.R')
source('getcoefs.R')
source('getHighProductionPeriod.R')
source('rf_model.R')
source('rf_full_model.R') # functions within function, must be called last

PATH_DATA <- paste0(baseDir, "DATA")
PATH_FIGURE <- paste0(baseDir, "FIGURE")

# Define countries, energy type, options for model
country_chosen <- c("AT","CH","DE","ES","FI","FR","IT","NO","PT","RO","SI","SK")
energy_chosen <- "HRE"
climate_variables <- c("t2m", "tp")
iteration_length <- 200     # Maximal length of lag time (days)
LABEL <- "HRO 2-round"      # Title of the plots = Type of plot + LABEL

is.seasonal_model <- TRUE  # TRUE if want to use the seasonal model for predicting generation
#                           # FALSE if onnly use the 2-round model


# For section (1): Validation period, for evaluating the model
validate_START <- as.Date("2017-01-01")
validate_END <- as.Date("2017-12-31")

# For section (2): Period to make estimation, for data reconstruction, seasonal forecast, etc.
# Training on the whole available dataset of generation
estimate_START <- as.Date("2000-01-01")
estimate_END <- as.Date("2014-12-31")

# Prepare data
load(file.path(PATH_DATA, 'ERA5_daily_2000_2018.rda')) # ERA5 daily climate data file: t2m, tp, sd, e
load(file.path(PATH_DATA, 'Generation_all_2015_2018.rda')) # ENTSOE generation data: HRE, HRO, SOL, WOF, WON

df_input  <-  ERA5_daily_2000_2018
df_target <-  ENTSOE_gen_new 

# Select countries in study
df_input_sel <- df_input %>% filter(Country %in% country_chosen) 
df_target_sel <- df_target %>% filter(Country %in% country_chosen) %>% select(Date, Country, Generation=energy_chosen) %>%
  filter(!is.na(Generation)) # unselect countries without generation data

# Countries in study
country_display <- unique(df_target_sel$Country) %>% print()
setdiff(country_chosen, country_display)     # countries without generation data

## PLOT [1] - Examine the climate data

### ================== Get lag sequences for precipitation & temperature ====================================

# Lag time for climate variables - with 5-day increments
ts <- data.frame(Date=unique(df_input_sel$Date))
lag_df <- data.frame()

for (cnt in country_chosen) {
  tmp <- df_input_sel %>% filter(Country == cnt)
  tmp_lag <- data.frame(Date=ts, Country=cnt) %>%  mutate_if(is.factor, as.character)
  
  # use the same iteration_length for all climate variables, e.g. 200 days here ?!!
  for (clim in climate_variables) {
    df1 <- tmp %>%  select(Date, Input = clim)
    o1 <- getLagSequences(df1$Input, 
                          aggr_function = ifelse(clim =="tp", sum, mean), # calculate the cumulative sum for precipitation, otherwise the mean for other variables
                          iteration = ifelse(clim == "t2m", iteration_length/2, iteration_length)) # only temperature use half of the iteration range
    colnames(o1$df_cumulative) <- colnames(o1$df_cumulative) %>% paste0(clim, "_",.)
    tmp_lag <- bind_cols(tmp_lag, o1$df_cumulative)
  }
  lag_df <- bind_rows(lag_df, tmp_lag)
}

rm(df1,tmp_lag,tmp,o1)

##########################################################################################################
### ========================= Random forest model ========================================================

# Dataframe of input full length, with selected climate data and multiple lags
full_input <- df_input_sel[, c("Date", "Country")] %>% 
              left_join(., lag_df, by = c("Date", "Country"))

# High production periods vary between countries
# Create in the input dataframe a new boolean variable is.highproduction 

tmp2 <- data.frame()
list_highproduction <- getHighProductionPeriod(input = df_target_sel, labs = energy_chosen)
for (cnt in country_display) {
  tmp <- full_input %>% filter(Country == cnt)
  tmp <- tmp %>% mutate(is.highproduction = ifelse(month(Date) %in% list_highproduction$is.highproduction[[cnt]], TRUE, FALSE))
  tmp2 <- bind_rows(tmp2, tmp)
}
full_input <- tmp2
rm(tmp, tmp2)

## PLOT [2] - Monthly generation and High production period 

# Select common period of climate and energy dataset
ID_START <- max(first(df_input$Date), first(df_target$Date)) %>% print()
ID_END  <- min(last(df_input$Date), last(df_target$Date)) %>% print()

df_sel <- left_join(full_input %>% filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END))),
                    df_target_sel %>% filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END))),
                    by = c("Date", "Country"))


###############################################################################################
# ============== Section (1): Model with validation ===============================================================

## Prepare training and validating dataset 
validate_df <- df_sel %>% filter(between(as.Date(Date), validate_START, validate_END))
training_df <- anti_join(df_sel, validate_df, by = c("Date", "Country")) ### FIX THIS full_df

set.seed(123)
RF_model_list <- vector(mode = "list", length = length(country_display))
plot_list <- vector(mode = "list", length = length(country_display))

for (cnt in country_display) {
  tmp <- training_df %>% filter(Country==cnt)
  message("RF model for ", cnt)
  
  # Normal (non seasonal) model
  RF_nor <- RF_full_model(tmp %>% select_if(is.numeric), labs = "normal")
  
  # Divide input to train separately 2 seasonal sub-models
  # High production period
  tmp_high <- tmp %>% filter(is.highproduction == TRUE)
  if (nrow(tmp_high)!=0) {
    RF_high <- RF_full_model(tmp_high %>% select_if(is.numeric), labs = "high")
  }  
  # Low production period
  tmp_low <- tmp %>% filter(is.highproduction == FALSE)
  if (nrow(tmp_low)!=0) {
    RF_low <- RF_full_model(tmp_low %>% select_if(is.numeric), labs = "low")
  }
  
  RF_model_list[[cnt]] <- list(normal = RF_nor$model, high = RF_high$model, low = RF_low$model)
  plot_list[[cnt]] <- list(normal = RF_nor$plots, high = RF_high$plots, low = RF_low$plots)
  
}

## PLOT [3] - Plot variable importance for all countries

rm(tmp, tmp_high, tmp_low, RF_high, RF_low, RF_nor)

# Validation and comparison =============
# Normal (non seasonal) model - benchmark

RF_normal_df <- data.frame()
RF_normal_coef <- data.frame()
for (cnt in country_display) {
  tmp <- validate_df %>% filter(Country == cnt) %>% na.omit()
  tmp <- tmp %>% mutate(Estimated = predict(RF_model_list[[cnt]][['normal']][['model']], newdata = tmp %>% select_if(is.numeric))) %>%
    select(Date, Country, Observed = Generation, Estimated)
  RF_normal_df <- bind_rows(RF_normal_df, tmp)
  
  tmp3 <- getcoefs(tmp$Observed, tmp$Estimated) %>%
    mutate(Country = cnt) %>% mutate_if(is.factor, as.character)
  RF_normal_coef <- bind_rows(RF_normal_coef, tmp3)
}

# Seasonal model

RF_seasonal_df <- data.frame()
RF_seasonal_coef <- data.frame()
for (cnt in country_display) {
  tmp <- validate_df %>% filter(Country == cnt) %>% na.omit()
  df_high <- data.frame()
  df_low <- data.frame()
  
  # Filter and predict high production period
  tmp_high <- tmp %>% filter(is.highproduction == TRUE)
  if (nrow(tmp_high)!=0) {
    df_high <- tmp_high %>% mutate(Estimated = predict(RF_model_list[[cnt]][['high']][['model']], newdata = tmp_high %>% select_if(is.numeric))) %>%
      select(Date, Country, Observed = Generation, Estimated)
  }
  # Low production
  tmp_low <- tmp %>% filter(is.highproduction == FALSE)
  if (nrow(tmp_low)!=0) {
    df_low <- tmp_low %>% mutate(Estimated = predict(RF_model_list[[cnt]][['low']][['model']], newdata = tmp_low %>% select_if(is.numeric))) %>%
      select(Date, Country, Observed = Generation, Estimated)
  }
  tmp2 <- bind_rows(df_high, df_low) %>% arrange(Date)
  RF_seasonal_df <- bind_rows(RF_seasonal_df, tmp2)
  
  # Get coefficients
  tmp3 <- getcoefs(tmp2$Observed, tmp2$Estimated) %>%
    mutate(Country = cnt) %>% mutate_if(is.factor, as.character)
  RF_seasonal_coef <- bind_rows(RF_seasonal_coef, tmp3)
}
rm(tmp, tmp_high,tmp_low,tmp2,tmp3, df_high,df_low)

## PLOT [4] - Comparison (correlation, nMAE, MAE, RMSE) normal and seasonal models


###############################################################################################################
### ======== Section (2) :: Train model on full available dataset to hindcast/predict generation =======================

training_df <- df_sel

# new input with estimate period and selected climate variables
new_input <- full_input  %>% filter(between(as.Date(Date), as.Date(estimate_START),as.Date(estimate_END)))

# Train the model =============

set.seed(123)
RF_model_list <- vector(mode = "list", length = length(country_display))
plot_list <- vector(mode = "list", length = length(country_display))

for (cnt in country_display) {
  tmp <- training_df %>% filter(Country==cnt)
  message("Training RF model for ", cnt)
  
  if (is.seasonal_model == TRUE) {
    # High production period
    tmp_high <- tmp %>% filter(is.highproduction == TRUE)
    if (nrow(tmp_high)!=0) {
      RF_high <- RF_full_model(tmp_high %>% select_if(is.numeric), labs = "high")
    }
    # Low production period
    tmp_low <- tmp %>% filter(is.highproduction == FALSE)
    if (nrow(tmp_low)!=0) {
      RF_low <- RF_full_model(tmp_low %>% select_if(is.numeric), labs = "low")
    }
    # Save the seasonal models and variable important plots
    RF_model_list[[cnt]] <- list(high = RF_high$model, low = RF_low$model)
    plot_list[[cnt]] <- list(high = RF_high$plots, low = RF_low$plots)
    
  } else {
    # Normal (non seasonal) model
    RF_nor <- RF_full_model(tmp %>% select_if(is.numeric), labs = "normal")
    # Save model and plots
    RF_model_list[[cnt]] <- RF_nor$model
    plot_list[[cnt]] <- RF_nor$plots
  }
  
}

# Hindcast/ predict the generation =============

RF_output <- data.frame()

for (cnt in country_display) {
  tmp <- new_input %>% filter(Country==cnt) %>% na.omit()
  message("Predicting RF model for ", cnt)
  
  if (is.seasonal_model == TRUE) {
    
    # Get estimated generation from 'high' and 'low' model, then add as a new column to the input dataframe
    df_high <- data.frame()
    df_low <- data.frame()
    
    tmp_high <- tmp %>% filter(is.highproduction == TRUE)
    if (nrow(tmp_high)!=0) {
      df_high <- tmp_high %>% mutate(Generation = predict(RF_model_list[[cnt]][['high']][['model']], newdata = tmp_high %>% select_if(is.numeric))) %>%
        select(Date, Country, Generation)
    }
    tmp_low <- tmp %>% filter(is.highproduction == FALSE)
    if (nrow(tmp_low)!=0) {
      df_low <- tmp_low %>% mutate(Generation = predict(RF_model_list[[cnt]][['low']][['model']], newdata = tmp_low %>% select_if(is.numeric))) %>%
        select(Date, Country, Generation)
    }
    tmp2 <- bind_rows(df_high, df_low) %>% arrange(Date)
    RF_output <- bind_rows(RF_output, tmp2)
    
  } else {
    # non-seasonal model
    df_nor <- tmp %>% mutate(Generation = predict(RF_model_list[[cnt]][['model']], newdata = tmp %>% select_if(is.numeric))) %>%
      select(Date, Country, Generation) %>% mutate(Data = "estimated")
    RF_output <- bind_rows(RF_output, df_nor)
  }
  
}

## PLOT [5] - Time series of generation data estimated & observed

# Compare reconstructed data from model with RTE generation data (to be continued)

