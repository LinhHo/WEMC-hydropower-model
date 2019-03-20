# Hydropower model application
# apply to seasonal forecast and climate projection
# Linh Ho 01/03/2019 - WEMC

rm(list=ls())
library(zoo)
library(tidyverse)
library(chron)
library(lubridate)
library(reshape2)
library(DALEX)
library(cowplot)



baseDir <- "D:/WEMC/R_code/LD_RCodes_20180914" # Change to your own directory
setwd(baseDir)
PATH_OUTPUT <- file.path(baseDir, 'OUTPUT')
PATH_TO_SF = 'D:/WEMC/R_code/LD_RCodes_20180914/DATA/SeasonalForecast/'

# Define countries, energy type, options for model
country_chosen <- c("AT","CH","DE","ES","FI","FR","IT","NO","PT","RO","SE", "SK")
energy_chosen <- "HRE"
climate_variables <- c("t2m", "tp")
iteration_length <- 200     # Maximal length of lag time (days)
LABEL <- "HRE 2-step"      # Title of the plots = Type of plot + LABEL

is.seasonal_model <- FALSE  # TRUE if want to use the seasonal model for predicting generation

# Read seasonal forecast (SF) data

SF_df <- data.frame()

for (i in 0:50) {
  filename = paste0(PATH_TO_SF, climate_variables, '/', 'S_SY05_ECMW_T159_TP-_0000m_Euro_1_nut0_S20190101_E20190804_ACC_MAP_06h_07m_qbc_org',
                    sprintf("_%02d_NA-_NA-_NA---.csv", i))
  if (file.exists(filename)) {
    d = read.csv(file = filename, header = TRUE, sep = ",")
    d <- d[-215,]  # the first days are not calculated
    d$X0 <- seq(from = as.Date("2018-01-02"), by = 1, length.out = 214)
    colnames(d)[1] <- "Date"
    d <- d %>% mutate(Model = sprintf("_%02d_", i))
  }
  SF_df <- bind_rows(SF_df, d)
}


tmp.m <- melt(SF_df, id.vars = c('Date', 'Model'))
colnames(tmp.m)[3:4] <- c("Country", "tp")
tmp.m$tp <- tmp.m$tp*1000

SF_full <- left_join(tmp_t2m, tmp.m)

tmp <- tmp.m %>% filter(Country == "FI") 
ggplot(tmp) + geom_line(aes(x=Date, y=t2m)) +
  facet_wrap(~Model) 

save(SF_full, file = file.path(PATH_TO_SF, "SF_full_S20190102_E20190803.rda"))

SF_sel <- SF_full %>% filter(Country %in% country_chosen)
SF_mean <- SF_sel %>% group_by(Date, Country) %>%
  summarise(Mean_t2m = mean(t2m), Mean_tp = mean(tp), 
            quantile25_t2m = quantile(t2m, 0.25), quantile75_t2m = quantile(t2m, 0.75),
            quantile25_tp = quantile(tp, 0.25), quantile75_tp = quantile(tp, 0.75))

ggplot() + geom_line(data=SF_sel, aes(x=Date, y=tp, colour = Model), alpha = 0.3) +
  geom_ribbon(data = SF_mean, aes(x = Date, ymin = quantile25_tp, ymax = quantile75_tp), alpha = 0.6) +
  geom_line(data=SF_mean, aes(x=Date, y=Mean_tp), size =1.2) +
  facet_wrap(~Country, scales = "free") + 
  ggtitle("Seasonal forecast 2019") +
  ylab("Precipitation (mm)") +
  theme_light(base_size = 14) +
  theme(legend.position = "none")


#############################################################################################################
# Hydropower model

baseDir <- "D:/WEMC/R_code/LD_RCodes_20180914/WEMC-hydropower-model/" # Change to your own directory
setwd(baseDir)
PATH_DATA <- paste0(baseDir, "DATA")
PATH_FIGURE <- paste0(baseDir, "FIGURE")

source('getLagSequences.R')
source('getImportantLags.R')
source('getcoefs.R')
source('getHighProductionPeriod.R')
source('rf_model.R')
source('rf_full_model.R') # functions within function, must be called last


# Temporary set the SF time is 2018, although the real period is 2019
load(file.path(PATH_DATA, "SF_full_S20190102_E20190803.rda"))
SF_sel <- SF_full %>% filter(Country %in% country_chosen) %>% mutate_if(is.factor, as.character)

# For section (2): Period to make estimation, for data reconstruction, seasonal forecast, etc.
# Training on the whole available dataset of generation
SF_START <- as.Date(min(SF_sel$Date))
SF_END <- as.Date(max(SF_sel$Date))

# Prepare data
load(file.path(PATH_DATA, 'ERA5_daily_1979_2018_SE.rda')) # ERA5 daily climate data file: t2m, tp, sd, e
load(file.path(PATH_DATA, 'Generation_all_2015_2018.rda')) # ENTSOE generation data: HRE, HRO, SOL, WOF, WON

df_input  <-  ERA5_daily_1979_2018_SE
df_target <-  ENTSOE_gen_new 

# temporary for 2018
# Training dataset
training_START <- as.Date("2015-01-01")
training_END <- as.Date("2018-01-01")

# Check if observed and SF data are continuous
if (SF_START - training_END != 1) {print("Warning: The training_END and SF_START must be subsequent")} 

# Select countries in study
df_input_sel <- df_input %>% filter(Country %in% country_chosen) %>%
  filter(between(as.Date(Date), as.Date(training_START),as.Date(training_END)))
df_target_sel <- df_target %>% filter(Country %in% country_chosen) %>% select(Date, Country, Generation=energy_chosen) %>%
  filter(between(as.Date(Date), as.Date(training_START),as.Date(training_END))) %>%
  filter(!is.na(Generation)) # unselect countries without generation data

# Countries in study
country_display <- unique(df_target_sel$Country) %>% print()
setdiff(country_chosen, country_display)     # countries without generation data

# Multiple lags with observed and SF data
# Include country without generation
ts <- data.frame(Date = seq(from = training_START, to = SF_END, by =1))
lag_df <- data.frame()

for (i in 0:50) {
  message("Lag times for model", sprintf("_%02d_", i))
  tmp_lag_bymodel <- data.frame()
  
  # Bind observed data with seasonal forecast
  tmp_SF <- SF_sel %>% filter(Model == sprintf("_%02d_", i)) %>% 
            mutate_if(is.factor, as.character) %>% select(-Model)
  df_all <- bind_rows(df_input_sel, tmp_SF) %>% arrange(Country, Date)
  
  for (cnt in country_chosen) {
    
    # Compute lag time for every country
    tmp <- df_all %>% filter(Country == cnt)
    if (dim(tmp)[1]==0) {  # dataframe with zero row
      stop(paste("Input to calculate lag times should not be empty: Country", cnt))
    }
    tmp_lag_bycountry <- data.frame(Date=ts, Country=cnt) %>%  mutate_if(is.factor, as.character)
    
    # lag times for every climate variable
    for (clim in climate_variables) {
      df1 <- tmp %>%  select(Date, Input = clim)
      o <- getLagSequences(df1$Input, 
                            aggr_function = ifelse(clim =="tp", sum, mean), # calculate the cumulative sum for precipitation, otherwise the mean for other variables
                            iteration = ifelse(clim == "t2m", iteration_length/2, iteration_length)) # only temperature use half of the iteration range
      colnames(o$df_cumulative) <- colnames(o$df_cumulative) %>% paste0(clim, "_",.)
      tmp_lag_bycountry <- bind_cols(tmp_lag_bycountry, o$df_cumulative)
    }
    tmp_lag_bymodel <- bind_rows(tmp_lag_bymodel, tmp_lag_bycountry)
    tmp_lag_bymodel <- tmp_lag_bymodel %>% mutate(Model = sprintf("_%02d_", i))
    
  }
  
  # Add daily data to the input
  colnames(df_all)[c(3,4)] <- paste0(climate_variables, "_lag0")
  tmp_lag_bymodel <- left_join(df_all, tmp_lag_bymodel, by = c("Date", "Country"))
  
  lag_df <- bind_rows(lag_df, tmp_lag_bymodel)
}

rm(tmp_lag_bycountry, tmp_lag_bymodel, tmp_SF, tmp, df_all, df1, o)

# Define high or low production period

# Train the model with existing energy data and observed climate data =================
# The training period was already define above with training_START and END
# only take one frame of observed data, this frame duplicates 51 times when applied SF

df_input_withlag <- lag_df %>% filter(Model == "_00_") %>%
                   filter(between(as.Date(Date), as.Date(training_START),as.Date(training_END))) %>% select(-Model)
training_df <- left_join(df_input_withlag, df_target_sel, by = c("Date", "Country"))

set.seed(123)
RF_model_list <- vector(mode = "list", length = length(country_display))
plot_list <- vector(mode = "list", length = length(country_display))

for (cnt in country_display) {
  
  tmp <- training_df %>% filter(Country==cnt) %>% select_if(is.numeric)
  message("Training RF model for ", cnt)
  
  if (is.seasonal_model == TRUE) {
    # High production period
    tmp_high <- tmp %>% filter(is.highproduction == TRUE)
    if (nrow(tmp_high)!=0) {
      RF_high <- rf_full_model(tmp_high %>% select_if(is.numeric), labs = "high")
    }
    # Low production period
    tmp_low <- tmp %>% filter(is.highproduction == FALSE)
    if (nrow(tmp_low)!=0) {
      RF_low <- rf_full_model(tmp_low %>% select_if(is.numeric), labs = "low")
    }
    # Save the seasonal models and variable important plots
    RF_model_list[[cnt]] <- list(high = RF_high$model, low = RF_low$model)
    plot_list[[cnt]] <- list(high = RF_high$plots, low = RF_low$plots)
    
  } else {
    # Normal (non seasonal) model
    RF_nor <- rf_full_model(tmp %>% select_if(is.numeric), labs = "normal")
    # Save model and plots
    RF_model_list[[cnt]] <- RF_nor$model
    plot_list[[cnt]] <- RF_nor$plots
  }
}

# Predict/ seasonal forecast =============

# Prepare the seasonal forecast climate data with multiple lag

new_input <- lag_df  %>% filter(between(as.Date(Date), as.Date(SF_START),as.Date(SF_END)))

RF_output <- data.frame()

for (i in 0:50) {
  message("Seasonal forecast for model ", sprintf("_%02d_", i))
  RF_output_bymodel <- data.frame()
  tmp_new_input <- new_input %>% filter(Model == sprintf("_%02d_", i)) %>% 
                  mutate_if(is.factor, as.character) %>% select(-Model)
  
  for (cnt in country_display) {
    tmp_bycountry <- tmp_new_input %>% filter(Country==cnt) %>% na.omit()

    if (is.seasonal_model == TRUE) {
      
      # Get estimated generation from 'high' and 'low' model, then add as a new column to the input dataframe
      df_high <- data.frame()
      df_low <- data.frame()
      
      tmp_high <- tmp_bycountry %>% filter(is.highproduction == TRUE)
      if (nrow(tmp_high)!=0) {
        df_high <- tmp_high %>% mutate(Generation = predict(RF_model_list[[cnt]][['high']][['model']], newdata = tmp_high %>% select_if(is.numeric))) %>%
          select(Date, Country, Generation)
      }
      tmp_low <- tmp_bycountry %>% filter(is.highproduction == FALSE)
      if (nrow(tmp_low)!=0) {
        df_low <- tmp_low %>% mutate(Generation = predict(RF_model_list[[cnt]][['low']][['model']], newdata = tmp_low %>% select_if(is.numeric))) %>%
          select(Date, Country, Generation)
      }
      tmp2 <- bind_rows(df_high, df_low) %>% arrange(Date)
      RF_output <- bind_rows(RF_output, tmp2)
      
    } else {
      # non-seasonal model
      df_nor <- tmp_bycountry %>% mutate(Generation = predict(RF_model_list[[cnt]][['model']], newdata = tmp_bycountry %>% select_if(is.numeric))) %>%
        select(Date, Country, Generation) %>% mutate(Data = "SF", Model = sprintf("_%02d_", i))
      RF_output_bymodel <- bind_rows(RF_output_bymodel, df_nor)
    }
  }
  RF_output <- bind_rows(RF_output, RF_output_bymodel)
}

# Plot with shadded P25/75
# Smoothed with rolling mean 30 days

RF_output_mean <- RF_output %>% group_by(Date, Country) %>%
  summarise(Mean_gen = mean(Generation), 
            quantile25_gen = quantile(Generation, 0.25), 
            quantile75_gen = quantile(Generation, 0.75)) %>%
  arrange(Country)  # Make sure that data is sorted by Country

# Calculate rolling mean for each country in each model

rollbyCountry <- function (df, variable, window) {
  df_sel <- df %>% select(Date, Country, variable)
  df_rolled <- data.frame()
  for (cnt in country_display) {
    tmp <- df_sel %>% filter(Country == cnt)
    tmp[[variable]] <- rollmean(tmp[[variable]], k = window, fill = NA, align = "center")
    colnames(tmp)[3] <- paste0(colnames(tmp)[3], "_rolled")
    df_rolled <- bind_rows(df_rolled, tmp)
  }
  return(df_rolled)
}

aa <- rollbyCountry(RF_output_mean, "quantile25_gen", window = 10)
bb <- rollbyCountry(RF_output_mean, "quantile75_gen", window = 10)
RF_output_mean <- left_join(RF_output_mean, aa) %>% left_join(., bb) 
RF_output_mean[,c(3:7)] <- RF_output_mean[,c(3:7)]/1000  # convert to GW
RF_output$Generation <- RF_output$Generation/1000

df_target_toplot <- df_target_sel %>% mutate(Gen = Generation/1000) %>% select(-Generation)
cc <- rollbyCountry(df_target_toplot, "Gen", 15)
df_target_toplot <- left_join(df_target_toplot, cc)

ggplot() + geom_line(data=RF_output, aes(x=Date, y=Generation, colour = Model), alpha = 0.4) +
  geom_ribbon(data = RF_output_mean, aes(x = Date, ymin = quantile25_gen_rolled, ymax = quantile75_gen_rolled), alpha = 0.6) +
  geom_line(data=RF_output_mean, aes(x=Date, y=Mean_gen), size =1.2) +
  geom_line(data = df_target_toplot, aes(x=Date, y = Gen_rolled), colour = "steelblue") +
  facet_wrap(~Country, scales = "free") + 
  ggtitle("Seasonal forecast HRE 2019") +
  ylab("Generation (GW)") +
  theme_light(base_size = 14) +
  theme(legend.position = "none")



