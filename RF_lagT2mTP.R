# Random forest - Linh (8/10/2018)
# Predictors: precipitation (tp), air temperature (t2m), lag precipitation
# Predictand: Capacity factor of hydropower HRO and HRE (with const NGC)

rm(list = ls())
setwd("D:/WEMC/R_code/LD_RCodes_20180914")
source ('00_init.R')
source('getLagSequences.R')
source('rf_model.R')
source('getImportantLags.R')

library(zoo)
library(reshape2)
library(gridExtra)
library(DALEX)
library(forecast)
library(cowplot)


# Define countries, Energy type and period to examine
country_chosen <- c("AT","CH","DE","ES","FI","FR","IT","NO","PT","RO","SI","SK")
energy_chosen <- "HRE"
iteration_length <- 200     # maximum examined period for optimise_lag.R 
LABEL <- "with multiple TP & T2m lags"

# Assign the climate data as input and the generation of hydropower as output
# Climate data:     ERAInterim_daily  
#                   ERA5_daily_2000_2017
#                   ERA5_hourly_2000_2017
# Generation data:  ENTSOE_gen_new %>% filter(Country %in% country_chosen) %>%
#                                     select(Date, Country, Generation=energy_chosen)
#                   ENTSOE_gen_renewable_hourly %>% filter(Country %in% country_chosen, EnergyType==energy_chosen) %>%
#                                                   select(TS, Country, Generation)

load(file.path(PATH_OUTPUT, 'ERA5_daily_2000_2017.rda')) # ERA5 daily climate data
load(file.path(PATH_OUTPUT, 'Generation_all_2015_2018_test.rda')) # ENTSOE generation data

df_input  <-  ERA5_daily_2000_2017
df_target <-  ENTSOE_gen_new %>% filter(Country %in% country_chosen) %>%
  select(Date, Country, Generation=energy_chosen)

# Select common countries and period
ID_START <- max(first(df_input$Date), first(df_target$Date)) %>% print()
ID_END  <- min(last(df_input$Date), last(df_target$Date)) %>% print()

df_input_sel <- df_input %>% filter(Country %in% country_chosen) %>%
  filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END)))
df_target_sel <- df_target %>% filter(Country %in% country_chosen) %>%
  filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END)))

df_sel <- left_join(df_input_sel, df_target_sel)

ggplot(df_sel, aes(x=Date)) + 
  geom_line(aes(y = t2m), colour = "red") + labs(title = "Daily temperature and precipitation", y = "") +
  geom_line(aes(y = tp), colour = "steelblue") +
  facet_wrap(~Country, scales = "free")
rm(df_input_sel, df_target_sel)

### ================== Get lag sequences for precipitation & temperature ====================================

# Lag time for precipitation - limited in 200 days
ts <- data.frame(Date=unique(df_sel$Date))
lag_TP <- list(series = data.frame(Cummulative_days = seq(1,iteration_length,by=1)),
               max = data.frame(Country=country_chosen, lag_chosen = NA),
               input = data.frame())
lag_T2m <- list(series = data.frame(Cummulative_days = seq(1,iteration_length/2,by=1)),
                max = data.frame(Country=country_chosen, lag_chosen = NA),
                input = data.frame())

for (cnt in country_chosen) {
  # Precipitation
  df <- df_sel %>% filter(Country == cnt) %>% 
    select(Date, Input = tp, Target = Generation)
  
  o <- getLagSequences(df$Target, df$Input, aggr_function = mean, iteration =iteration_length) 
  if (is_empty(o$chosen_lag)) {
    next
  } else {
    lag_TP$max$lag_chosen[[which(lag_TP$max$Country==cnt)]] <- o$chosen_lag     # chosen value for lag time
    tmp1 <- data.frame(cnt=o$corr_lags) 
    lag_TP$series <- bind_cols(lag_TP$series,tmp1)   # to plot
    # Get series of lag precipitation
    colnames(o$cumulative_input) <- colnames(o$cumulative_input) %>% paste0("TP_",.)
    tmp2 <- data.frame(Date=ts, Country=cnt) %>% #, TP_lagN = o$opt_input) %>% 
      mutate_if(is.factor, as.character) %>%
      bind_cols(., o$cumulative_input) # add lag by increments
    lag_TP$input <- bind_rows(lag_TP$input, tmp2)    # input to RF model
  }
  
  # Temperature
  df2 <- df_sel %>% filter(Country == cnt) %>% 
    select(Date, Input = t2m, Target = Generation)
  
  o2 <- getLagSequences(df2$Target, df2$Input, aggr_function = mean, iteration =iteration_length/2) 
  if (is_empty(o2$chosen_lag)) {
    next
  } else {
    lag_T2m$max$lag_chosen[[which(lag_T2m$max$Country==cnt)]] <- o2$chosen_lag     # chosen value for lag time
    
    tmp3 <- data.frame(cnt=o2$corr_lags) 
    lag_T2m$series <- bind_cols(lag_T2m$series,tmp3)   # to plot
    
    # Get series of lag precipitation
    colnames(o2$cumulative_input) <- colnames(o2$cumulative_input) %>% paste0("Tm_",.)
    tmp4 <- data.frame(Date=ts, Country=cnt) %>% #, TP_lagN = o$opt_input) %>% 
      mutate_if(is.factor, as.character) %>%
      bind_cols(., o2$cumulative_input) # add lag by increments
    lag_T2m$input <- bind_rows(lag_T2m$input, tmp4)    # input to RF model
  }
}


# Get names of non-NA countries - find a neater way!
country_display <- lag_TP$max[!is.na(lag_TP$max$lag_chosen),] %>% pull(Country) %>% as.character() %>% print()
setdiff(country_chosen, country_display)     # countries cannot calculate optimal lag
colnames(lag_TP$series) <- c("Cumulative_days", country_display)
colnames(lag_T2m$series) <- c("Cumulative_days", country_display)
lag_TP$series <- lag_TP$series %>% gather(key="Country", value=Correlation, -`Cumulative_days`)
lag_T2m$series <- lag_T2m$series %>% gather(key="Country", value=Correlation, -`Cumulative_days`)

## Plots of lag time curves for countries with available data
ggplot(lag_TP$series, aes(x = Cumulative_days, y = Correlation)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Country, nrow = 3, scales = "free") +
  geom_text(data = lag_TP$max, aes(x=-Inf,y=Inf, label= paste("Optimal lag =", lag_chosen), 
                                   hjust = -0.3, vjust = 14))  +
  ggtitle(paste("Optimal cumulative precipitation for", energy_chosen, "ERA Interim")) +
  theme_grey()

ggplot(lag_T2m$series, aes(x = Cumulative_days, y = Correlation)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Country, nrow = 3, scales = "free") +
  geom_text(data = lag_T2m$max, aes(x=-Inf,y=Inf, label= paste("Optimal lag =", lag_chosen), 
                                    hjust = -0.3, vjust = 14))  +
  ggtitle(paste("Optimal cumulative temperature for", energy_chosen)) +
  theme_grey()
rm(df,df2,tmp1,tmp2,tmp3,tmp4)

### ========================= Random forest model ========================================================

# Prepare predictors

full_df <- df_sel %>% filter(Country %in% country_display) %>% select(-sd, -e) %>%
  left_join(., lag_TP$input, by = c("Date", "Country"))  %>% 
  left_join(., lag_T2m$input, by = c("Date", "Country"))

# Prepare training and validating dataset
new_full_df <- full_df[sample(nrow(full_df), replace=FALSE),]   # permute
training_df <- new_full_df[1:round(nrow(new_full_df)*2/3),]           # choose 2/3 data for training
validate_df <- anti_join(new_full_df, training_df, by = c("Date", "Country"))                # use the left 1/3 for validation

validate_df <- full_df %>% filter(between(as.Date(Date), as.Date("2016-11-01"), as.Date("2017-03-31")))
training_df <- anti_join(full_df, validate_df, by = c("Date", "Country"))


set.seed(123)
RF_results <- list(OOB = data.frame(), cross_valid = data.frame())
RF_df <- data.frame()
RF_predictors <- list()
RF_results_OOB <- data.frame()
RF_results_crossvalidate <- data.frame()
total_plot <- list()
i <- 1

for (cnt in country_display) {
  tmp <- training_df %>% filter(Country==cnt) %>% na.omit()
  message("RF model for ", cnt)
  
  ## RF preliminary round: bootstrapping --------------
  RF_pre <- rf_model(target = tmp %>% pull(Generation),
                     inputs = tmp %>% select_if(is.numeric) %>% select(-Generation),
                     ntrees = 500, node_size=5)
  vimp <- getImportantLags(model_explain = RF_pre$importance,
                           variable_1 = "TP", variable_2 = "Tm", range = 30)
  var_ID <- c("Date", "Country", "Generation", vimp$lags_chosen)
  
  ## RF main round -----------------------   
  training_main <- training_df[,var_ID] %>% 
    filter(Country==cnt) %>% na.omit()
  RF <- rf_model(target = training_main %>% pull(Generation),
                 inputs = training_main %>% select_if(is.numeric) %>% select(-Generation), 
                 ntrees = 500, node_size=1)
  
  # Get output
  # Out-of-bag coefficients 
  tmp3 <- RF$coefficients %>% mutate(Country = cnt) %>%
    mutate_if(is.factor, as.character)
  RF_results_OOB <- bind_rows(RF_results_OOB, tmp3)                  
  
  # Cross-validation
  valid_df <- validate_df[, var_ID] %>% filter(Country==cnt) %>% na.omit() 
  preds <- predict(RF$model, newdata = valid_df %>% select_if(is.numeric))
  RF_cor  = cor(preds, valid_df$Generation)
  RF_rmse = sqrt(mean((preds - valid_df$Generation)^2))  # root mean square error
  RF_mae  = mean(abs(preds - valid_df$Generation)) # mean absolute error
  RF_nmae  = 100 * mean(abs((preds - valid_df$Generation) / valid_df$Generation)) # normalised mean absolute error
  tmp4 <- data.frame(corr=RF_cor, nMAE=RF_nmae, MAE=RF_mae, RMSE=RF_rmse) %>% 
    mutate(Country = cnt) %>% mutate_if(is.factor, as.character)
  RF_results_crossvalidate <- bind_rows(RF_results_crossvalidate, tmp4)
  
  # qqplot compare observation vs model prediction
  RF_predictors[[paste(cnt)]] <- RF$predictors
  tmp5 <- data.frame(Date = valid_df$Date, Observed = valid_df$Generation, Estimated= preds) %>%  mutate(Country=cnt)
  RF_df <- bind_rows(RF_df, tmp5)                           
  
  
  # Model explain and the most important variables
  rf_varimp = plot(RF$importance) + theme(legend.position="none")
  top_3_variables = RF$importance %>% filter(!(variable %in% c("_baseline_", "_full_model_"))) %>% top_n(3, dropout_loss) %>%
    arrange(-dropout_loss) %>%
    pull(variable) %>% as.character()
  # partial dependence plots
  rf_1 = plot(single_variable(RF$explain, top_3_variables[1], type = 'pdp'))+ theme(legend.position="none")
  # rf_2 = plot(single_variable(RF$explain, top_3_variables[2], type = 'pdp'))+ theme(legend.position="none")
  if (!is.na(top_3_variables[2])) {
    rf_2 = plot(single_variable(RF$explain, top_3_variables[2], type = 'pdp'))+ theme(legend.position="none")
  } else (rf_2 = NULL)
  if (!is.na(top_3_variables[3])) {
    rf_3 = plot(single_variable(RF$explain, top_3_variables[3], type = 'pdp'))+ theme(legend.position="none")
  } else (rf_3 = NULL)
  
  total_plot[[i]]   = plot_grid(rf_varimp, rf_1, rf_2, rf_3, nrow = 1)
  i <- i+1
}

RF_predictors

## ====================== Some plots =============================================================================

# total_plot = plot_grid(row_1, row_2, row_3, row_4, ncol = 1)
# Add name!
do.call(grid.arrange, c(total_plot[1:3], ncol = 1))
do.call(grid.arrange, c(total_plot[4:6], ncol = 1))
do.call(grid.arrange, c(total_plot[7:9], ncol = 1))
do.call(grid.arrange, c(total_plot[10:12], ncol = 1))

# Plotting RF model's Out-of-bag coefficients
data.m <- melt(RF_results_OOB, id.vars='Country')
ggplot(data.m, aes(x=Country, y=value)) +
  geom_bar(fill="lightblue", stat = "identity", position="dodge") +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_text(aes(label = sprintf('%.2f', value)), 
            position = position_dodge(width = 0.9),
            vjust = -1.0 ) +
  ggtitle(paste("OOB coefficients for", energy_chosen, LABEL))

# Cross validation
data.m2 <- melt(RF_results_crossvalidate, id.vars='Country')
ggplot(data.m2, aes(x=Country, y=value)) +
  geom_bar(fill="lightblue", stat = "identity", position="dodge") +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_text(aes(label = sprintf('%.2f', value)), 
            position = position_dodge(width = 0.9), vjust = -1.0 ) +
  ggtitle(paste("Cross validation for", energy_chosen, LABEL))

# QQplot estimated vs observed values
a <- data.m2 %>% filter(variable == "corr") %>% select(Country, value)
RF_df <- RF_df %>% mutate(Month = month(Date)) %>%
  mutate(conf = (Estimated - Observed)/Observed*100)
ggplot(RF_df, aes(x=Observed, y=Estimated)) +
  geom_point(aes(colour = Month)) +
  scale_colour_gradientn(colours = c("blue","white","red","white","blue"), limits = c(1,12)) +
  geom_abline(intercept=0, slope = 1) +
  facet_wrap(~Country, nrow=3, scales = "free") +
  labs(title=paste("QQplot cross validation", energy_chosen, LABEL)) +
  geom_text(data = a, aes(x=-Inf,y=Inf, label= paste("cor =", sprintf('%.2f', value)), hjust = - 3, vjust = 14)) +
  theme_gray()

ggplot() +  
  geom_point(data = RF_df, aes(x=Date, y = Observed, colour = conf)) +
  scale_colour_gradientn(colours = c("darkblue","blue", "white","red", "darkred"),  limits = c(-100,100)) +
  facet_wrap(~Country, scales = "free")

rm(new_full_df,tmp, tmp1, tmp2,tmp3,tmp4, data.m,data.m2, o,i,a)

# Add vertical line of lag_x on correlation plot to compare
# Plotting lag time curves for countries with available data
# Note: different if using training_df
ggplot(lag_series, aes(x = Cumulative_days, y = Correlation)) +
  geom_point(alpha = 0.7) +
  geom_text(data = lag_max, aes(x=-Inf,y=Inf, label= paste("Optimal lag =", lag_chosen), 
                                hjust = -0.3, vjust = 30))  +
  geom_vline(data = TP_lag_value, aes(xintercept = TP_lag_value$lag_value)) +
  facet_wrap(~Country, nrow = 1, scales = "free") +
  ggtitle(paste("Optimal cumulative days for precipitation for", energy_chosen, LABEL)) +
  theme_grey()

