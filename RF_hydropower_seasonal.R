# Random forest model - Linh Ho (8/10/2018)
# Seasonal model: with high and low production periods
# Target: Hydropower generation of run-of-river (HRO) and reservoir (HRE)
# Predictors: lag time of precipitation (tp), air temperature (t2m), snow depth (sd)


rm(list = ls())
setwd("D:/WEMC/R_code/LD_RCodes_20180914")
source ('00_init.R')
source('getLagSequences.R')
source('rf_model.R')
source('getImportantLags.R')
source('getcoefs.R')

library(zoo)
library(reshape2)
library(gridExtra)
library(DALEX)
library(forecast)
library(cowplot)


# Define countries, Energy type and period to examine
country_chosen <- c("AT","CH","DE","ES","FI","FR","IT","NO","PT","RO","SI","SK")
energy_chosen <- "HRE"
climate_variables <- c("t2m", "tp", "sd")
iteration_length <- 200     # maximum examined period for optimise_lag.R 
high_production_period <- c(1:6)
LABEL <- "seasonal"

load(file.path(PATH_OUTPUT, 'ERA5_daily_2000_2017.rda')) # ERA5 daily climate data: t2m, tp, sd, e
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
lag_TP <- data.frame()
lag_T2m <- data.frame()
lag_SD <- data.frame()

for (cnt in country_chosen) {
  tmp <- df_sel %>% filter(Country == cnt)
  if (all(is.na(tmp$Generation))) {next}
  
  # Precipitation
  df1 <- tmp %>%  select(Date, Input = tp, Target = Generation)
  o1 <- getLagSequences(df1$Target, df1$Input, aggr_function = mean, iteration =iteration_length) 
  colnames(o1$cumulative_input) <- colnames(o1$cumulative_input) %>% paste0("TP_",.)
  tmp1 <- data.frame(Date=ts, Country=cnt) %>%  mutate_if(is.factor, as.character) %>%
    bind_cols(., o1$cumulative_input) # add lag by increments
  lag_TP <- bind_rows(lag_TP, tmp1)    # input to RF model
  
  # Temperature
  df2 <- tmp %>%  select(Date, Input = t2m, Target = Generation)
  o2 <- getLagSequences(df2$Target, df2$Input, aggr_function = mean, iteration =iteration_length/2) 
  colnames(o2$cumulative_input) <- colnames(o2$cumulative_input) %>% paste0("Tm_",.)
  tmp2 <- data.frame(Date=ts, Country=cnt) %>%  mutate_if(is.factor, as.character) %>%
    bind_cols(., o2$cumulative_input) 
  lag_T2m <- bind_rows(lag_T2m, tmp2) 
  
  # Snow depth
  df3 <- tmp %>%  select(Date, Input = sd, Target = Generation)
  o3 <- getLagSequences(df3$Target, df3$Input, aggr_function = mean, iteration =iteration_length) 
  colnames(o3$cumulative_input) <- colnames(o3$cumulative_input) %>% paste0("SD_",.)
  tmp3 <- data.frame(Date=ts, Country=cnt) %>%  mutate_if(is.factor, as.character) %>%
    bind_cols(., o3$cumulative_input) 
  lag_SD <- bind_rows(lag_SD, tmp3)   
  
}

country_display <- unique(lag_T2m$Country) %>% print()
setdiff(country_chosen, country_display)     # countries without generation
rm(df1,df2,df3,tmp,tmp1,tmp2,tmp3,o1,o2,o3)


### ========================= Random forest model ========================================================

## Function of full RF model ------------------
RF_full_model <- function (input, labs = "") {
  tmp <- input %>% na.omit()
  rf_OOB <- data.frame()
  ## RF preliminary round: bootstrapping --------------
  rf_pre <- rf_model(target = tmp %>% pull(Generation),
                     inputs = tmp %>% select_if(is.numeric) %>% select(-Generation),
                     ntrees = 500, node_size=10)
  vimp <- getImportantLags(model_explain = rf_pre$importance,
                           variable_1 = "TP", variable_2 = "Tm", variable_3 = "SD", range = 30)
  var_main <- c("Date", "Country", "Generation", vimp$lags_chosen) %>% as.character()
  
  ## RF main round -----------------------   
  tmp_main <- input[, which(colnames(input) %in% var_main)]  %>% na.omit()
  rf <- rf_model(target = tmp_main %>% pull(Generation),
                 inputs = tmp_main %>% select_if(is.numeric) %>% select(-Generation), 
                 ntrees = 500, node_size=1)
  
  # Out-of-bag coefficients 
  tmp3 <- rf$coefficients %>% mutate(Country = cnt) %>% mutate_if(is.factor, as.character)
  rf_OOB <- bind_rows(rf_OOB, tmp3)                  
  
  # Model explain and the most important variables
  rf_varimp = plot(rf$importance) + theme(legend.position="none")
  top_3_variables = rf$importance %>% filter(!(variable %in% c("_baseline_", "_full_model_"))) %>% top_n(3, dropout_loss) %>%
    arrange(-dropout_loss) %>%  pull(variable) %>% as.character()
  # partial dependence plots
  rf_1 = plot(single_variable(rf$explain, top_3_variables[1], type = 'pdp'))+ theme(legend.position="none")
  if (!is.na(top_3_variables[2])) {
    rf_2 = plot(single_variable(rf$explain, top_3_variables[2], type = 'pdp'))+ theme(legend.position="none")
  } else (rf_2 = NULL)
  if (!is.na(top_3_variables[3])) {
    rf_3 = plot(single_variable(rf$explain, top_3_variables[3], type = 'pdp'))+ theme(legend.position="none")
  } else (rf_3 = NULL)
  total_plot   = plot_grid(rf_varimp, rf_1, rf_2, rf_3, nrow = 1, labels = paste(cnt, labs), align = "v")
  
  full_model <- list(inputs = input, # sth useful here
                     model = rf,
                     OOB = rf_OOB,
                     plots = total_plot)
  return(full_model)
}
## End of function ##

## Prepare predictors: high production period  --------------------------

# fixed
full_df <- df_sel %>% filter(Country %in% country_display) %>% select(Date, Country, Generation) %>%
  left_join(., lag_TP, by = c("Date", "Country"))  %>% 
  left_join(., lag_T2m, by = c("Date", "Country")) %>%
  left_join(., lag_SD, by = c("Date", "Country")) %>%
  mutate(is.highproduction = ifelse(month(Date) %in% high_production_period, TRUE, FALSE))

# High production period vary between countries
highproduction <- list(AT=c(5:9), CH=c(6:12,1), DE=c(6:9), ES=c(1:6), FR=c(1:6),
                       IT=c(5:8), NO=c(11,12,1:3), PT=c(1:4), RO=c(3:7), SK=c(4:6))
full_df <- df_sel %>% filter(Country %in% country_display) %>% select(Date, Country, Generation) %>%
  left_join(., lag_TP, by = c("Date", "Country"))  %>% 
  left_join(., lag_T2m, by = c("Date", "Country")) %>%
  left_join(., lag_SD, by = c("Date", "Country"))
tmp2 <- data.frame()
for (cnt in country_display) {
  tmp <- full_df %>% filter(Country == cnt)
  tmp <- tmp %>% mutate(is.highproduction = ifelse(month(Date) %in% highproduction[[cnt]], TRUE, FALSE))
  tmp2 <- bind_rows(tmp2, tmp)
}
full_df <- tmp2

# Divide into seasons based on HRE productivity

# Prepare training and validating dataset
validate_df <- full_df %>% filter(between(as.Date(Date), as.Date("2017-01-01"), as.Date("2017-06-30")))
training_df <- anti_join(full_df, validate_df, by = c("Date", "Country"))

set.seed(123)
RF_model_list <- vector(mode = "list", length = length(country_display))
plot_list <- vector(mode = "list", length = length(country_display))
RF_nor_OOB <- data.frame()

for (cnt in country_display) {
  tmp <- training_df %>% filter(Country==cnt)
  message("RF model for ", cnt)
  
  # Normal (non seasonal) model
  RF_nor <- RF_full_model(tmp %>% select_if(is.numeric), labs = "normal")
  RF_nor_OOB <- bind_rows(RF_nor_OOB, RF_nor$OOB)
  
  # High production period
  tmp_high <- tmp %>% filter(is.highproduction == TRUE) %>% na.omit()
  RF_high <- RF_full_model(tmp_high %>% select_if(is.numeric), labs = "high")
  
  # Low production period
  tmp_low <- tmp %>% filter(is.highproduction == FALSE)
  RF_low <- RF_full_model(tmp_low %>% select_if(is.numeric), labs = "low")
  
  RF_model_list[[cnt]] <- list(normal = RF_nor$model, high = RF_high$model, low = RF_low$model)
  plot_list[[cnt]] <- list(normal = RF_nor$plots, high = RF_high$plots, low = RF_low$plots)
  
}
rm(tmp, tmp_high, tmp_low, RF_high, RF_low, RF_nor)

# Plot variable importance -----------
# for (cnt in country_display) {
#   plot_file <- paste(baseDir,"FIGURE/Seasonal/sd_Variable_importance_",cnt,".png",sep="")
#   png(plot_file, width=32, height=16, units="cm", res=300)
#   do.call(grid.arrange, c(plot_list[[cnt]][1:3], ncol = 1))
#   dev.off()
# }

## ==================== Model validation ==================================================================

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

# Cross-validation with seasonal model

RF_validation_df <- data.frame()
RF_validation_coef <- data.frame()
for (cnt in country_display) {
  tmp <- validate_df %>% filter(Country == cnt) %>% na.omit()
  
  tmp_high <- tmp %>% filter(is.highproduction == TRUE)
  if (!is.null(tmp_high)) {
    tmp_high <- tmp_high %>% mutate(Estimated = predict(RF_model_list[[cnt]][['high']][['model']], newdata = tmp_high %>% select_if(is.numeric))) %>%
      select(Date, Country, Observed = Generation, Estimated)
  }
  tmp_low <- tmp %>% filter(is.highproduction == FALSE)
  if (!is.null(tmp_high)) {
    tmp_low <- tmp_low %>% mutate(Estimated = predict(RF_model_list[[cnt]][['low']][['model']], newdata = tmp_low %>% select_if(is.numeric))) %>%
      select(Date, Country, Observed = Generation, Estimated)
  }
  tmp2 <- bind_rows(tmp_high, tmp_low) %>% arrange(Date)
  RF_validation_df <- bind_rows(RF_validation_df, tmp2)
  
  tmp3 <- getcoefs(tmp2$Observed, tmp2$Estimated) %>%
    mutate(Country = cnt) %>% mutate_if(is.factor, as.character)
  RF_validation_coef <- bind_rows(RF_validation_coef, tmp3)
}
rm(tmp, tmp_high,tmp_low,tmp2,tmp3)


## ====================== Some plots =============================================================================

# Plotting RF model's non seasonal
data.m <- melt(RF_normal_coef, id.vars='Country')
ggplot(data.m, aes(x=Country, y=value)) +
  geom_bar(fill="lightblue", stat = "identity", position="dodge") +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_text(aes(label = sprintf('%.2f', value)), position = position_dodge(width = 0.9), vjust = -1.0 ) +
  ggtitle(paste("Normal model's coefficients for", energy_chosen))

# Cross validation
data.m2 <- melt(RF_validation_coef, id.vars='Country')
ggplot(data.m2, aes(x=Country, y=value)) +
  geom_bar(fill="lightblue", stat = "identity", position="dodge") +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_text(aes(label = sprintf('%.2f', value)), position = position_dodge(width = 0.9), vjust = -1.0 ) +
  ggtitle(paste("Cross validation for", energy_chosen, LABEL))

## OOB
data.m3 <- melt(RF_nor_OOB, id.vars='Country')
ggplot(data.m3, aes(x=Country, y=value)) +
  geom_bar(fill="lightblue", stat = "identity", position="dodge") +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_text(aes(label = sprintf('%.2f', value)), position = position_dodge(width = 0.9), vjust = -1.0 ) +
  ggtitle(paste("OOB for", energy_chosen, "normal"))

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

