# Random forest model - Linh Ho (8/10/2018)
# Seasonal model: with high and low production periods
# Target: Hydropower generation of hydropower reservoir (HRE)
# Predictors: lag time of precipitation (tp), air temperature (t2m)


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

select <- dplyr::select # avoid conflict with select function from other packages

source('getLagSequences.R')
source('rf_model.R')
source('getImportantLags.R')
source('getcoefs.R')
source('getHighProductionPeriod.R')
source('RF_full_model.R') # functions within function, must be called last

baseDir <- "D:/WEMC/R_code/LD_RCodes_20180914/WEMC-hydropower-model/" # Change to your own directory
setwd(baseDir)
PATH_DATA <- paste0(baseDir, "DATA")
PATH_FIGURE <- paste0(baseDir, "FIGURE")

# Define countries, Energy type, read input files
country_chosen <- c("AT","CH","DE","ES","FI","FR","IT","NO","PT","RO","SI","SK")
energy_chosen <- "HRE"
climate_variables <- c("t2m", "tp", "sd")
iteration_length <- 200
LABEL <- "HRE"

# Prepare data
load(file.path(PATH_DATA, 'ERA5_daily_2000_2018.rda')) # ERA5 daily climate data: t2m, tp, sd, e
load(file.path(PATH_DATA, 'Generation_all_2015_2018.rda')) # ENTSOE generation data

df_input  <-  ERA5_daily_2000_2018
df_target <-  ENTSOE_gen_new 

# Select common countries and period
ID_START <- max(first(df_input$Date), first(df_target$Date)) %>% print()
ID_END  <- min(last(df_input$Date), last(df_target$Date)) %>% print()

df_input_sel <- df_input %>% filter(Country %in% country_chosen) 
df_target_sel <- df_target %>% filter(Country %in% country_chosen) %>% select(Date, Country, Generation=energy_chosen)
df_sel <- left_join(df_input_sel %>% filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END))), 
                    df_target_sel %>% filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END))), 
                    by = c("Date", "Country"))

### ================== Get lag sequences for precipitation & temperature ====================================

# Lag time for climate variables - with 5-day increments
ts <- data.frame(Date=unique(df_sel$Date))
lag_df <- data.frame()

for (cnt in country_chosen) {
  tmp <- df_sel %>% filter(Country == cnt)
  if (all(is.na(tmp$Generation))) {next}
  tmp_lag <- data.frame(Date=ts, Country=cnt) %>%  mutate_if(is.factor, as.character)
  
  # use the same iteration_length for all climate variables, e.g. 200 days here ?!!
  for (clim in climate_variables) {
    df1 <- tmp %>%  select(Date, Input = clim, Target = Generation)
    o1 <- getLagSequences(df1$Target, df1$Input, aggr_function = mean, 
                          iteration = ifelse(clim == "t2m", iteration_length/2, iteration_length)) # only temperature use half of the iteration range
    colnames(o1$df_cumulative) <- colnames(o1$df_cumulative) %>% paste0(clim, "_",.)
    tmp_lag <- bind_cols(tmp_lag, o1$df_cumulative)
  }
  lag_df <- bind_rows(lag_df, tmp_lag)
}

# Countries in study
country_display <- unique(lag_df$Country) %>% print()
setdiff(country_chosen, country_display)     # countries without generation data
rm(df1,tmp_lag,tmp,o1)

##########################################################################################################
### ========================= Random forest model ========================================================


full_df <- df_sel %>% filter(Country %in% country_display) %>% select(Date, Country, Generation) %>%
  left_join(., lag_df, by = c("Date", "Country")) 

set.seed(123)
RF_nor_OOB <- data.frame()

for (cnt in country_display) {
  tmp <- full_df %>% filter(Country==cnt)
  message("RF model for ", cnt)
  
  # Normal (non seasonal) model
  RF_nor <- RF_full_model(tmp %>% select_if(is.numeric), labs = "normal")
  
  tmp2 <- RF_nor$OOB %>% mutate(Country = cnt)
  RF_nor_OOB <- bind_rows(RF_nor_OOB, tmp2)
}

HRE_multSD <- RF_nor_OOB %>% mutate(model = "multSD")
tmp <- bind_rows(HRO_multi, HRO_df) # tmp now have mult, multSD, opt
HRE_coef$model <- factor(HRE_coef$model, levels = c("opt", "optSD", "mult", "multSD"))
rm(tmp,tmp2,RF_nor)

# c("opt"="#F8766D", "optSD"="#B79F00", "mult"="#00BA38", "multSD"="#00BFC4")
# Comparison
data.m <- HRE_coef %>% select(Country, model, corr)
my.colour <- c(opt="#F8766D", optSD="#B79F00", mult="#00BA38", multSD="#00BFC4")
ggplot(data.m, aes(x = model, y = corr, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country, strip.position = "top", scales = "free_x", nrow =1) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(from = -0.4, to =1.0, by = 0.2)) +
  scale_fill_manual("Model", values = c("opt"="coral4", "optSD"="coral", "mult"="springgreen4", "multSD"="#00BFC4")) +
  ggtitle(paste(energy_chosen)) +
  theme_grey(base_size = 14) + ylab("Correlation") +
  theme(legend.position = "bottom", axis.title.x=element_blank(), axis.text.x = element_blank())

tostore_nmae <- tostore_nmae %>% mutate(Year = 2016)
data.nmae <- RF_normal_coef %>% select(Country, normal = nMAE) %>% left_join(.,RF_seasonal_coef, by = "Country") %>% 
  select(Country, normal, seasonal = nMAE) %>% mutate(Year=2017)
tmp <- bind_rows(tostore_nmae, data.nmae) 
tmp$Year <- as.factor(tmp$Year)
data.nmae.m <- melt(tmp, id.vars = c('Country', 'Year'))
colnames(data.nmae.m) <- c("Country", "Year", "model", "nMAE")
ggplot(data.nmae.m, aes(x = Year, y = nMAE, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country, strip.position = "top", scales = "free_x", nrow =1) +
  ggtitle(paste("Normalise Mean Absolute error", energy_chosen)) +
  theme(legend.position = "bottom",
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank()) + 
  theme_grey(base_size = 18) + xlab("Validation Year")

tostore_rmse <- tostore_rmse %>% mutate(Year = 2016)
data.rmse <- RF_normal_coef %>% select(Country, normal = RMSE) %>% left_join(.,RF_seasonal_coef, by = "Country") %>% 
  select(Country, normal, seasonal = RMSE) %>% mutate(Year=2017)
tmp <- bind_rows(tostore_rmse, data.rmse) 
tmp$Year <- as.factor(tmp$Year)
data.rmse.m <- melt(tmp, id.vars = c('Country', 'Year'))
colnames(data.rmse.m) <- c("Country", "Year", "model", "RMSE")
ggplot(data.rmse.m, aes(x = Year, y = RMSE, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country, strip.position = "top", scales = "free_x", nrow =1) +
  ggtitle(paste("Root-mean-square error", energy_chosen)) +
  theme(legend.position = "bottom",
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank()) + 
  theme_grey(base_size = 17) + xlab("Validation Year")



