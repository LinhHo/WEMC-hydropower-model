# Random forest with optimal lag - Linh Ho (10/08/2018) adapted from Matteo de Felice
# Target: Generation of hydropower HRO and HRE
# Predictors: daily air temperature (t2m), daily and optimal lag of precipitation (tp), +/- snow depth (sd)

rm(list = ls())
library(zoo)
library(reshape2)
library(gridExtra)
library(DALEX)
library(forecast)
library(cowplot)
library(tidyverse)

baseDir <- "D:/WEMC/R_code/LD_RCodes_20180914/WEMC-hydropower-model/" # Change to your own directory
setwd(baseDir)
PATH_DATA <- paste0(baseDir, "DATA")
PATH_FIGURE <- paste0(baseDir, "FIGURE")

source('rf_model.R')
source('getOptimalLag.R')


# Define countries, Energy type and period to examine
country_chosen <- c("AT","CH","DE","ES","FI","FR","IT","NO","PT","RO","SI","SK")
energy_chosen <- "HRE"
iteration_length <- 200     # maximum examined period for optimising lag
LABEL <- "ERA5"

# Prepare data
# load(file.path(PATH_DATA, 'ERAInterim_daily_1979_2016.rda')) # ERA INTERIM data
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
  
# Lag time for precipitation - limited in 200 days
ts <- data.frame(Date=unique(df_sel$Date))
TP_df <- data.frame()
SD_df <- data.frame()
plot_corr_tp <- data.frame()
plot_max_tp <- data.frame(Country = country_chosen, optimal_lag = NA)
plot_corr_sd <- data.frame()
plot_max_sd <- data.frame(Country = country_chosen, optimal_lag = NA)

for (cnt in country_chosen) {
  # Cumulative precipitation
  df <- df_sel %>% filter(Country == cnt) %>% 
    select(Date, Input = tp, Target = Generation)
  
  o <- getOptimalLag(df$Input, df$Target, aggr_function = sum) 
  df_tp <- data.frame(Date = ts)
  
  if (is_empty(o$chosen_lag)) {
    next
  } else {
    tmp <- data.frame(Days = c(1:iteration_length), corr_lag = o$corr_seq) %>% mutate(Country = cnt)
    plot_corr_tp <- bind_rows(plot_corr_tp, tmp) # to plot the optimised correlation
    plot_max_tp$optimal_lag[which(plot_max_tp$Country==cnt)] <- o$chosen_lag
    df_tp <- df_tp %>% mutate(TP_lag = o$optimised_input) %>% mutate(Country = cnt)
    }
  print(paste(cnt, o$chosen_lag))
  TP_df <- bind_rows(TP_df, df_tp)
  
  # Average snow depth
  df <- df_sel %>% filter(Country == cnt) %>%
    select(Date, Input = sd, Target = Generation)

  o <- getOptimalLag(df$Input, df$Target, aggr_function = mean)
  df_sd <- data.frame(Date = ts)

  if (is_empty(o$chosen_lag)) {
    next
  } else {
    tmp <- data.frame(Days = c(1:iteration_length), corr_lag = o$corr_seq) %>% mutate(Country = cnt)
    plot_corr_sd <- bind_rows(plot_corr_sd, tmp) # to plot the optimised correlation
    plot_max_sd$optimal_lag[which(plot_max_sd$Country==cnt)] <- o$chosen_lag
    df_sd <- df_sd %>% mutate(SD_lag = o$optimised_input) %>% mutate(Country = cnt)
    }
  print(paste(cnt, o$chosen_lag))
  SD_df <- bind_rows(SD_df, df_sd)
}

# Get names of non-NA countries - find a neater way!
country_display <- unique(TP_df$Country) %>% print()
setdiff(country_chosen, country_display)     # countries cannot calculate optimal lag

rm(df_tp,o,df,tmp)
plot_corr <- plot_corr %>% mutate(group = "ERA5")
plot_max <- plot_max %>% mutate(group = "ERA5")

PLOT_CORR <- bind_rows(PLOT_CORR, plot_corr)
PLOT_MAX <- bind_rows(PLOT_MAX, plot_max)

ggplot(PLOT_CORR, aes(x = Days, y = corr_lag, group=group)) +
  geom_point(alpha = 0.3, aes(colour=group)) +
  geom_text(data = PLOT_MAX %>% filter(group=="ERA5"), aes(x=-Inf,y=Inf, label= paste("ERA5", optimal_lag), colour = group, 
                                hjust = -0.01, vjust = 14)) +
  geom_text(data = PLOT_MAX %>% filter(group=="INTERIM"), aes(x=-Inf,y=Inf, label= paste("INTR", optimal_lag), colour = group, 
                                 hjust = -0.01, vjust = 16)) +
  facet_wrap(~Country, nrow = 3) +
  ggtitle(paste(energy_chosen)) +
  theme_grey(base_size = 14) + xlab("Cumulative days") + ylab("Correlation")


### ========================= Random forest model ========================================================

# Prepare predictors
full_df <- df_sel %>% filter(Country %in% country_display) %>% select(Date, Country, t2m, tp, sd, Generation) %>%
  left_join(., TP_df, by = c("Date", "Country")) %>% left_join(.,SD_df, by = c("Date", "Country"))
# %>%   left_join(., SD_df, by = c("Date", "Country"))

# Use OOB error, no validation period

set.seed(123)
RF_OOB <- data.frame()

for (cnt in country_display) {
  tmp <- full_df %>% filter(Country==cnt) %>% na.omit()
  message("RF model for ", cnt)
  
  RF <- rf_model(inputs = tmp %>% select_if(is.numeric) %>% select(-Generation), 
                     target = tmp %>% pull(Generation))
  tmp <- RF$coefficients %>% mutate(Country = cnt)
  RF_OOB <- bind_rows(RF_OOB, tmp)
}
is.na(RF_OOB) <- do.call(cbind,lapply(RF_OOB, is.infinite)) # assign NA to Inf value(s)

HRE_optSD <- RF_OOB %>% mutate(model = "OptSD")
HRE_OOB <- bind_rows(HRE_opt, HRE_optSD)

## ====================== Some plots =============================================================================

# Plotting RF model's Out-of-bag coefficients
data.m <- melt(RF_OOB, id.vars='Country')
ggplot(data.m, aes(x=Country, y=value)) +
  geom_bar(fill="lightblue", stat = "identity", position="dodge") +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_text(aes(label = sprintf('%.2f', value)), 
            position = position_dodge(width = 0.9),
            vjust = -1.0 ) +
  ggtitle(paste("OOB coefficients for", energy_chosen, LABEL))

# Compare Coefficients of Optimal lag model between ERA INTERIM and ERA5
data.m2 <- melt(HRE_OOB, id.vars = c('Country', 'model'))
ggplot(data.m2, aes(x = Country, y = value, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable, strip.position = "top", scales = "free", nrow =2) +
  ggtitle(paste(energy_chosen)) +
  theme(legend.position = "bottom",
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank()) + 
  theme_grey(base_size = 18) + xlab("Country")

