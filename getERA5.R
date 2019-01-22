# Read ERA5 data for climate variables: temperature at 2m (t2m), precipitation (tp), snow depth (sd), evaporation (e), etc.
# Original data in country average, hourly

rm(list=ls())
baseDir <- "D:/WEMC/R_code/LD_RCodes_20180914" # Change to your own directory
setwd(baseDir)
PATH_OUTPUT <- file.path(baseDir, 'OUTPUT')
PATH_TO_ERA5 <- file.path(baseDir, "DATA/ERA5")
ECEM_outputDir <- file.path(baseDir, "HISTORICAL")
library(zoo)

## Define variables
country_chosen <- c("AT","CH","DE","ES","FI","FR","IT","NO","PT","RO","SI","SK")
YEARS <- c(2000:2018) 

### ======= Climate data from ERA5 hourly data for each country ========================================

getERA5_hourly <- function (variable_code, PATHs) {
  ERA5_df <- data.frame()
  for (cnt in country_chosen) {
    tmp <- data.frame()
    print(paste(variable_code, "for", cnt))
    for (YEAR in YEARS) {
      # Observation for precipitation in 2000 start at 7:00:00
      if (YEAR == 2000 && variable_code == 'tp') {
        filename = paste0(PATHs, '/', cnt, '_ecmwf_era5_forecast_S200001010700_E200012312300_1hr_EU1_025d_0m_tp_noc_org.csv')
      } else {
        filename = paste0(PATHs, "/", cnt, '_ecmwf_era5_', ifelse(variable_code %in% c("tp","e"),'forecast','analysis'), 
                          '_S',YEAR, '01010000_E',YEAR,
                          ifelse(YEAR==2018,'0630', '1231'), '2300_1hr_EU1_025d_', # half-year data in 2018 at the moment, delete when having the complete data
                          ifelse(variable_code == "t2m", '2m', '0m'),
                          '_', variable_code, '_noc_org.csv')
      }
      if (file.exists(filename)) {
        d = read.table(file = filename, header = FALSE, sep = ",") 
        d$V1 <- as.POSIXct(d$V1, tz="UTC", format="%Y-%m-%d %H:%M:%OS") # UTC time
        d <- d %>% mutate(Country=cnt) %>% select(TS=V1, Country, V2)
        colnames(d)[3] <- variable_code
        tmp <- bind_rows(tmp,d)
      }
    }
    ERA5_df <- bind_rows(ERA5_df, tmp)
  }
  return(ERA5_df)
}

# Temperature (Kelvin --> Celcius)
PATH <- file.path(PATH_TO_ERA5, "t2m_country_averages")
ERA5_t2m_hourly <- getERA5_hourly('t2m', PATH)
ERA5_t2m_hourly$t2m <- ERA5_t2m_hourly$t2m - 273.15

# Precipitation in metre (m) --> millimetre (mm)
PATH <- file.path(PATH_TO_ERA5, "tp_country_averages")
ERA5_tp_hourly <- getERA5_hourly('tp', PATH)
ERA5_tp_hourly$tp <- ERA5_tp_hourly$tp*1000 

# Snow depth (metre of water equivalent)
PATH <- file.path(PATH_TO_ERA5, "sd_country_averages")
ERA5_sd_hourly <- getERA5_hourly('sd', PATH)

# Evaporation (metre of water equivalent)
PATH <- file.path(PATH_TO_ERA5, "e_country_averages")
ERA5_e_hourly <- getERA5_hourly('e', PATH)

# Data frame with all variables
ERA5_hourly_2000_2018 <- left_join(ERA5_t2m_hourly, ERA5_tp_hourly) %>% left_join(., ERA5_sd_hourly) %>% left_join(., ERA5_e_hourly)


### ================== Convert to daily data from ERA5 hourly =========================================================
# Note: daily temperature = average hourly data (Celcius); daily precipitation = sum of hourly data (mm)
getERA5_daily <- function (variable_code, input_hourly) {
  df_daily <- data.frame()
  aggregate_method <- ifelse (variable_code %in% c("tp", "e"), sum, mean)
  for (cnt in country_chosen) {
    tmp <- input_hourly %>% filter(Country==cnt) %>% mutate(Date=as.Date(TS,"%Y-%m-%d", tz="UTC")) %>%
      select(-Country, -TS)
    tmp2 <- aggregate(tmp[,1] ~ Date, tmp, aggregate_method) %>% mutate(Country=cnt) 
    colnames(tmp2)[2] <- variable_code
    df_daily <- bind_rows(df_daily, tmp2)
  }
  df_daily <- df_daily[,c(1,3,2)]
  return(df_daily)
}

# Temperature
ERA5_t2m_daily <- getERA5_daily('t2m', ERA5_t2m_hourly)
ERA5_tp_daily  <- getERA5_daily('tp', ERA5_tp_hourly)
ERA5_sd_daily  <- getERA5_daily('sd', ERA5_sd_hourly)
ERA5_e_daily   <- getERA5_daily('e', ERA5_e_hourly)

ERA5_daily_2000_2018 <- left_join(ERA5_t2m_daily, ERA5_tp_daily) %>% left_join(., ERA5_sd_daily)%>% left_join(., ERA5_e_daily)
# NA value for precipitation on the first day (2000-01-01) started at 7:00:00, not the whole day
ERA5_daily_2000_2018$tp[which(ERA5_daily_2000_2018$Date=="2000-01-01")]  <- NA

# Check the data
df <- ERA5_daily_2000_2018 %>% filter(between(as.Date(Date), as.Date("2017-01-01"),as.Date("2018-06-30")))
ggplot(data = df) + geom_line(aes(x=Date, y=tp)) + facet_wrap(~Country, scales = "free") + theme_grey()

save(ERA5_hourly_2000_2018, file = file.path(PATH_OUTPUT, 'ERA5_hourly_2000_2018.rda'))
save(ERA5_daily_2000_2018, file = file.path(PATH_OUTPUT, 'ERA5_daily_2000_2018.rda'))


#### ======================== Climate data ERA-INTERIM (from ECEM Demonstrator) ======================
# 2-metre temperature and precipitation 2000 - 2016 # paper (June 2017)

# Air temperature
ecem_file <- file.path(ECEM_outputDir, "H_ERAI_ECMW_T159_TA-_0002m_Euro_22W27N_45E72N_CTRY_IN_TIM_19790101_20161231_01d_NA-_nbc_org_NA_NA-.csv")
ERAInterim_t2m_daily <- read.csv(ecem_file)
ERAInterim_t2m_daily = ERAInterim_t2m_daily %>% gather(Country, t2m, -Year, -Month, -Day) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  select(Year, Month, Day, Date, Country, t2m)

# Precipitation
ecem_file <- file.path(ECEM_outputDir, "H_ERAI_ECMW_T159_TP-_0000m_Euro_22W27N_45E72N_CTRY_IN_TIM_19790101_20161231_01d_NA-_gbc_org_NA_NA-.csv")
ERAInterim_tp_daily <- read.csv(ecem_file)
ERAInterim_tp_daily = ERAInterim_tp_daily %>% gather(Country, tp, -Year, -Month, -Day) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  select(Year, Month, Day, Date, Country, tp)

ERAInterim_daily_1979_2016 <- left_join(ERAInterim_t2m_daily, ERAInterim_tp_daily) %>%
  select(Date, Country, t2m, tp)
ERAInterim_daily_1979_2016 <- ERAInterim_daily_1979_2016[order(ERAInterim_daily_1979_2016$Country),]
save(ERAInterim_daily_1979_2016, file = file.path(PATH_OUTPUT, 'ERAInterim_daily_1979_2016.rda'))
rm(ecem_file, ERAInterim_t2m_daily, ERAInterim_tp_daily)

### ======================= Compare ERA5 and ERA Interim =========================================
ID_START <- max(first(ERA5_daily_2000_2017$Date), first(ERAInterim_daily$Date)) %>% print()
ID_END  <- min(last(ERA5_daily_2000_2017$Date), last(ERAInterim_daily$Date)) %>% print()
df_ERAinterim <- ERAInterim_daily %>% filter(Country %in% country_chosen) %>%
  filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END))) %>% select(Date, Country, T2m, Precipitation)
colnames(df_ERAinterim)[c(3,4)] <- c("ERAIn_t2m", "ERAIn_tp")
df_ERA5 <- ERA5_daily_2000_2017 %>% select(Date, Country, t2m, tp)
colnames(df_ERA5)[c(3,4)] <- c("ERA5_t2m", "ERA5_tp")
df_ERA <- left_join(df_ERAinterim, df_ERA5)
df_ERA <- df_ERA %>% mutate(dif_t2m = ERA5_t2m - ERAIn_t2m, dif_tp = ERA5_tp - ERAIn_tp)
df <- df_ERA %>% filter(between(as.Date(Date), as.Date("2000-01-01"),as.Date("2001-12-31")))

df_monthly <- df
df_monthly$Month_Yr <- format(as.Date(df_monthly$Date), "%Y-%m")
Month_Yr <- unique(df_monthly$Month_Yr) %>% as.data.frame()
df_ERA_monthly <- data.frame()
for (cnt in country_chosen) {
  tmp <- df_monthly %>% filter(Country == cnt) %>% select(-Date, -Country)
  tmp2 <- aggregate(. ~ Month_Yr, data = tmp, mean) %>% mutate(Country = cnt)
  df_ERA_monthly <- bind_rows(df_ERA_monthly, tmp2)  
}
df_ERA_monthly$Month_Yr <- as.Date(df_ERA_monthly$Month_Yr, format = "%Y-%m")
df_ERA_monthly$Month_Yr <- as.factor(df_ERA_monthly$Month_Yr)

ggplot(data = df_ERA_monthly) + geom_line( aes(x=Month_Yr, y= ERAIn_t2m, colour = "red")) +
  geom_line(aes(x=Month_Yr, y=ERA5_t2m, colour = "steelblue")) +
  labs(title = "Monthly temperature ERA5 vs ERA Interim") +
  facet_wrap(~Country) + theme_grey()
ggplot(data=df_roll5) + geom_line(aes(x=Date, y=dif_t2m)) + facet_wrap(~Country) + theme_grey() +
  labs(title = "Different in temperature ERA5 - ERAInterim in Switzerland") + ylab("Precipitation difference")

# Difference
ID_START <- as.Date("2000-01-01")
ID_END <- as.Date("2016-12-31")

era <- ERA5_daily_2000_2017 %>% filter(Country %in% country_chosen) %>% select(Date, Country, t2m, tp) %>% 
  filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END)))
intr <- ERAInterim_daily_1979_2016 %>% filter(Country %in% country_chosen) %>% select(Date, Country, t2m, tp) %>% 
  filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END)))

plot_t2m <- data.frame(Date = intr$Date, Country = intr$Country, ERA5 = era$t2m, INTERIM = intr$t2m, Clim = "Temperature (C)")
plot_tp <- data.frame(Date = intr$Date, Country = intr$Country, ERA5 = era$tp, INTERIM = intr$tp, Clim = "Precipitation (mm)")
plot_data <- bind_rows(plot_t2m, plot_tp)
data.m <- melt(plot_data, id.vars = c("Date", "Country","Clim"))
colnames(data.m)[4] <- c("Dataset")

ggplot(data.m) + geom_boxplot(aes(x=Country, y=value, fill = Dataset)) +
  facet_wrap(~Clim, nrow = 1, scales = "free") + theme_grey(base_size = 16) + theme(axis.title.y=element_blank(), legend.position="bottom")

