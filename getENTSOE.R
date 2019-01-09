### -------- Generation hydropower data from ENTSO-E ------------------------------------------------------

# load(file.path(PATH_OUTPUT, 'ENTSOE_CFR_daily_NGCpaper_2015_2017.rda'))
load(file.path(PATH_OUTPUT, 'Generation_all_2015_2018_test.rda')) # Generation data

ENTSOE_sel <- ENTSOE_gen_new %>% filter(between(as.Date(Date),ID_START, ID_END)) %>%
  select(Date, Country, Generation=energy_chosen)

# load(file.path(PATH_OUTPUT, 'ENTSOE_CFR_daily_constNGC_2015_2017.rda')) # CFR with constant value of NGC
# 
# ENTSOE_sel <- data.frame()
# for (cnt in country_chosen) {
# 
#     tmp <- ENTSOE_CFR %>% filter(Country == cnt) %>%
#                   filter(between(as.Date(Date), as.Date(ID_START),as.Date(ID_END)))%>%
#                   mutate(Country=cnt) %>%
#                   select(Date, Country, type_chosen)
#     ENTSOE_sel <- bind_rows(ENTSOE_sel, tmp)
# }

## Get Generation data hourly from ENTSOE for renewable energy ------

# load(file.path(PATH_OUTPUT, 'ENTSOE_generation_hourly_2015_2018.rda'))
# ENTSOE_gen_renewable_hourly <- ENTSOE_generation_hourly %>%
#   mutate(EnergyType=ProductionType ) %>%
#   mutate(TS=as.POSIXct(paste(Date,Time), tz="", format="%Y-%m-%d %H:%M:%OS")) %>%
#   select(-ProductionType, -Pumping, -Year, -Month, -Day) %>%  
#   spread(EnergyType, Generation) %>%
#   select(TS, Country,
#          WON='Wind Onshore ',
#          WOF='Wind Offshore ',
#          HRE='Hydro Water Reservoir ',
#          HRO='Hydro Run-of-river and poundage ',
#          SOL='Solar ') %>%
#   gather(key='EnergyType', value=Generation, -TS, -Country)
# print(unique(ENTSOE_gen$Country)) # 31 countries
# save(ENTSOE_gen_renewable_hourly, file = file.path(PATH_OUTPUT, 'ENTSOE_generation_renewable_hourly_2015_2018.rda'))
load(file.path(PATH_OUTPUT, 'ENTSOE_generation_renewable_hourly_2015_2018.rda')) # my file shortcut

ENTSOE_gen_hourly <- ENTSOE_gen_renewable_hourly %>% filter(Country %in% country_chosen, EnergyType==energy_chosen)


