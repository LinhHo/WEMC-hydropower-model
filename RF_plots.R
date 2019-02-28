# All necessary plots from the hydropower model
# Produce figures stored in the baseDir/FIGURE directory
# Numbered according to the main code RF_hydropower_all.R

## [1] ## Examine the CLIMATE data - daily temperature and precipitation by country
## It's not a good practice to plot two different variables with different unit in a same plot,
## but it's just easier to see and compare, so bear with me

ggplot(df_input_sel, aes(x=Date)) + 
  geom_line(aes(y = t2m), colour = "red") + labs(title = "Daily temperature and precipitation", y = "") +
  geom_line(aes(y = tp), colour = "steelblue") +
  facet_wrap(~Country, scales = "free") + theme_grey(base_size = 18)


## [2]  ## High production period vary between countries

## Monthly generation data
list_highproduction$plot_monthly_generation
## High production periods defined for all countries
list_highproduction$plot_highproduction

##############################################################################################################
### ======  RANDOM FOREST MODEL  ===============================

## [3] ## Plot variable importance for all countries -----------
for (cnt in country_display) {
  plot_file <- paste(baseDir,"FIGURE/HRE_Variable_importance_",cnt,".png",sep="")
  png(plot_file, width=32, height=16, units="cm", res=300)
  do.call(grid.arrange, c(plot_list[[cnt]][1:3], ncol = 1))
  dev.off()
}

## Section (1) - Model validation

## [4] ## Comparison between normal 2-step and seasonal models
a <- RF_normal_coef %>% mutate(model = "2-step", valid = 2017)
b <- RF_seasonal_coef %>% mutate(model = "seasonal", valid = 2017)
HRO_coef <- bind_rows(a,b)
df <- bind_rows(df, HRO_coef) # store and rerun if has more than ONE year
is.na(df) <- do.call(cbind,lapply(df, is.infinite)) # assign NA to Inf value(s)
df$valid <- as.factor(df$valid)

tmp <- df %>% select(Country, model, valid, corr)  # change to corr, nMAE, MAE or RMSE
ggplot(tmp, aes(x = valid, y = corr, fill = model)) + # similarly, change y
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country, strip.position = "top", scales = "free_x", nrow =1) +
  # scale_y_continuous(limits = c(-0.1, 1.0), breaks = seq(from = -0.4, to =1.0, by = 0.2)) + # this line is only for corr
  ggtitle(paste(energy_chosen)) +
  ylab("Root-mean-square error") + xlab("Validation Year") + # change ylab accordingly
  theme_grey(base_size = 14) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))


## Section (2) - Model hindcast/ prediction

## [5] ##
# Time series of generation data estimated & observed

df_target_sel <- df_target_sel %>% mutate(Data = "observed")
full_df <- bind_rows(RF_output, df_target_sel)
full_df$Data <- factor(full_df$Data, levels = c("estimated", "observed"))

# Convert to GW !!
# vv[,-1] <- vv[,-1]*1000


ggplot(full_df) + geom_line(aes(x=Date, y=Generation, colour = Data)) + 
  facet_wrap(~Country, nrow = 4, scales = "free") + theme_grey(base_size = 14) +
  ggtitle(energy_chosen) +
  ylab("Generation (GW)") +
  theme(legend.position = "bottom")


## [6] ## COMPARE GENERATION RECONSTRUCTION vs RTE

# for France 2013-2014

RF_reconst <- Generation_hindcast_nonseasonal_2000_2014 %>% filter(Country == "FR") %>%
  filter(between(as.Date(Date), as.Date("2013-01-01"), as.Date("2014-12-31"))) 
tmp_reconst <- RF_reconst %>% select(-Country, -Data) %>%
  gather(key = Type, value = Reconstruction, -Date)
tmp_reconst$Reconstruction <- round(tmp_reconst$Reconstruction/1000, digits = 3)  # convert MW --> GW
tmp_RTE <- RTE_daily %>% gather(key = Type, value = RTE, -Date)
tmp_RTE$RTE <- round(tmp_RTE$RTE/1000, digits = 3)
df <- left_join(tmp_RTE, tmp_reconst) %>% na.omit()
df <- df %>% mutate(Month = month(Date))

# Get correlation coefficients

a <- getcoefs(df$RTE, df$Reconstruction) %>% print()
df_HRO <- df %>% filter(Type=="HRO")
aa <- getcoefs(df_HRO$RTE, df_HRO$Reconstruction) %>% mutate(Type = "HRO")
df_HRE <- df %>% filter(Type=="HRE")
aaa <- getcoefs(df_HRE$RTE, df_HRE$Reconstruction) %>% mutate(Type = "HRE")
df_coef <- bind_rows(aa, aaa) %>% print()


ggplot() +
  geom_point(data=df, aes(x=RTE, y=Reconstruction, colour = Month)) +
  scale_colour_gradientn(colours = c("blue","white","red","white","blue"), limits = c(1,12)) +
  geom_text(data = df_coef, aes(x=-Inf,y=Inf, label= paste("corr =", sprintf('%.2f', corr)),
                                hjust = -0.1, vjust = 4)) +
  geom_text(data = df_coef, aes(x=-Inf,y=Inf, label= paste("nMAE =", round(nMAE, digits = 2)),
                                hjust = -0.1, vjust = 6)) +
  geom_text(data = df_coef, aes(x=-Inf,y=Inf, label= paste("RMSE =", round(RMSE, digits = 2)),
                                hjust = -0.1, vjust = 8)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~Type, scales = "free") +
  labs(title = "French Hydropower generation (GW) 2013-2014") +
  theme_grey(base_size = 14)

df1 <- df %>% gather(key = Data, value = Generation, -Date, -Type)
ggplot(data = df1) +
  geom_line(aes(x=Date, y=Generation, colour = Data)) +
  facet_wrap(~Type, scales = "free") + 
  ylab("Generation (GW)") + labs(title="Timeseries RTE - Reconstructed") +
  theme_grey(base_size = 14) +
  theme(legend.position = "bottom") 


