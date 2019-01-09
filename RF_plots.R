# All necessary plots from the hydropower model
# Produce figures stored in the baseDir/FIGURE directory
# Numbered according to the main code RF_hydropower_all.R

## (1) ## Examine the CLIMATE data - daily temperature and precipitation by country

ggplot(df_sel, aes(x=Date)) + 
  geom_line(aes(y = t2m), colour = "red") + labs(title = "Daily temperature and precipitation", y = "") +
  geom_line(aes(y = tp), colour = "steelblue") +
  facet_wrap(~Country, scales = "free") + theme_grey(base_size = 18)

## (2) ## Curves of CORRELATION between lag time for climate variable and hydropower generation

ggplot(lag_TP$series, aes(x = Cumulative_days, y = Correlation)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Country, nrow = 3, scales = "free") +
  geom_text(data = lag_TP$max, aes(x=-Inf,y=Inf, label= paste("Optimal lag =", lag_chosen), hjust = -0.3, vjust = 14))  +
  ggtitle(paste("Optimal cumulative precipitation for", energy_chosen, "ERA Interim")) +
  theme_grey(base_size = 18)

## (3)  ## High production period vary between countries

## Monthly generation data
list_highproduction$plot_monthly_generation
## High production periods defined for all countries
list_highproduction$plot_highproduction

##############################################################################################################
### ======  RANDOM FOREST MODEL  ===============================

## (4) ## Plot variable importance for all countries -----------
for (cnt in country_display) {
  plot_file <- paste(baseDir,"FIGURE/HRO_Variable_importance_",cnt,".png",sep="")
  png(plot_file, width=32, height=16, units="cm", res=300)
  do.call(grid.arrange, c(plot_list[[cnt]][1:3], ncol = 1))
  dev.off()
}

## Section (1) - Model validation

## (5) ## Comparison between normal 2-round and seasonal models
a <- RF_normal_coef %>% mutate(model = "2-round", valid = 2017)
b <- RF_seasonal_coef %>% mutate(model = "seasonal", valid = 2017)
HRO_coef <- bind_rows(a,b)
df <- bind_rows(df, HRE_coef)
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

## (6) ##
# Time series of generation data estimated & observed

df_target_sel <- df_target_sel %>% mutate(Data = "observed")
full_df <- bind_rows(RF_output, df_target_sel)
full_df$Data <- factor(full_df$Data, levels = c("estimated", "observed"))

ggplot(full_df) + geom_line(aes(x=Date, y=Generation, colour = Data)) + 
  facet_wrap(~Country, scales = "free") + theme_grey(base_size = 14) +
  ggtitle(energy_chosen) +
  theme(legend.position = "bottom")


## (7) ##

# Compare with RTE data hydropower generation for France (to be continued)
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




