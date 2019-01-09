# All necessary plots from the hydropower model
# Produce figures stored in the baseDir/FIGURE directory

## Examine the CLIMATE data - daily temperature and precipitation by country

ggplot(df_sel, aes(x=Date)) + 
  geom_line(aes(y = t2m), colour = "red") + labs(title = "Daily temperature and precipitation", y = "") +
  geom_line(aes(y = tp), colour = "steelblue") +
  facet_wrap(~Country, scales = "free") + theme_grey(base_size = 18)

## Curves of CORRELATION between lag time for climate variable and hydropower generation

ggplot(lag_TP$series, aes(x = Cumulative_days, y = Correlation)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Country, nrow = 3, scales = "free") +
  geom_text(data = lag_TP$max, aes(x=-Inf,y=Inf, label= paste("Optimal lag =", lag_chosen), hjust = -0.3, vjust = 14))  +
  ggtitle(paste("Optimal cumulative precipitation for", energy_chosen, "ERA Interim")) +
  theme_grey(base_size = 18)

## High production period vary between countries

## Monthly generation data
list_highproduction$plot_monthly_generation
## High production periods defined for all countries
list_highproduction$plot_highproduction

##############################################################################################################
# RANDOM FOREST MODEL

## Plot variable importance for all countries -----------
for (cnt in country_display) {
  plot_file <- paste(baseDir,"FIGURE/HRO_Variable_importance_",cnt,".png",sep="")
  png(plot_file, width=32, height=16, units="cm", res=300)
  do.call(grid.arrange, c(plot_list[[cnt]][1:3], ncol = 1))
  dev.off()
}
