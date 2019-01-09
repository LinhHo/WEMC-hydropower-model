analyse_gen_data <- function(df, size_roll_mean = 90) {
  library(tidyverse)
  
  # Check data
  stopifnot(c('Datetime', 'Value') %in% names(df))
  # Extract variables (in case)
  t = df$Datetime
  y = df$Value
  # P1: time series plot with two smoothers
  p1 = ggplot(df %>% ungroup(), aes(x = Datetime, y = Value)) + 
    geom_line(size = 0.5, color = '#377eb8') + 
    theme_minimal() + geom_smooth(method = 'lm', color = '#4daf4a') +
    geom_line(aes(y = zoo::rollmean(Value, size_roll_mean, na.pad=TRUE, align = 'center')),color = '#e41a1c')
  p2 = autoplot(Acf(y, 200, plot = FALSE), conf.int.fill = '#0000FF', alpha = 0.6) + theme_minimal()
  return(list(p1, p2))
}