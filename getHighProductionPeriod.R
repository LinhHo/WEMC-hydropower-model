# Define the high production period of hydropower generation for each country
# based on the mid-range value of avarage monthly generation
# Choose only period with at least two months
# Input structure: dataframe(Date, Country, Generation)

getHighProductionPeriod <- function (input, labs = "") {

  df <- input %>% mutate(Month = month(input[['Date']]), Year = year(input[['Date']]))
  YEAR <- unique(df$Year)
  COUNTRY <- unique(df$Country)
  Threshold = data.frame(Country = COUNTRY, midvalue = NA)
  highproduction <- list()
  toplot_monthly <- data.frame()
  df_avg <- data.frame()
  
  for (cnt in COUNTRY) {
    tmp <- df %>% filter(Country == cnt)
    if (all(is.na(tmp$Generation))) {next}
    o <- data.frame()
    for (yr in YEAR) {
      tmp1 <- tmp %>% filter(Year == yr)
      tmp2 <- aggregate(Generation ~ Month, data = tmp1, mean) %>% mutate(Country = cnt, Year = yr)
      o <- bind_rows(o, tmp2)
    }
    o <- o %>% spread(Month, Generation) %>% gather(Month, Generation, -Country, -Year) 
    # Monthly average each year
    o$Month <- as.numeric(o$Month)
    toplot_monthly <- bind_rows(toplot_monthly, o)
    
    # Monthly average of all years
    avg <- aggregate(Generation ~ Month, data = tmp, mean) %>% mutate(Country = cnt)
    threshold <- (max(avg$Generation)+min(avg$Generation))/2
    Threshold$midvalue[which(Threshold$Country==cnt)] = threshold
    
    # Define whether one month is within the high production period
    aa <- avg %>% mutate(is.higher = as.integer(avg$Generation > threshold))
    a <- aa[c(12,1:12,1),] # Consider the annual cycle Dec-Jan
    bb <- data.frame(roll_diff = rollapply(a$is.higher, width = 3, FUN = mean, fill = NA, align = 'center')) %>% na.omit()
    avg <- bind_cols(aa, bb)
    df_avg <- bind_rows(df_avg, avg)
    highproduction[[cnt]] <- (avg$Month[which(avg$roll_diff >= 2/3)]) # at least two consecutive months with larger generation compared to the threshold
    
  }
  
  # Warning for country not having ONE single high production period
  tmp <- sapply(highproduction, diff)
  for (cnt in unique(df_avg$Country)) {
    if (length(tmp[[cnt]])==0) {
      message(paste("Warning:", cnt, "has no high production period"))
    } else if (length(which(tmp[[cnt]] != 1)) >1) { # a small bug here if the result is 1,2,3, 6,7,8 
      message(paste("Warning:", cnt, "has", length(which(tmp[[cnt]] != 1)), "high production periods"))
    } else (next)
  }
  
  # Plot monthly generation with mid-range value
  g1 <- ggplot() + geom_point(data = toplot_monthly %>% na.omit(), aes(x=Month, y=Generation)) +
                    geom_line(data = df_avg, aes(x=Month, y = Generation), colour = "red") +
                    geom_hline(data = Threshold %>% na.omit(), aes(yintercept = midvalue)) +
                    scale_x_continuous(breaks = seq(0,12, by =2)) +
                    labs(title = paste("Monthly generation", labs)) +
                    facet_wrap(~Country, scales = "free") +
                    theme_grey(base_size = 18)

  # Plot periods defined as high production
  o <- unlist(highproduction, use.names=FALSE)
  cnt_list <- c()
  for (cnt in COUNTRY) {
    tmp <- rep(cnt, length(highproduction[[cnt]]))
    cnt_list <- c(cnt_list, tmp)
  }
  toplot_highproduction <- data.frame(Country = cnt_list, start = o, end = o+1)
  g2 <- ggplot(toplot_highproduction, aes(ymin = start, ymax = end, x = Country)) + 
                    geom_linerange( size = 3) + scale_y_discrete(limits = c(0:12)) +  ylab("Month") + expand_limits(y=12) + 
    labs( title = paste( labs)) +
                    coord_flip() + theme_grey(base_size = 18)
  
  return (list(is.highproduction = highproduction,
               plot_monthly_generation = g1,
               plot_highproduction = g2))

}