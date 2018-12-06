# Linh adapt from Matteo's code on 29/10/2018
# Choose optimal lag as the greatest positive correlation value instead of absolute value
# Add series of cumulative precipitation with 5-day increments

getLagSequences <- function(target, input, aggr_function = mean, iteration = 200) {
  library(zoo)
  stopifnot(length(input) == length(target))

  # MAX LAG based on CORRS
  iter_roll <- function(x) {
    roll_input = zoo::rollapply(input, width = x, FUN = aggr_function, fill = NA, align = 'right')
    return(cor(target, roll_input, method = 'spearman', use = 'na.or.complete'))
  }
  
  corr_lag = pbapply::pbsapply(seq_len(iteration), iter_roll)
  max_lag = which.max(corr_lag)

  # data frame of cumulative values with 5-day increments
  cumulative_seq = seq(5, iteration, by=5)              
  cumulative_inputs <- matrix(nrow = length(cumulative_seq), ncol = length(input))
  for (i in 1:length(cumulative_seq)) {
    cumulative_inputs[i,] <- zoo::rollapply(input, width = i*5, FUN = aggr_function, fill = NA, align = 'right')
  }
  cumulative_inputs <- as.data.frame(t(cumulative_inputs))
  colnames(cumulative_inputs) <- sprintf("lag%s",  cumulative_seq)
  corr <- function (lag_series) {
    cor(target, lag_series, method = 'spearman', use = 'na.or.complete')
  }
  cumulative_cors <- sapply(cumulative_inputs, corr)    # correlation between generation and precipitation lag

  return(
    list(
      orig_input = input,
      optimised_input  = zoo::rollapply(input, width = max_lag, FUN = aggr_function, fill = NA, align = 'right'),
      chosen_lag = max_lag, 
      corr_lags = corr_lag,
      cumulative_input = cumulative_inputs,
      cumulative_corr = cumulative_cors
    )
  )
}