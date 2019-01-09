# Get optimal lag based on optimising the correlation
# Adapt from Matteo de Felice (8/10/2018)

getOptimalLag <- function(input, target, aggr_function = mean, iteration = 200) {
  stopifnot(length(input) == length(target))
  
  # MAX LAG based on CORRS
  iter_roll <- function(x) {
    roll_input = zoo::rollapply(input, width = x, FUN = aggr_function, fill = NA, align = 'right')
    return(cor(target, roll_input, method = 'spearman', use = 'na.or.complete'))
  }
  
  corr_lag = pbapply::pbsapply(seq_len(iteration), iter_roll)
  max_lag = which.max(corr_lag) # not consider the absolute value
  
  return(list(chosen_lag = max_lag, 
              corr_seq = corr_lag,
              optimised_input  = zoo::rollapply(input, width = max_lag, FUN = aggr_function, fill = NA, align = 'right')))
}