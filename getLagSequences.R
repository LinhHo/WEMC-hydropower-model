# Dataframe of lag of climate variables with 5-day increments
# Linh Ho (29/10/2018)

getLagSequences <- function(input, aggr_function, iteration = 200) {

  # data frame of cumulative values with 5-day increments
  cumulative_seq = seq(5, iteration, by=5)              
  cumulative_df <- matrix(nrow = length(cumulative_seq), ncol = length(input))
  for (i in 1:length(cumulative_seq)) {
    cumulative_df[i,] <- zoo::rollapply(input, width = i*5, FUN = aggr_function, fill = NA, align = 'right')
  }
  cumulative_df <- as.data.frame(t(cumulative_df))
  colnames(cumulative_df) <- sprintf("lag%s",  cumulative_seq)

  return(
    list(orig_input = input,
         df_cumulative = cumulative_df
    )
  )
}