# Choose the most important lags for one variable

getImportantLags <- function (model_explain, n = 10,
                              variable_1 = "",
                              variable_2 = "",  # must be 2 characters, e.g. "TP", "Tm", "SD"
                              variable_3 = "",
                              variable_4 = "",
                              range = 30) {  
    # Choose 10 precipitation lags with highest dropout_loss
    vimp_basic <- model_explain %>% filter(variable %in% c( "_baseline_", "_full_model_"))
    vimp_n <- anti_join(model_explain, vimp_basic, by = "variable") %>% 
              top_n(n, dropout_loss) %>% arrange(desc(dropout_loss) )
  
    # Choose the precipitation lags which are not overlapped in the range of +/- 30 days
    tmp_range_1 <- c()
    tmp_range_2 <- c()
    tmp_range_3 <- c()
    tmp_range_4 <- c()
    for (j in 1:n) {
      o1 <- substr(vimp_n$variable[j],start = 1, stop = 2)
      if (o1 == variable_1) {                  # choose the right lags for variable 1
        o <- as.numeric(substring(vimp_n$variable[j],7))
          if (o %in% tmp_range_1) {
            vimp_n$is.chosen[j] <- FALSE
            next
            } else {
            vimp_n$is.chosen[j] <- TRUE 
            tmp_seq_1 <- seq(from = ifelse(o >= range, o-range, 0),
                           to = o+range, by = 5)
            tmp_range_1 <- unique(c(tmp_range_1, tmp_seq_1))
            }
        } else if (o1 == variable_2) {                 # choose the right lags for variable 2
              o <- as.numeric(substring(vimp_n$variable[j],7))
              if (o %in% tmp_range_2) {
                vimp_n$is.chosen[j] <- FALSE
                next
              } else {
                vimp_n$is.chosen[j] <- TRUE 
                tmp_seq_2 <- seq(from = ifelse(o>=range, o-range, 0),
                               to = o+range, by = 5)
                tmp_range_2 <- unique(c(tmp_range_2, tmp_seq_2))
              }
        } else if (o1 == variable_3) {                 # choose the right lags for variable 2
          o <- as.numeric(substring(vimp_n$variable[j],7))
          if (o %in% tmp_range_3) {
            vimp_n$is.chosen[j] <- FALSE
            next
          } else {
            vimp_n$is.chosen[j] <- TRUE 
            tmp_seq_3 <- seq(from = ifelse(o>=range, o-range, 0),
                             to = o+range, by = 5)
            tmp_range_3 <- unique(c(tmp_range_3, tmp_seq_3))
          }
        } else if (o1 == variable_4) {                 # choose the right lags for variable 2
          o <- as.numeric(substring(vimp_n$variable[j],7))
          if (o %in% tmp_range4) {
            vimp_n$is.chosen[j] <- FALSE
            next
          } else {
            vimp_n$is.chosen[j] <- TRUE 
            tmp_seq_4 <- seq(from = ifelse(o>=range, o-range, 0),
                             to = o+range, by = 5)
            tmp_range_4 <- unique(c(tmp_range_4, tmp_seq_4))
          }
        } else {
            vimp_n$is.chosen[j] <- FALSE
        }
    }
       
      
    lag_chosen <- vimp_n %>% filter(is.chosen == TRUE) %>%  pull(variable) %>% as.character()
    lag_value <-  as.numeric(substring(lag_chosen,7)) # not complete
    
  return(list(lags_chosen = lag_chosen,
              lags_value = lag_value))
}

