# Choose the most important lags with the range of 30 days
# From preliminary model, to give the input of the main Random Forest model
# Linh Ho (09/01/2019)

getImportantLags <- function (model_explain, n = 15, # consider the top n variables
                              variable_list, range = 30) {

    # Choose 15 variables with highest dropout_loss value
    vimp_basic <- model_explain %>% filter(variable %in% c( "_baseline_", "_full_model_"))
    vimp_n <- anti_join(model_explain, vimp_basic, by = "variable") %>% 
              top_n(n, dropout_loss) %>% arrange(desc(dropout_loss) ) 
  
    # Choose the lags which are not overlapped in the range of +/- 30 days
    tmp_range <- vector(mode = "list", length = length(variable_list))
    for (i in 1:n) {
      o1 <- sub("\\_.*", "", vimp_n$variable[i]) # extract variable's name
      
      for (j in 1:length(variable_list)) {
        
        if (o1 == variable_list[j]) {
          o <- as.numeric(substring(vimp_n$variable[i], nchar(variable_list[j])+5)) # extract lag time
          if (o %in% tmp_range[[j]]) {
            vimp_n$is.chosen[i] <- FALSE # NOT chosen if within the range of previous lags
            next
          } else {
            vimp_n$is.chosen[i] <- TRUE
            tmp_seq <- seq(from = ifelse(o >= range, o-range, 0), to = o+range, by = 5) # range of lag value +/- 30 days
            tmp_range[[j]] <- unique(c(tmp_range[[j]], tmp_seq))
          }
        }
      }
    }
      
    lag_chosen <- vimp_n %>% filter(is.chosen == TRUE) %>%  pull(variable) %>% as.character()

  return(list(lags_chosen = lag_chosen))
}

