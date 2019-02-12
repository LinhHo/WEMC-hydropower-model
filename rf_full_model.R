### Function of full RF model with 2 rounds ###
# Linh Ho (09/01/2019)

RF_full_model <- function (input, labs = "") {
  tmp <- input %>% na.omit()
  rf_OOB <- data.frame()
  
  ## RF preliminary round: bootstrapping --------------
  rf_pre <- rf_model(target = tmp %>% pull(Generation),
                     inputs = tmp %>% select_if(is.numeric) %>% select(-Generation),
                     ntrees = 500, node_size=10)
  vimp <- getImportantLags(model_explain = rf_pre$importance, variable_list = climate_variables, range = 30)
  var_main <- c("Date", "Country", "Generation", vimp$lags_chosen) %>% as.character()
  
  ## RF main round ------------------------------------ 
  tmp_main <- input[, which(colnames(input) %in% var_main)]  %>% na.omit()
  rf <- rf_model(target = tmp_main %>% pull(Generation),
                 inputs = tmp_main %>% select_if(is.numeric) %>% select(-Generation), 
                 ntrees = 500, node_size=1)
  
  # Out-of-bag coefficients 
  tmp3 <- rf$coefficients %>% mutate(Country = cnt) %>% mutate_if(is.factor, as.character)
  rf_OOB <- bind_rows(rf_OOB, tmp3)                  
  
  # Model explain and the most important variables
  rf_varimp = plot(rf$importance) + theme(legend.position="none")
  top_3_variables = rf$importance %>% filter(!(variable %in% c("_baseline_", "_full_model_"))) %>% top_n(3, dropout_loss) %>%
    arrange(-dropout_loss) %>%  pull(variable) %>% as.character()
  
  # Partial dependence plots of the top 3 important variables
  rf_1 = plot(single_variable(rf$explain, top_3_variables[1], type = 'pdp'))+ theme(legend.position="none")
  if (!is.na(top_3_variables[2])) {
    rf_2 = plot(single_variable(rf$explain, top_3_variables[2], type = 'pdp'))+ theme(legend.position="none")
  } else (rf_2 = NULL)
  if (!is.na(top_3_variables[3])) {
    rf_3 = plot(single_variable(rf$explain, top_3_variables[3], type = 'pdp'))+ theme(legend.position="none")
  } else (rf_3 = NULL)
  total_plot   = plot_grid(rf_varimp, rf_1, rf_2, rf_3, nrow = 1, labels = paste(cnt, labs), align = "v")
  
  full_model <- list(inputs = input, # sth useful here !!
                     model = rf,
                     OOB = rf_OOB,
                     plots = total_plot)
  return(full_model)
}
