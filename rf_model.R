#' Create and calibrate a model based on Random Forests. 
#' Work based on ECEM 
#' @author Matteo De Felice
#' @param target Vector of length t target of the modelling
#' @param inputs Data frame with t rows of all the k inputs used in the model
#' @return A list containing the trained model and the cross-validated output
rf_model <- function(LABEL = '', 
                     target, 
                     inputs, 
                     ntrees = 500, 
                     node_size = 1) {
  library(tidyverse)
  library(randomForest)
  
  # Input checks
  stopifnot(nrow(inputs) == length(target))
  # Create merged tibble
  all_data = bind_cols(list(target = target), inputs)
  # Train model
  f_rf = randomForest::randomForest(target ~ ., data = all_data, ntree = ntrees, nodesize = node_size)
  
  ## Model coefficients ------
  RF_cor  = cor(f_rf$predicted, f_rf$y)
  RF_rmse = sqrt(mean((f_rf$predicted - f_rf$y)^2))  # root mean square error
  RF_mae  = mean(abs(f_rf$predicted - f_rf$y)) # mean absolute error
  RF_nmae  = 100 * mean(abs((f_rf$predicted - f_rf$y) / f_rf$y)) # normalised mean absolute error
  RF_results <- data.frame(corr=RF_cor, nMAE=RF_nmae, MAE=RF_mae, RMSE=RF_rmse) 
  
  
  ## Model explain and variable importance -----
  ex_rf = explain(f_rf, data = all_data[,-1], y = target)
  vimp = variable_importance(ex_rf) 
  
  rf_model =list(model = f_rf,
                 data  = all_data,
                 explain = ex_rf,
                 importance = vimp,
                 coefficients = RF_results,
                 predictors = colnames(inputs))
  
  return(rf_model)
}