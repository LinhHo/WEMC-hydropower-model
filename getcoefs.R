# Get correlation coefficients

getcoefs <- function (Observed, Estimated) {
  RF_cor  = cor(Estimated, Observed)
  RF_rmse = sqrt(mean((Estimated - Observed)^2))  # root mean square error
  RF_mae  = mean(abs(Estimated - Observed)) # mean absolute error
  RF_nmae  = 100 * mean(abs((Estimated - Observed) / Observed)) # normalised mean absolute error
  tmp <- data.frame(corr=RF_cor, nMAE=RF_nmae, MAE=RF_mae, RMSE=RF_rmse) 
  
  return(coefs = tmp)
}