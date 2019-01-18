# WEMC-hydropower-model
Statistical model for hydropower using climate variables - within European Climatic Energy Mixes (ECEM) project\
Linh Ho (09/01/2019)

Random forest models for different purposes (starting with RF_) and supplementary functions (get*, rf*)\
Make your own DATA and FIGURE directories

## Models

### (A1) RF_hydropower_all.R : Main codes (start with this)

This file contains the aggreed model for hydropower at country average level: use multiple lag time with option for on/off seasonality.\
Set the boolean variable < is.seasonal_model > for this option

It allows user to define: countries to examine, type of hydropower (HRO or HRE), climate variables in use (must be the same as the name code from ECMWF, e.g. t2m for temperature, tp for precipitation, sd for snow depth, etc.)\
Other options such as validating period, estimating period can also be specified before running the model (## =======)

Note that in this code, I use a general term "estimate" to refer both hindcast for reconstruction as well as predict for seasonal forecast and climate projection

Main parts:

- Define necessary variables
- Calculate multiple lag for climate variables
- Prepare input for the random forest model
- Validate the model using an indepedent dataset of energy
- Estimate (hindcast/predict) the energy generation using the model trained from available dataset

The first three steps are compulsory while the 4th and 5th are indepedent and not necessary to run one before the other.

For example, the climate data ERA5 available from 2000-2017, the energy data ENTSO-E available from 2015, thus the common period is 2015-2017.\
To validate the model, the validate period is set in the year 2016, e.g. < validate_START is 2016-01-01 > and < validate_END is 2016-12-31 >. The model is trained with the remaining dataset, i.e. 2015 and 2017. Next, the climate data of 2016 will be the input of this model to produce the output - estimated generation for 2016. Finally, the output is then compared with the 2016 generation from ENTSOE data, calculate correlation, RMSE, etc. to produce the validation result.\
Note that these validation coefficietns would normally be lower than the out-of-bag estimate error.\
To reconstruct the energy generation data from 2000-2014, the model is trained with the whole common period 2015-2017, then with the climate input 2000-2014, this model will estimate the generation in this period. These output are the reconstruction for energy data in the period 2000-2014.

To make the codes cleaner, all the plots were moved to a separate file < RF_plots.R >.\
Find ## PLOT (N) - with N is the corresponding number to the RF_plots.R file.\
These plots are not required to run the subsequents lines of code.

### (A2) RF_plots.R

Find the corresponding number ## (N)  ## to produce the necessary plot(s).

Some plots need a little tweak to meet the user's needs, e.g. font size, text position.\
In particular, plots related to coefficients may need to change the variable name to produce the required plot: corr = (Pearson) correlation, RMSE = root-mean-square error, MAE = mean absolute error, nMAE = normalised mean absolute error

### (A3) RF_optimal_lag.R

The original model, rewritten from Matteo de Felice (2018) to keep the same format

This model uses input including daily climate variables as well as optimal lag for precipitation (and snow depth)\
The coeffients are from out-of-bag estimate of randomForest R package

## Supplementary functions

In order of appearance in RF_hydropower_all.R

### (B1) getENTSOE.R and getERA5.R

Reshape the data from ENTSO-E/ERA5 into proper shape to use in the model

### (B2) getLagSequences.R

Return dataframe of multiple lag time of the input variable, with 5-day increments.\
Aggregation function needs to be specified.\
Suggested options: mean for temperature, sum (cumulative) for precipitation\
Snow depth needs further discussion, perhaps sum or difference

Iteration length (the maximum length of lag time) is agreed as 200 days for precipitation and 100 days for temperature. However, the variable < iteration_length > is still kept rather than replaced by a fixed number, in case further tests needed.

### (B3) getHighProductionPeriod.R

Return a list of the high production period of energy generation for each country based on two criteria:

- The monthly value of that month is higher than the mid-range value (average of maximum and minimum value)
- There are at least TWO consecutive months in order to be called a high production period

There will be a warning if that country has more or less than ONE high production period.

Two plots of monthly average generation and the high production period of all examined countries are included.

### (B4) getImportantLags.R

Input is the model explain, aka the list of variables arranged by the drop-out loss values.\
Output is a short-listed important variables, selected by the following steps:

- Take the first 15 variables with the highest drop-out loss values
- In the order of decreasing drop-out loss, extract the climate variable name and the lag time, e.g. tp_lag35 --> "tp" and "35"
- Drop (not select) a variable if its lag time is within the range of +/- N days (default = 30) of ALL previous chosen variables with the same climate variable name
- Otherwise, add this new variable to the chosen list and continue with the next one

Taking 15 variables is to make sure each country has at least TWO important variables, which is a requirement for random forest model.

### (B5) getcoefs.R

Return calculated coefficients: Pearson correlation, RMSE, MAE, nMAE

### (B6) getOptimalLag.R

(courtesy of Matteo)\
This function is only used in RF_optimal_lag.R
It calculates the Spearman correlation between energy data (target) and aggregated N-day of climate variables (input) with the maximum length of 200 days.\
The N value produces the highest correlation is then returned as the optimal lag.

### (C1) rf_model.R

(courtesy of Matteo)\
The original random forest from R package, with more detailed returned results such as coefficients, model explain and variable important.

### (C2) rf_full_model.R

TWO round random forest model where:

- First round (preliminary): model with all lag time as the input, then use getImportantLags.R to select the most important lag sequences
- Second round (main): run a random forest model again but with input are only the above important lags. Save this model for further use

The out-of-bag error and important variable plots are also included.

This function contains other functions and must be called last. I'm not sure this is a good practice but it's neater to save this full model in a separate file.




