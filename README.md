# WEMC-hydropower-model
Statistical model for hydropower using climate variables - within European Climatic Energy Mixes (ECEM) project
Contains random forest models for different purposes (starting with RF_) and supplementary functions (get*, rf*)
Make your own DATA and FIGURE directories

(A1) Main codes (start with this): RF_hydropower_all.R

This file contains the aggreed model for hydropower at country average level: use multiple lag time with option for on/off seasonality.
set the boolean variable < is.seasonal_model > for this option

It allows user to define: countries to examine, type of hydropower (HRO or HRE), climate variables in use (must be the same as the name code from ECMWF, e.g. t2m for temperature, tp for precipitation, sd for snow depth, etc.)
Other variables such as validating period, estimating period can also be specified before running the model (## =======)

Note that in this code, I use a general term "estimate" to refer both hindcast for reconstruction as well as predict for seasonal forecast

