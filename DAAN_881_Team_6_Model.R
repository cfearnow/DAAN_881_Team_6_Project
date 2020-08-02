library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(data.table)
library(stats)
library(lmtest)

analysis_norm <- fread('C:/Users/chris/OneDrive/Documents/DAAN_881_Team_6_Project/analysis_set_normal.csv')
analysis <- fread('C:/Users/chris/OneDrive/Documents/DAAN_881_Team_6_Project/analysis_set_non-normal.csv')

analysis$wm_yr_wk <- as.factor(analysis$wm_yr_wk)
analysis$cat_id <- as.factor(analysis$cat_id)

weekly_norm <- analysis[, list(sell_price = mean(sell_price),
                                    qty_sales = mean(qty_sales), 
                                    food_inflation = mean(food_inflation), 
                                    fuel_all_grades_forms = mean(fuel_all_grades_forms), 
                                    fuel_all_grades_conv = mean(fuel_all_grades_conv), 
                                    fuel_diesel = mean(fuel_diesel), 
                                    cpi = mean(cpi), 
                                    unemployment_rate = mean(unemployment_rate)), 
                                    by = .(wm_yr_wk, cat_id)][order(wm_yr_wk)]


food_norm <- weekly_norm[cat_id == 'FOODS']
hobbies_norm <- weekly_norm[cat_id == 'HOBBIES']
household_norm <- weekly_norm[cat_id == 'HOUSEHOLD']

#create time series
###############################################################################
#food_data_set
    food_qty_sales <- tsclean(ts(food_norm$qty_sales, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    food_unem <- tsclean(ts(food_norm$unemployment_rate, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    food_cpi <- tsclean(ts(food_norm$cpi, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    food_fuel_all <- tsclean(ts(food_norm$fuel_all_grades_forms, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    food_fuel_conv <- tsclean(ts(food_norm$fuel_all_grades_conv, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    food_fuel_diesel <- tsclean(ts(food_norm$fuel_diesel, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    food_food_inflation <- tsclean(ts(food_norm$food_inflation, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    food_sell_price <- tsclean(ts(food_norm$sell_price, start = c(2011,1,29), frequency = 52), lambda = 'auto')

#hobbies_data_set
    hobbies_qty_sales <- tsclean(ts(hobbies_norm$qty_sales, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    hobbies_unem <- tsclean(ts(hobbies_norm$unemployment_rate, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    hobbies_cpi <- tsclean(ts(hobbies_norm$cpi, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    hobbies_fuel_all <- tsclean(ts(hobbies_norm$fuel_all_grades_forms, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    hobbies_fuel_conv <- tsclean(ts(hobbies_norm$fuel_all_grades_conv, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    hobbies_fuel_diesel <- tsclean(ts(hobbies_norm$fuel_diesel, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    hobbies_food_inflation <- tsclean(ts(hobbies_norm$food_inflation, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    hobbies_sell_price <- tsclean(ts(hobbies_norm$sell_price, start = c(2011,1,29), frequency = 52), lambda = 'auto')

#household_data_set
    household_qty_sales <- tsclean(ts(household_norm$qty_sales, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    household_unem <- tsclean(ts(household_norm$unemployment_rate, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    household_cpi <- tsclean(ts(household_norm$cpi, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    household_fuel_all <- tsclean(ts(household_norm$fuel_all_grades_forms, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    household_fuel_conv <- tsclean(ts(household_norm$fuel_all_grades_conv, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    household_fuel_diesel <- tsclean(ts(household_norm$fuel_diesel, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    household_food_inflation <- tsclean(ts(household_norm$food_inflation, start = c(2011,1,29), frequency = 52), lambda = 'auto')
    household_sell_price <- tsclean(ts(household_norm$sell_price, start = c(2011,1,29), frequency = 52), lambda = 'auto')

# check for seasonal effects
    
    boxplot(food_qty_sales~cycle(food_qty_sales), main = 'Food Quantity Sales')
    boxplot(food_unem~cycle(food_unem), main = 'Unemployment')
    boxplot(food_cpi~cycle(food_cpi), main = 'CPI')
    boxplot(food_fuel_all~cycle(food_fuel_all), main = 'Fuel All Grades/Types')
    boxplot(food_fuel_conv~cycle(food_fuel_conv), main = 'Fuel Conventional')
    boxplot(food_fuel_diesel~cycle(food_fuel_diesel), main = 'Fuel Diesel')
    boxplot(food_food_inflation~cycle(food_food_inflation), main = 'Food Inflation')
    boxplot(food_sell_price~cycle(food_sell_price), main = 'Food Sell Price')

#take log of variables
    adf.test(diff(log(food_qty_sales)), alternative="stationary", k=0)
    adf.test(diff(log(food_unem)), alternative="stationary", k=0)
    adf.test(diff(log(food_cpi)), alternative="stationary", k=0)
    adf.test(diff(log(food_fuel_all)), alternative="stationary", k=0)
    adf.test(diff(log(food_fuel_conv)), alternative="stationary", k=0)
    adf.test(diff(log(food_fuel_diesel)), alternative="stationary", k=0)
    adf.test(diff(log(food_food_inflation)), alternative="stationary", k=0)
    adf.test(diff(log(food_sell_price)), alternative="stationary", k=0)

    
#Prices between goods

#food_hobbies_model

      #find optimal lags
      bound_food_hobbies <- cbind(log(food_sell_price), log(hobbies_qty_sales))
      str(bound_food_hobbies)
      food_hobbies_lagselect <- VARselect(bound_food_hobbies, lag.max = 100, type = 'const')
      food_hobbies_lagselect$selection
      
      acf(log(food_sell_price), main = 'ACF Food Price')
      pacf(log(food_sell_price), main = 'PACF Food Price')
      
      acf(diff(log(hobbies_qty_sales)), main = 'ACF Hobbies Sales')
      pacf(diff(log(hobbies_qty_sales)), main = 'PACF Hobbies Sales')
      
      
      food_hobbies_model <- VAR(bound_food_hobbies, p = 1, type = 'both', season = NULL, )
      summary(food_hobbies_model)
      
      
#Diagnose the VAR Model
      
      #Serial coorelation
      food_hobbies_serial_demand <- serial.test(food_hobbies_model, lags.pt = 52, type = "PT.asymptotic")
      food_hobbies_serial_demand
      
      #heteroscedacticity
      hetero_food_hobbies <- arch.test(food_hobbies_model, lags.multi = 52, multivariate.only = T)
      hetero_food_hobbies
      
      norm_food_hobbies <- normality.test(food_hobbies_model, multivariate.only = T)
      norm_food_hobbies
      
      stability_food_hobbies <- stability(food_hobbies_model, type = "OLS-CUSUM")
      plot(stability_food_hobbies)
      
#food_hobbies_model_causality
      
      food_hobbies_granger <- causality(food_hobbies_model, cause = "log.food_sell_price.")
      food_hobbies_granger
      
#food_household_model
      
      #find optimal lags
      bound_food_household <- cbind(log(food_sell_price), log(household_qty_sales))
      str(bound_food_household)
      food_household_lagselect <- VARselect(bound_food_household, lag.max = 100, type = 'const')
      food_household_lagselect$selection
      
      acf(log(food_sell_price), main = 'ACF Food Price')
      pacf(log(food_sell_price), main = 'PACF Food Price')
      
      acf(diff(log(household_qty_sales)), main = 'ACF Household Sales')
      pacf(diff(log(household_qty_sales)), main = 'PACF Household Sales')
      
      
      food_household_model <- VAR(bound_food_household, p = 1, type = 'both', season = NULL, )
      summary(food_household_model)
      
#Diagnose the VAR Model
      
      #Serial coorelation
      food_household_serial_demand <- serial.test(food_household_model, lags.pt = 52, type = "PT.asymptotic")
      food_household_serial_demand
      
      #heteroscedacticity
      hetero_food_household <- arch.test(food_household_model, lags.multi = 52, multivariate.only = T)
      hetero_food_household
      
      norm_food_household <- normality.test(food_household_model, multivariate.only = T)
      norm_food_household
      
      stability_food_household <- stability(food_household_model, type = "OLS-CUSUM")
      plot(stability_food_household)
      
#food_household_model_causality
      
      food_household_granger <- causality(food_household_model, cause = "log.food_sell_price.")
      food_household_granger
      
      
#food_price_demand_model
      
      #find optimal lags
      bound_food_price_demand <- cbind(log(food_sell_price), log(food_qty_sales))
      str(bound_food_price_demand)
      food_price_demand_lagselect <- VARselect(bound_food_price_demand, lag.max = 100, type = 'const')
      food_price_demand_lagselect$selection
      
      acf(log(food_sell_price), main = 'ACF Food Price')
      pacf(log(food_sell_price), main = 'PACF Food Price')
      
      acf(diff(log(food_qty_sales)), main = 'ACF Food Sales')
      pacf(diff(log(food_qty_sales)), main = 'PACF Food Sales')
      
      food_price_demand_model <- VAR(bound_food_price_demand, p = 1, type = 'both', season = NULL, )
      summary(food_price_demand_model)
      
      
#Diagnose the VAR Model
      
      #Serial coorelation
      food_price_demand_serial_demand <- serial.test( food_price_demand_model, lags.pt = 52, type = "PT.asymptotic")
      food_price_demand_serial_demand
      
      #heteroscedacticity
      hetero_food_price_demand <- arch.test( food_price_demand_model, lags.multi = 52, multivariate.only = T)
      hetero_food_price_demand
      
      norm_food_price_demand <- normality.test( food_price_demand_model, multivariate.only = T)
      norm_food_price_demand
      
      stability_food_price_demand <- stability( food_price_demand_model, type = "OLS-CUSUM")
      plot(stability_food_price_demand)

#food_price_demand_causality
      
      food_price_demand_granger <- causality(food_price_demand_model, cause = "log.food_sell_price.")
      food_price_demand_granger

#food_demand_price_causality

      food_demand_price_granger <- causality(food_price_demand_model, cause = "log.food_qty_sales.")
      food_demand_price_granger      

##########
#household_price_demand_model
      
      #find optimal lags
      bound_household_price_demand <- cbind(log(household_sell_price), log(household_qty_sales))
      str(bound_household_price_demand)
      household_price_demand_lagselect <- VARselect(bound_household_price_demand, lag.max = 100, type = 'const')
      household_price_demand_lagselect$selection
      
      acf(log(household_sell_price), main = 'ACF Household Price')
      pacf(log(household_sell_price), main = 'PACF Household Price')
      
      acf(diff(log(household_qty_sales)), main = 'ACF Household Sales')
      pacf(diff(log(household_qty_sales)), main = 'PACF Household Sales')
      
      household_price_demand_model <- VAR(bound_household_price_demand, p = 1, type = 'both', season = NULL, )
      summary(household_price_demand_model)
      
      
      #Diagnose the VAR Model
      
      #Serial coorelation
      household_price_demand_serial_demand <- serial.test( household_price_demand_model, lags.pt = 52, type = "PT.asymptotic")
      household_price_demand_serial_demand
      
      #heteroscedacticity
      hetero_household_price_demand <- arch.test( household_price_demand_model, lags.multi = 52, multivariate.only = T)
      hetero_household_price_demand
      
      norm_household_price_demand <- normality.test( household_price_demand_model, multivariate.only = T)
      norm_household_price_demand
      
      stability_household_price_demand <- stability( household_price_demand_model, type = "OLS-CUSUM")
      plot(stability_household_price_demand)
      
      #household_price_demand_causality
      
      household_price_demand_granger <- causality(household_price_demand_model, cause = "log.household_sell_price.")
      household_price_demand_granger
      
      #household_demand_price_causality
      
      household_demand_price_granger <- causality(household_price_demand_model, cause = "log.household_qty_sales.")
      household_demand_price_granger 

##########
#hobbies_price_demand_model
      
      #find optimal lags
      bound_hobbies_price_demand <- cbind(log(hobbies_sell_price), log(hobbies_qty_sales))
      str(bound_hobbies_price_demand)
      hobbies_price_demand_lagselect <- VARselect(bound_hobbies_price_demand, lag.max = 100, type = 'const')
      hobbies_price_demand_lagselect$selection
      
      acf(log(hobbies_sell_price), main = 'ACF Hobbies Price')
      pacf(log(hobbies_sell_price), main = 'PACF Hobbies Price')
      
      acf(diff(log(hobbies_qty_sales)), main = 'ACF Hobbies Sales')
      pacf(diff(log(hobbies_qty_sales)), main = 'PACF Hobbies Sales')
      
      hobbies_price_demand_model <- VAR(bound_hobbies_price_demand, p = 1, type = 'both', season = NULL, )
      summary(hobbies_price_demand_model)
      
      
      #Diagnose the VAR Model
      
      #Serial coorelation
      hobbies_price_demand_serial_demand <- serial.test( hobbies_price_demand_model, lags.pt = 52, type = "PT.asymptotic")
      hobbies_price_demand_serial_demand
      
      #heteroscedacticity
      hetero_hobbies_price_demand <- arch.test( hobbies_price_demand_model, lags.multi = 52, multivariate.only = T)
      hetero_hobbies_price_demand
      
      norm_hobbies_price_demand <- normality.test( hobbies_price_demand_model, multivariate.only = T)
      norm_hobbies_price_demand
      
      stability_hobbies_price_demand <- stability( hobbies_price_demand_model, type = "OLS-CUSUM")
      plot(stability_hobbies_price_demand)
      
      #hobbies_price_demand_causality
      
      hobbies_price_demand_granger <- causality(hobbies_price_demand_model, cause = "log.hobbies_sell_price.")
      hobbies_price_demand_granger
      
      #hobbies_demand_price_causality
      
      hobbies_demand_price_granger <- causality(hobbies_price_demand_model, cause = "log.hobbies_qty_sales.")
      hobbies_demand_price_granger 


#Diagnose the VAR Model

#Serial coorelation
serial_demand <- serial.test(demand_model, lags.pt = 30, type = "PT.asymptotic")
serial_demand

#heteroscedacticity
hetero_food <- arch.test(food_model, lags.multi = 30, multivariate.only = T)
hetero_food

norm_food <- normality.test(food_model, multivariate.only = T)
norm_food

stability_food <- stability(food_model, type = "OLS-CUSUM")
plot(stability_food)




Food_Granger <- causality(food_model, cause = "diff.log.food_qty_sales..")
Food_Granger

test <- causality(food_model, cause = "diff.log.food_qty_sales..", vcov.=vcovHC(food_model))
test

############################################################################
#find optimal lags
forecast_food <- cbind(diff(log(food_qty_sales)),diff(log(food_unem)), diff(log(food_cpi)), diff(log(food_fuel_all)), diff(log(food_fuel_conv)), diff(log(food_fuel_diesel)), diff(log(food_food_inflation)), diff(log(food_sell_price)))

str(forecast_food)
forecast_food_lagselect <- VARselect(forecast_food, lag.max = 100, type = 'const')
forecast_food_lagselect$selection


forecast_food_model <- VAR(forecast_food, p = 1, type = 'const', season = NULL, )
summary(forecast_food_model)


#Diagnose the VAR Model

#Serial coorelation
serial_forecast_food <- serial.test(forecast_food_model, lags.pt = 22, type = "PT.asymptotic")
serial_forecast_food

#heteroscedacticity
hetero_forecast_food <- arch.test(forecast_food_model, lags.multi = 22, multivariate.only = T)
hetero_forecast_food

norm_forecast_food <- normality.test(forecast_food_model, multivariate.only = T)
norm_forecast_food

stability_forecast_food <- stability(forecast_food_model, type = "OLS-CUSUM")
plot(stability_forecast_food)

food_forecast <- predict(forecast_food_model, n.ahead = 5, ci = 0.95)
fanchart(food_forecast, names = 'diff.log.food_qty_sales..', main = "Food Qty Sales Forecast")

Food_Granger <- causality(forecast_food_model, cause = "diff.log.food_qty_sales..")
Food_Granger


#hobbies forecast
forecast_hobbies <- cbind(diff(log(hobbies_qty_sales)),diff(log(hobbies_unem)), diff(log(hobbies_cpi)), diff(log(hobbies_fuel_all)), diff(log(hobbies_fuel_conv)), diff(log(hobbies_fuel_diesel)), diff(log(hobbies_food_inflation)), diff(log(hobbies_sell_price)))

str(forecast_hobbies)
forecast_hobbies_lagselect <- VARselect(forecast_hobbies, lag.max = 100, type = 'const')
forecast_hobbies_lagselect$selection


forecast_hobbies_model <- VAR(forecast_hobbies, p = 1, type = 'const', season = NULL, )
summary(forecast_hobbies_model)


#Diagnose the VAR Model

#Serial coorelation
serial_forecast_hobbies <- serial.test(forecast_hobbies_model, lags.pt = 22, type = "PT.asymptotic")
serial_forecast_hobbies

#heteroscedacticity
hetero_forecast_hobbies <- arch.test(forecast_hobbies_model, lags.multi = 22, multivariate.only = T)
hetero_forecast_hobbies

norm_forecast_hobbies <- normality.test(forecast_hobbies_model, multivariate.only = T)
norm_forecast_hobbies

stability_forecast_hobbies <- stability(forecast_hobbies_model, type = "OLS-CUSUM")
plot(stability_forecast_hobbies)

hobbies_forecast <- predict(forecast_hobbies_model, n.ahead = 5, ci = 0.95)
fanchart(hobbies_forecast, names = 'diff.log.hobbies_qty_sales..', main = "hobbies Qty Sales Forecast")

#household forecast
forecast_household <- cbind(diff(log(household_qty_sales)),diff(log(household_unem)), diff(log(household_cpi)), diff(log(household_fuel_all)), diff(log(household_fuel_conv)), diff(log(household_fuel_diesel)), diff(log(household_food_inflation)), diff(log(household_sell_price)))

str(forecast_household)
forecast_household_lagselect <- VARselect(forecast_household, lag.max = 100, type = 'const')
forecast_household_lagselect$selection


forecast_household_model <- VAR(forecast_household, p = 1, type = 'const', season = NULL, )
summary(forecast_household_model)


#Diagnose the VAR Model

#Serial coorelation
serial_forecast_household <- serial.test(forecast_household_model, lags.pt = 22, type = "PT.asymptotic")
serial_forecast_household

#heteroscedacticity
hetero_forecast_household <- arch.test(forecast_household_model, lags.multi = 22, multivariate.only = T)
hetero_forecast_household

norm_forecast_household <- normality.test(forecast_household_model, multivariate.only = T)
norm_forecast_household

stability_forecast_household <- stability(forecast_household_model, type = "OLS-CUSUM")
plot(stability_forecast_household)

household_forecast <- predict(forecast_household_model, n.ahead = 5, ci = 0.95)
fanchart(household_forecast, names = 'diff.log.household_qty_sales..', main = "household Qty Sales Forecast")

