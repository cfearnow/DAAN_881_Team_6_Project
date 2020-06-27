library(data.table)
library(DMwR)
library(psych)
library(Hmisc)
library(forecast)
library(tsoutliers)
library(aTSA)
library(lubridate)
library(caret)
library(psych)

####################################################################################################################
#import data

#create month names dt
month_names <- data.table('Period' = c('M01', 'M02', 'M03', 'M04', 'M05', 'M06', 'M07', 'M08', 'M09', 'M10', 'M11', 'M12') , 'Month_No' = c(1,2,3,4,5,6,7,8,9,10,11,12), 'Month' = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))

#import calendar data
calendar_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/M5_Forecasting_Accuracy/calendar.csv')
calendar_data[, date := as.Date(date, format = "%m/%d/%Y") ]
summary(calendar_data)
str(calendar_data)
Hmisc::describe(calendar_data)

#indentify duplicate entries in calandar data
calendar_dups <- calendar_data[, duplicate := (duplicated(date) | duplicated(date, fromLast = T))]
table(calendar_dups$duplicate)

#import training data
training_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/M5_Forecasting_Accuracy/sales_train_evaluation.csv')
summary(training_data)
str(training_data)
Hmisc::describe(training_data[, c(1:6)])

#indentify duplicate entries in training data
training_dups <- training_data[, duplicate := (duplicated(id) | duplicated(id, fromLast = T))]
table(training_dups$duplicate)

#import sell price data
sell_price_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/M5_Forecasting_Accuracy/sell_prices.csv')
summary(sell_price_data)
str(sell_price_data)
apply(is.na(sell_price_data), 2, sum)
Hmisc::describe(sell_price_data)

pivot_price <- sell_price_data[, .N, by = wm_yr_wk]

#indentify duplicate entries in calandar data
dups <- sell_price_data[, c('store_id', 'item_id', 'wm_yr_wk')]
sell_price_dups <- sell_price_data[, duplicate := (duplicated(dups) | duplicated(dups, fromLast = T))]
table(sell_price_dups$duplicate)
Hmisc::describe(pivot_price)

#import fuel price data
fuel_price_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/additional_data/Fuel_Prices.csv')
fuel_price_data[, Date := as.Date(Date, format = "%m/%d/%Y") ]
summary(fuel_price_data)
str(fuel_price_data)
apply(is.na(fuel_price_data), 2, sum)

#indentify duplicate entries in fuel data
fuel_dups <- fuel_price_data[, Date]
fuel_price_dups <- fuel_price_data[, duplicate := (duplicated(fuel_dups) | duplicated(fuel_dups, fromLast = T))]
table(fuel_price_dups$duplicate)

#add week number and year to fuel price data
fuel_price_data[, year := as.integer(strftime(Date, format = "%Y"))]

#only relavent fuel price columns
fuel_price_sub <- fuel_price_data[, c('year', 'week_number', 'Weekly U.S. All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)', 'Weekly U.S. All Grades Conventional Retail Gasoline Prices  (Dollars per Gallon)', 'Weekly U.S. No 2 Diesel Ultra Low Sulfur (0-15 ppm) Retail Prices  (Dollars per Gallon)')]
colnames(fuel_price_sub) <- c('year', 'week_num', 'fuel_all_grades_forms', 'fuel_all_grades_conv', 'fuel_diesel')

#import food inflation data
food_inflation_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/additional_data/Historical_Food_Inflation.csv')
summary(food_inflation_data)
str(food_inflation_data)
apply(is.na(food_inflation_data), 2, sum)

food_inflation_melt <- melt(food_inflation_data[, -'AVE'], id.vars='YEAR')
food_inflation_melt$variable <- tolower(food_inflation_melt$variable)
food_inflation_melt[, Month_No := month_names$Month_No[match(variable, month_names$Month)]]
colnames(food_inflation_melt) <- c('year', 'month', 'food_inflation', 'month_no')
food_inflation_sub <- food_inflation_melt[, c('year', 'food_inflation', 'month_no')]

#import cpi data
cpi_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/additional_data/Historical_CPI_Data.csv')
summary(cpi_data)
str(cpi_data)
apply(is.na(cpi_data), 2, sum)

cpi_melt <- melt(cpi_data[, -c('Avg', 'Dec-Dec', 'Avg-Avg')], id.vars='Year')
cpi_melt[, month := variable]
cpi_melt[variable == 'July', month := 'jul']
cpi_melt[variable == 'June', month := 'jun']
cpi_melt$month <- tolower(cpi_melt$month)

cpi_melt[, Month_No := month_names$Month_No[match(month, month_names$Month)]]
cpi_sub <- cpi_melt[, c('Year', 'Month_No', 'value')]
colnames(cpi_sub) <- c('year', 'month_no', 'cpi')

#import unemployment data
unemployment_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/additional_data/Unemployment_Data_Seasonal_Adj.csv')
summary(unemployment_data)
str(unemployment_data)
apply(is.na(unemployment_data), 2, sum)

#fix months in unemploymet data
unemployment_data[, Month_No := month_names$Month_No[match(Period, month_names$Period)]]
unemployment_sub <- unemployment_data[, c('Year', 'Month_No', 'Value')]
colnames(unemployment_sub) <- c('year', 'month_no', 'unemployment_rate')

#######################################################################################################################
#merge data sets

unemploy_merge <- merge(calendar_data, unemployment_sub, by.x = c('month', 'year'), by.y = c('month_no', 'year'), all.x = T)
cpi_merge <- merge(unemploy_merge, cpi_sub, by.x = c('month', 'year'), by.y = c('month_no', 'year'), all.x = T)
fuel_merge <- merge(cpi_merge, fuel_price_sub, by.x = c('analysis_week', 'analysis_year'), by.y = c('week_num', 'year'), all.x = T)
merged <- merge(fuel_merge, food_inflation_sub, by.x = c('month', 'year'), by.y = c('month_no', 'year'), all.x = T)
merged[, Month_Name := month_names$Month[match(month, month_names$Month_No)]]
merged[, number := 1:nrow(merged)]

cor_set <- merged[,c('unemployment_rate', 'cpi', 'fuel_all_grades_forms', 'fuel_all_grades_conv', 'fuel_diesel', 'food_inflation')]
dummy_set <- merged[,-c('month', 'year', 'week_number','analysis_week', 'analysis_year', 'date', 'wm_yr_wk', 'wday', 'd', 'event_name_1', 'event_name_2')]
non_dummy_set <- merged[,c('week_number','analysis_week','month', 'year', 'week_number', 'date', 'wm_yr_wk', 'wday', 'd', 'event_name_1', 'event_name_2', 'number')]

dmys <- dummyVars("~.", data = dummy_set)
dummy_vars <- data.table(predict(dmys, newdata = dummy_set))
dummy_chart <- dummy_vars[, -c('number')]

cors <- cor(cor_set)

corPlot(cors, main = 'corPlot of Numeric Veriables', labels = c('Unemployment', 'CPI', 'Fuel (All)', 'Fuel(Conv)', 'Fuel (Diesel)', 'Food Inflation'))

dummy_merge <- merge(dummy_vars, non_dummy_set, by = 'number')

write.csv(final_merge, 'training_data_merged.csv')

########################################################################################################################

#tidy training data (long)
training_dates <- colnames(final_merge[,-c(1:6)])

long_training = melt(training_data, measure.vars = training_dates, value.name = 'qty_sales')
setnames(long_training, 'variable', 'd')

#merge calendar and training
training_whole <- setorder(merge(long_training, calendar_data, by.x = 'd', by.y = 'd'), date)

#time series analysis on complete data set

ts_training_whole <- setorder(training_whole[, sum(qty_sales), by = date],date)
ts_training_whole$date <- as.Date(ts_training_whole$date, format = "%Y-%m-%d")
class(ts_training_whole$date)

ts_training <- ts(ts_training_whole$V1, start=c(2011, 1, 29), frequency=365 )
plot.ts(ts_training)
training_comp <- decompose(ts_training)
training_comp$seasonal
plot(training_comp)

#acf and pacf for complete set
acf(ts_training,main="ACF of Total Data")
pacf(ts_training,main="PACF of TOtal Data")

#remove outliers
after_ol_removed <-tsclean(ts_training)

#acf after removing outliers
acf(after_ol_removed,main="ACF of Total Data")
plot.ts(after_ol_removed)

#retry using weekly data
ts_training_weekly <- setorder(training_whole[, sum(qty_sales), by = wm_yr_wk],wm_yr_wk)

ts_training_wk <- ts(ts_training_weekly$V1, start=c(2011, 1), frequency=52)
plot.ts(ts_training_wk)
training_comp_wk <- decompose(ts_training_wk)
training_comp_wk$seasonal
plot(training_comp_wk)

adf.test(ts_training_wk)
acf(ts_training_wk,main="ACF of Weekly Data")

#arima
test_arima <- arima(x = ts_training_whole$V1, order = c(5, 0, 0))
test_arima
test_aa <- auto.arima(ts_training_whole$V1)
test_aa
test_arima

plot(test_aa)
resid_arima <- residuals(test_arima)
pars_arima <- coefs2poly(test_arima)

outliers <- locate.outliers(resid_arima, pars_arima)
setDT(outliers)
outliers_pivot <- outliers[, .N, by = 'type']


#subset california
ts_training_CA <- setorder(training_whole[state_id == 'CA', sum(qty_sales), by = wm_yr_wk],wm_yr_wk)

#descriptive stats california
mean(ts_training_CA$V1)
sd(ts_training_CA$V1)
min(ts_training_CA$V1)
max(ts_training_CA$V1)

ts_training_CA$date <- as.Date(ts_training_CA$date, format = "%Y-%m-%d")
class(ts_training_CA$date)

#arima california
test_arima_ca <- arima(x = ts_training_CA$V1, order = c(1, 0, 0))
test_arima_ca
test_aa_ca <- auto.arima(ts_training_CA$V1)
test_aa_ca

plot(test_arima_ca)
resid_arima_ca <- residuals(test_arima_ca)
pars_arima_ca <- coefs2poly(test_arima_ca)

outliers_ca <- locate.outliers(resid_arima_ca, pars_arima_ca)
setDT(outliers_ca)
outliers_pivot_ca <- outliers_ca[, .N, by = 'type']

#subset texas
ts_training_TX <- setorder(training_whole[state_id == 'TX', sum(qty_sales), by = wm_yr_wk],wm_yr_wk)

#descriptive stats california
mean(ts_training_TX$V1)
sd(ts_training_TX$V1)
min(ts_training_TX$V1)
max(ts_training_TX$V1)
nrow(ts_training_TX)

ts_training_TX$date <- as.Date(ts_training_TX$date, format = "%Y-%m-%d")
class(ts_training_TX$date)

#arima texas
test_arima_TX <- arima(x = ts_training_TX$V1, order = c(1, 0, 0))
test_arima_TX
test_aa_TX <- auto.arima(ts_training_TX$V1)
test_aa_TX

plot(test_arima_TX)
resid_arima_tx <- residuals(test_arima_TX)
pars_arima_tx <- coefs2poly(test_arima_TX)

outliers_tx <- locate.outliers(resid_arima_tx, pars_arima_tx)
setDT(outliers_tx)
outliers_pivot_tx <- outliers_tx[, .N, by = 'type']

#subset wisconsin
ts_training_WI <- setorder(training_whole[state_id == 'WI', sum(qty_sales), by = wm_yr_wk],wm_yr_wk)

#descriptive stats wisconsin
mean(ts_training_WI$V1)
sd(ts_training_WI$V1)
min(ts_training_WI$V1)
max(ts_training_WI$V1)
nrow(ts_training_WI)

ts_training_WI$date <- as.Date(ts_training_WI$date, format = "%Y-%m-%d")
class(ts_training_WI$date)

wi <- ts(ts_training_WI$date, start=c(2011, 1, 29), frequency=365 )
acf(wi,main="ACF of Wisconsin Subset")
pacf(wi,main="PACF of Wisconsin Subset")

#arima wisconsin
test_arima_WI <- arima(x = ts_training_WI$V1, order = c(1, 0, 0))
test_arima_WI


test_aa_WI <- auto.arima(ts_training_WI$V1)
test_aa_WI

plot(test_arima_WI)
resid_arima_WI <- residuals(test_arima_WI)
pars_arima_WI <- coefs2poly(test_arima_WI)

outliers_WI <- locate.outliers(resid_arima_WI, pars_arima_WI)
setDT(outliers_WI)
outliers_pivot_WI <- outliers_WI[, .N, by = 'type']


evaluation_data <- fread('D:/Chris_Files/PSU_One_Drive/OneDrive - The Pennsylvania State University/DAAN_881_Team_6/Team_6_DAAN_881_Data_Files/csv_converted_files/M5_Forecasting_Accuracy/sales_train_validation.csv')
#summary(evaluation_data)
str(evaluation_data)

#tidy evaluation data (long)

eval_dates <- colnames(evaluation_data[,-c(1:6)])

long_eval = melt(evaluation_data, measure.vars = eval_dates, value.name = 'qty_sales')
setnames(long_eval, 'variable', 'd')

#merge calendar and training
eval_whole <- setorder(merge(long_eval, calendar_data, by.x = 'd', by.y = 'd'), date)

#subset california
ts_eval_CA <- setorder(eval_whole[state_id == 'CA', sum(qty_sales), by = wm_yr_wk],wm_yr_wk)

#descriptive stats california
mean(ts_eval_CA$V1)
sd(ts_eval_CA$V1)
min(ts_eval_CA$V1)
max(ts_eval_CA$V1)

ts_eval_CA$date <- as.Date(ts_eval_CA$date, format = "%Y-%m-%d")
class(ts_eval_CA$date)

#arima california
test_arima_ca <- arima(x = ts_eval_CA$V1, order = c(1, 0, 0))
test_arima_ca
test_aa_ca <- auto.arima(ts_eval_CA$V1)
test_aa_ca

plot(test_arima_ca)
resid_arima_ca <- residuals(test_arima_ca)
pars_arima_ca <- coefs2poly(test_arima_ca)

outliers_ca <- locate.outliers(resid_arima_ca, pars_arima_ca)
setDT(outliers_ca)
outliers_pivot_ca <- outliers_ca[, .N, by = 'type']

#subset texas
ts_eval_TX <- setorder(eval_whole[state_id == 'TX', sum(qty_sales), by = wm_yr_wk],wm_yr_wk)

#descriptive stats california
mean(ts_eval_TX$V1)
sd(ts_eval_TX$V1)
min(ts_eval_TX$V1)
max(ts_eval_TX$V1)
nrow(ts_eval_TX)

ts_eval_TX$date <- as.Date(ts_eval_TX$date, format = "%Y-%m-%d")
class(ts_eval_TX$date)

#arima texas
test_arima_TX <- arima(x = ts_eval_TX$V1, order = c(1, 0, 0))
test_arima_TX
test_aa_TX <- auto.arima(ts_eval_TX$V1)
test_aa_TX

plot(test_arima_TX)
resid_arima_tx <- residuals(test_arima_TX)
pars_arima_tx <- coefs2poly(test_arima_TX)

outliers_tx <- locate.outliers(resid_arima_tx, pars_arima_tx)
setDT(outliers_tx)
outliers_pivot_tx <- outliers_tx[, .N, by = 'type']

#subset wisconsin
ts_eval_WI <- setorder(eval_whole[state_id == 'WI', sum(qty_sales), by = wm_yr_wk],wm_yr_wk)

#descriptive stats california
mean(ts_eval_WI$V1)
sd(ts_eval_WI$V1)
min(ts_eval_WI$V1)
max(ts_eval_WI$V1)
nrow(ts_eval_WI)

ts_eval_WI$date <- as.Date(ts_eval_WI$date, format = "%Y-%m-%d")
class(ts_eval_WI$date)

wi <- ts(ts_eval_WI$date, start=c(2011, 1, 29), frequency=365 )
acf(wi,main="ACF of Wisconsin Subset")
pacf(wi,main="PACF of Wisconsin Subset")

#arima wisconsin
test_arima_WI <- arima(x = ts_eval_WI$V1, order = c(1, 0, 0))
test_arima_WI


test_aa_WI <- auto.arima(ts_eval_WI$V1)
test_aa_WI

plot(test_arima_WI)
resid_arima_WI <- residuals(test_arima_WI)
pars_arima_WI <- coefs2poly(test_arima_WI)

outliers_WI <- locate.outliers(resid_arima_WI, pars_arima_WI)
setDT(outliers_WI)
outliers_pivot_WI <- outliers_WI[, .N, by = 'type']

Hmisc::describe(evaluation_data[, c(1:6)])

#merge training and sales
training_complete <- merge(training_whole, sell_price_data, by = c('store_id', 'item_id', 'wm_yr_wk'))

by(training_whole, training_whole$store_id, summary)
psych::describeBy(training_whole, training_whole$store_id)

store_category_group <- training_whole[, sum(qty_sales),by = c('cat_id', 'state_id', 'date')]
setnames(store_category_group, 'V1', 'daily_sales')

training_ts <- ts(store_category_group, frequency=365, start=c(2011,01,29))

hist_training <- hist(training_data$d_1, breaks = 500)

#analyze training data
training_outliers <- lofactor(training_data$d_1, k=5)
kmeans_training_outliers <- kmeans()


