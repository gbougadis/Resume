
# Loading Environment -----------------------------------------------------

library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
library(TTR)
library(fpp2)

con = dbConnect(MariaDB(), user="deepAnalytics",password="Sqltask1234!", dbname="dataanalytics2018", host="data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com")

# Listing Tables in Connection
dbListTables(con)
yr2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

# Binding tables with full years worth of data
energy <- bind_rows(yr2007, yr2008, yr2009)
energy <- cbind(energy, paste(energy$Date, energy$Time), stringsAsFactors = FALSE)
str(energy)

# Renaming DateTime field
colnames(energy)[11] <- "DateTime"
str(energy)

# moving the DateTime attribute within the dataset
energy <- energy[,c(ncol(energy), 1:(ncol(energy)-1))]
str(energy)

# Convert the DateTime field from UNIXtime
energy$DateTime <- as.POSIXct(energy$DateTime, "%Y/%m/%d %H:%M:%S")
str(energy)

attr(energy$DateTime, "tzone") <- "Europe/Paris"
str(energy)

# Creating a year attribute with Lubridate
energy$year <- year(energy$DateTime)
energy$week <- week(energy$DateTime)
energy$month <- month(energy$DateTime)
energy$day <- day(energy$DateTime)
energy$wday <- wday(energy$DateTime)
energy$hour <- hour(energy$DateTime)
energy$minute <- minute(energy$DateTime)

# Creating data subsets
# This will plot the second week of 2008
houseWeek <- filter(energy, year == 2008 & week == 2)
# This granularity still has over 10,000 observations and granularity can be reduced further

# Intraday plotting with plotly package
houseDay <- filter(energy, year == 2008 & month == 1 & day == 9)
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = "Kitchen", type = "Scatter", mode = "lines" %>%
            add_trace(y = ~houseDay$Sub_metering_2, name = "Laundry Room", mode = "lines" %>%
            add_trace(y = ~houseDay$Sub_metering_3, name= "Water Heater & AC", mode = "Lines")%>%
            layout(title = "Power Consumption January 9th, 2008", xaxis = list(title= "Time"), yaxis=list(title = "Power (watt-hours)"))
        
            plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
              add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
              add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
              layout(title = "Power Consumption January 9th, 2008",
                     xaxis = list(title = "Time"),
                     yaxis = list (title = "Power (watt-hours)"))
# we can reduce this frequency further (10 minute frequencies)
houseDay10 <- filter(energy, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))            

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
              add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
              add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
              layout(title = "Power Consumption January 9th, 2008",
                     xaxis = list(title = "Time"),
                     yaxis = list (title = "Power (watt-hours)"))   

#Week 2 of December in 2009
houseDecWeek<- filter(energy, year == 2009 & week == 50 )
plot_ly(houseDecWeek, x = ~houseDecWeek$DateTime, y = ~houseDecWeek$Sub_metering_1, name = "Kitchen", type = "Scatter", mode = "lines" %>%
          add_trace(y = ~houseDecWeek$Sub_metering_2, name = "Laundry Room", mode = "lines" %>%
                      add_trace(y = ~houseDecWeek$Sub_metering_3, name= "Water Heater & AC", mode = "Lines")%>%
                      layout(title = "Power Consumption 2nd Week of December, 2009", xaxis = list(title= "Time"), yaxis=list(title = "Power (watt-hours)"))
                    
                    plot_ly(houseDecWeek, x = ~houseDecWeek$DateTime, y = ~houseDecWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
                      add_trace(y = ~houseDecWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
                      add_trace(y = ~houseDecWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
                      layout(title = "Power Consumption 2nd Week of December, 2009",
                             xaxis = list(title = "Time"),
                             yaxis = list (title = "Power (watt-hours)"))
                    
# increasing frequency of the time frame (30 minute time frame)
houseDecWeek<- filter(energy, year == 2009 & week == 50 & (hour %% 4==0))
dailyavg2009 <- energy %>% group_by(year, month)%>%summarise(sub_metering_1 = sum(Sub_metering_1), sub_metering_2 = SUM(Sub_metering_2), sub_metering_3 = SUM(sub_metering_3), Global_active_power = sum(Global_active_power))
hourly_energy <- energy %>% group_by(hour) %>% summarise(Sub_metering_1=sum(Sub_metering_1),
                                                            Sub_metering_2=sum(Sub_metering_2),
                                                            Sub_metering_3=sum(Sub_metering_3),
                                                            Global_active_power=sum(Global_active_power))x
# Visualizing data consumption by sub-meter for the year 2009
data2009 <- energy %>% dplyr::filter(`year` == 2009) %>% group_by(day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000))
ggplot() + geom_line(data = data2009, aes(x=DateTime, y = ) 


# Selecting a specific time-range to filter
house070809weekly <- dplyr::filter(energy, wday == 2 & hour == 20 & minute == 1)
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))
tsSM3_070809weekly

# Preliminary analysis
autoplot(tsSM3_070809weekly, ts.colour = "red") + ggtitle("Time Plot: Sub-Meter 3") + ylab("Energy Usage")
autoplot(tsSM3_070809weekly, color = "red", xlab = "Time", ylab = "Energy Usage", main = "Time Plot: Sub-Meter 3")

# Plotting with plot.ts
plot.ts(tsSM3_070809weekly)

# Distributing the sums by types of submeter
MonthlySum <- energy %>%
  group_by(year, month) %>% 
  summarise(across(starts_with('sub'), sum))


MonthSumGather <- gather(MonthlySum, 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3',
                         key = 'submeter', value = 'amount')


# Monthly Energy Use by Year
subset(MonthSumGather, year != 2010) %>%
  ggplot(aes(month, amount, color=submeter)) +
  geom_line(size = 1) +
  facet_grid(year~.) +
  theme_bw() +
  theme(aspect.ratio = .25,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(0,0,0,6),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Set1', name = 'Submeter: ', labels = c('Kitchen', 'Laundry', 'Water Heater & AC')) +
  scale_x_discrete(limits=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Monthly Energy Use by Year')

# Bar Chart Monthly Energy use by year
ggplot() +
  geom_col(data=MonthSumGather,
           aes(x=year, y=amount, fill=submeter),
           position="dodge") +
  labs(x="Year", y="Consumption Watt/Hour", title="Total Consumption by Sub-meters") +
  scale_y_continuous(labels=scales::comma) +
  theme_linedraw(base_size = 11, base_family = "") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

# Applying time series linear regression to sub-meter 3 ts
# Forecasting our time series for Tuesday nights from 2007-2010
library(forecast)
library(fpp2)
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)
forecastfitSM3 <- forecast(fitSM3, h=20)
plot(forecastfitSM3)

# Removing negative forecasted values. Also adding lables, changing confidence level and ploting only forecast portion
forecastfitSM3c <- forecast(fitSM3, h = 20, level =c(80,90))
plot(forecastfitSM3c, ylim = c(0,20), ylab = "Watt-Hours", xlab = "Time")
summary(forecastfitSM3c)
checkresiduals(forecastfitSM3c)



plot(forecastSM2)

#Plotting seasonality test
seasonality_test <- dplyr::filter(energy, wday == 2 & hour == 20 & minute == 1)

ggseasonplot(tsSM3_070809weekly)
ggsubseriesplot(tsSM3_070809weekly)



monthly_sm1 <- ts(energy$Sub_metering_3, start = c(2007, 1), end = c(2009, 12), frequency = 12)
monthlysm1
tslm(monthly_sm1 ~ trend + season)
autoplot(monthlysm1, color = "red") + ggtitle("Time Plot: Sub-Meter 1") + ylab("Energy Usage")

# Forecasting Sub-Meter 2
daily_sm2 <- dplyr::filter(energy, wday == 3 & hour == 18 & minute == 0)
dailysm2 <- ts(daily_sm2$Sub_metering_2, frequency = 52, start = c(2007, 1), end = c(2010, 1))
dailysm2
fitSM2 <- tslm(dailysm2 ~ trend + season)
forecastfitSM2 <- forecast(fitSM2, h = 20, level = c(90,95))
autoplot(forecastfitSM2)
plot(forecastfitSM2, ylim = c(0,60), ylab = "Watt-Hours", xlab = "Time")
checkresiduals(forecastfitSM2)


# Forecasting Sub-Meter 1 for Same time period as Sub-Meter 2
daily_for_sm1 <- dplyr::filter(energy, wday == 3 & hour == 18 & minute == 0)
dailysm1 <- ts(daily_for_sm1$Sub_metering_1, frequency = 52, start = c(2007, 1), end = c(2010, 1))
dailysm1
fitSM1 <- tslm(dailysm1 ~ trend + season)
forecastfitSM1 <- forecast(fitSM1, h = 20, level = c(90,95))
autoplot(forecastfitSM1)
plot(forecastfitSM1, ylab = "Watt-Hours", xlab = "Time")
checkresiduals(forecastfitSM1)


# 1st Submeter (Monthly) Forecast
energy_monthly <- energy %>% group_by(year(Date), quarter(Date)) %>% summarise(sm1 = sum(`Sub_metering_1`)/1000,
                                                                              sm2 = sum(`Sub_metering_2`)/1000,
                                                                              sm3 = sum(`Sub_metering_3`)/1000)
monthly_ts <- ts(energy_monthly[, 3], frequency = 4, start = c(2007, 1), end = c(2010, 1))
View(monthly_ts)
autoplot(monthly_ts, color = "red") + ggtitle("Time Plot: Sub-Meter 1 Quarterly")

fit_quarterlysm1 <- tslm(monthly_ts ~ trend + season)
forecast_quarterlysm1 <- forecast(fit_quarterlysm1, h= 20, level = c(90,95))
plot(forecast_quarterlysm1)
autoplot(forecastsm1_Monthly)
summary(fit_quarterlysm1)
checkresiduals(fit_quarterlysm1)

# SM3 Forecast
autoplot(dailysm2, color = "red") + ggtitle("Time Plot: Sub-Meter 2") + ylab("Energy Usage")
# TS tends not to be a good fit for daily data but we will plot it here as a demonstration.
plot.ts(dailysm2)
# Zoo package is more fit for this date range
#Time Plot 
## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
forecastSM3 <- forecast(fitSM3, h= 20, level = c(90,95))
plot(forecastSM3, ylim = c(0,30), ylab = "Watt-Hours", xlab = "Time")
plot(forecastSM3)
summary(fitSM3)
checkresiduals(forecastSM3)
autoplot(tsSM3_070809weekly)

# Holt-Winters Forecasting ------------------------------------------------
#  Decomposing Sub-meter3 into trend, season, and remainder ---------------
components070809weekly <- decompose(tsSM3_070809weekly)
plot(components070809weekly)
summary(components070809weekly)
decomposed_SM3 <- tsSM3_070809weekly - components070809weekly$seasonal


# Need to remove seasonal components
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809weekly$seasonal
autoplot(tsSM3_070809Adjusted)

# Testing seasonal adjustment
plot(decompose(tsSM3_070809Adjusted))

# SM decompositions:
components_dailysm2 <- decompose(dailysm2)
plot(components_dailysm2)
summary(components_dailysm2)

# Summary stats for decomposition of time-series
#summary(components070809weekly)
summary(components_dailysm2)


# Decomposition for Sub-Meter 1 -------------------------------------------
# Pre-Decomposition
plot(dailysm1)

component_dailysm1 <- decompose(dailysm1)
plot(component_dailysm1)
summary(component_dailysm1)
decomposed_SM1 <- dailysm1 - component_dailysm1$seasonal
autoplot(decomposed_SM1)


# Seasonal decomposition Sub-Meter 2 Continued
dailysm2_decomposedAdjusted <- dailysm2 - components_dailysm2$seasonal
autoplot(dailysm2)
autoplot(dailysm2_decomposedAdjusted)
plot(decompose(dailysm2_decomposedAdjusted))
summary(decompose(dailysm2))



# Testing seasonal adjustment
plot(decompose(tsSM3_070809Adjusted))
# Seasonal scale is much smaller, therefore removed.

# HW Exponential smoothing
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM3_HW070809, ylim = c(0,25))

# Holt-Winters forecast
tSSM3_HW070809forecast <- forecast(tsSM3_HW070809, h = 25)
plot(tSSM3_HW070809forecast, ylim = c(0,20), ylab = "Watt Hours", xlab = "Time - SM3")
# Forecast Only
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


# Holt-Winters forecast for Sub-Meter 1
HW_DailySM1<- HoltWinters(dailysm1, beta = FALSE, gamma = FALSE)
plot(HW_DailySM1)
# Forecast
dailysm1_HWforecast <- forecast(HW_DailySM1, h = 10)
plot(dailysm1_HWforecast,  ylab = "Watt Hours", xlab = "Time - SM1", start(2010))


# Holt-Winters forecast for Sub-Meter 2
HW_DailySM2 <- HoltWinters(dailysm2_decomposedAdjusted, beta = FALSE, gamma = FALSE)
plot(HW_DailySM2)
# Forecast
dailysm2_HWforecast <- forecast(HW_DailySM2, h=20)
plot(dailysm2_HWforecast,  ylab = "Watt Hours", xlab = "Time - SM1", start(2010))
