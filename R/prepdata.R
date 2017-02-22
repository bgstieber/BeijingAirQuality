library(tidyverse)
theme_set(theme_bw())
library(lubridate)
library(scales)
library(zoo)

code_hours <- function(x){
  xnew <- 
    ifelse(x <= 3, '0-3',
         ifelse(x <= 7, '4-7',
                ifelse(x <= 11, '8-11',
                       ifelse(x <= 15, '12-15',
                              ifelse(x <= 19, '16-19', '20-23')))))
  
  factor(xnew, levels = c('0-3','4-7','8-11','12-15','16-19','20-23'))
}

#read all csvs

years <- 2010:2016
file_1 <- 'Beijing_'
file_2 <- '_Hourly.csv'

for(yr in years){
  newfile <- paste0('Data/',file_1, yr, file_2)
  
  assign(paste0('air_q_',yr), 
         read.csv(newfile, skip = 3, stringsAsFactors = FALSE,
                  na.strings = -999))
}


#rbind data together

air_q_all <- rbind(air_q_2010,
                   air_q_2011,
                   air_q_2012,
                   air_q_2013,
                   air_q_2014,
                   air_q_2015,
                   air_q_2016) %>%
  filter(Value > 0) %>%
  mutate(ShortDate = ifelse(Year %in% 2015:2016, 
                            as.Date(Date..LST., '%m/%d/%Y'),
                            as.Date(Date..LST., '%Y-%m-%d')),
         Week = week(as.Date(ShortDate, origin = '1970-01-01')),
         Hour_Bin = code_hours(Hour),
         DayOfYear = yday(as.Date(ShortDate, origin = '1970-01-01')))

#remove missing data
table(air_q_all$QC.Name)

air_q_all <- subset(air_q_all, QC.Name != 'Missing') %>%
  filter(Year != 2010)

#summarise the data
#grouping by year, month, and day, calculate:
#avg, median, 2.5% q, 97.5% q, 5% q, 95% q, sd

air_q_all.byday <- air_q_all %>%
  group_by(Year, Month, Day, DayOfYear) %>%
  summarise(AvgPollution = mean(Value), 
            MedPollution = median(Value),
            SDPollution = sd(Value),
            MaxPollution = max(Value),
            MinPollution = min(Value),
            Lower95Pollution = quantile(Value, .025),
            Upper95Pollution = quantile(Value, .975),
            Lower90Pollution = quantile(Value, .05),
            Upper90Pollution = quantile(Value, .10),
            Lower50Pollution = quantile(Value, .25),
            Upper50Pollution = quantile(Value, .75),
            Observations = n()) %>%
  ungroup() %>%
  mutate(Date = as.Date(paste0(Year, '-', Month, '-', Day))) %>%
  arrange(Date) %>%
  mutate(RollingAvgPollution = rollmean(AvgPollution, k = 7, fill = NA))

#grab weather data

weather_data <- read.csv('Data/BeijingWeatherData11_16.csv', 
                         stringsAsFactors = FALSE) %>%
  select(CST, 
         MaxTemperatureF, MaxHumidity, MaxWindSpeedMPH,
         MaxSeaLevelPressureIn,
         MeanTemperatureF, MeanHumidity, MeanWindSpeedMPH, 
         MeanSeaLevelPressureIn,
         MinTemperatureF, MinHumidity, MinSeaLevelPressureIn,
         MaxGustSpeedMPH, PrecipitationIn,
         Events) %>%
  mutate(Year = year(CST),
         Month = month(CST),
         Day = day(CST),
         Events = ifelse(Events == '', 'Clear', Events))

#merge weather data to pm2.5 data
air_q_all.byday <- merge(x = air_q_all.byday, y = weather_data,
                         by.x = c('Year','Month','Day'),
                         by.y = c('Year','Month','Day'),
                         all.x = TRUE)

#add rolling averages for temp, humidity, wind speed

air_q_all.byday <- air_q_all.byday %>%
  arrange(Date) %>%
  mutate(MeanTempF_RollingAvg = rollmean(MeanTemperatureF,
                                         k = 7, fill = NA),
         MeanHumid_RollingAvg = rollmean(MeanHumidity,
                                         k = 7, fill = NA),
         MeanWindSpeed_RollingAvg = rollmean(MeanWindSpeedMPH,
                                             k = 7, fill = NA))


#investigate other rolling averages
rolling_avg_dat <- 
  do.call('rbind', 
lapply(seq(5, 21, by = 2), 
       FUN = function(k){
          air_q_all.byday %>%
          select(Date, Year, Month, Day, 
                 AvgPollution, MeanHumidity,
                 MeanWindSpeedMPH, MeanTemperatureF) %>%
          arrange(Date) %>%
          mutate(
            Pollution = rollmean(AvgPollution, k = k, fill = NA),
            Humidity = rollmean(MeanHumidity, k = k, fill = NA),
            WindSpeed = rollmean(MeanWindSpeedMPH, k = k, fill = NA),
            Temperature = rollmean(MeanTemperatureF, k = k, fill = NA),
            RollWindow = k)
         }
       )
)


#group by year and week
air_q_all %>%
  group_by(Year, Week) %>%
  summarise(AvgPollution = mean(Value), 
            MedPollution = median(Value),
            SDPollution = sd(Value),
            MaxPollution = max(Value),
            MinPollution = min(Value),
            Lower95Pollution = quantile(Value, .025),
            Upper95Pollution = quantile(Value, .975),
            Lower90Pollution = quantile(Value, .05),
            Upper90Pollution = quantile(Value, .10),
            Lower50Pollution = quantile(Value, .25),
            Upper50Pollution = quantile(Value, .75),
            Observations = n()) -> air_q_all.byweek


