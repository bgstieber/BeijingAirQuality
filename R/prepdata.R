library(tidyverse)
theme_set(theme_bw())
library(lubridate)

code_hours <- function(x){
  ifelse(x <= 3, '0-3',
         ifelse(x <= 7, '4-7',
                ifelse(x <= 11, '8-11',
                       ifelse(x <= 15, '12-15',
                              ifelse(x <= 19, '16-19', '20-23')))))
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
  mutate(ShortDate = ifelse(Year %in% 2015:2016, 
                            as.Date(Date..LST., '%m/%d/%Y'),
                            as.Date(Date..LST., '%Y-%m-%d')),
         Week = week(as.Date(ShortDate, origin = '1970-01-01')),
         Hour_Bin = code_hours(Hour))

#remove missing data
table(air_q_all$QC.Name)

air_q_all <- subset(air_q_all, QC.Name != 'Missing')

#summarise the data
#grouping by year, month, and day, calculate:
#avg, median, 2.5% q, 97.5% q, 5% q, 95% q, sd

air_q_all %>%
  group_by(Year, Month, Day) %>%
  summarise(AvgPollution = mean(Value), 
            MedPollution = median(Value),
            SDPollution = sd(Value),
            Max = max(Value),
            Min = min(Value),
            Lower95 = quantile(Value, .025),
            Upper95 = quantile(Value, .975),
            Lower90 = quantile(Value, .05),
            Upper90 = quantile(Value, .10),
            Lower50 = quantile(Value, .25),
            Upper50 = quantile(Value, .75)) %>%
  ungroup() %>%
  mutate(Date = as.Date(paste0(Year, '-', Month, '-', Day))) -> air_q_all.byday

#group by year and week
air_q_all %>%
  group_by(Year, Week) %>%
  summarise(AvgPollution = mean(Value), 
            MedPollution = median(Value),
            SDPollution = sd(Value),
            Max = max(Value),
            Min = min(Value),
            Lower95 = quantile(Value, .025),
            Upper95 = quantile(Value, .975),
            Lower90 = quantile(Value, .05),
            Upper90 = quantile(Value, .10),
            Lower50 = quantile(Value, .25),
            Upper50 = quantile(Value, .75)) -> air_q_all.byweek


