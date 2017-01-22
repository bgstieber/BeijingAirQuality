library(tidyverse)
theme_set(theme_bw())

#read all csvs

years <- 2010:2015
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
                   air_q_2015)

#remove missing data
table(air_q_all$QC.Name)
# Missing   Valid 
# 2155   50429 

air_q_all <- subset(air_q_all, QC.Name != 'Missing')

#summarise the data
#grouping by year, month, and day, calculate:
#avg, median, 2.5% q, 97.5% q, 5% q, 95% q, sd

air_q_all %>%
  group_by(Year, Month, Day) %>%
  summarise(AvgPollution = mean(Value), 
            MedPollution = median(Value),
            SDPollution = sd(Value),
            Lower95 = quantile(Value, .025),
            Upper95 = quantile(Value, .975),
            Lower90 = quantile(Value, .05),
            Upper90 = quantile(Value, .10),
            Lower50 = quantile(Value, .25),
            Upper50 = quantile(Value, .75)) %>%
  ungroup() %>%
  mutate(Date = as.Date(paste0(Year, '-', Month, '-', Day))) -> air_q_all.byday