suppress <- TRUE

if(suppress){
  suppressPackageStartupMessages(source('R/prepdata.R'))  
}else{
  source('R/prepdata.R')
}



pm_label <- function(log2 = FALSE){
  if(log2){
    expression(paste('3 Day Rolling Average of  ',
                     PM[2.5],'  Concentration (',
                     log[2], ' scale)'))
  }else{
    expression(paste('3 Day Rolling Average of  ',
                     PM[2.5],'  Concentration'))
  }
}

# longer time series plot using rolling average

ggplot(air_q_all.byday, aes(x = Date, y = RollingAvgPollution))+
  geom_point()+
  ylab(pm_label())+
  ggtitle(expression(
    paste('Beijing Air Quality: Daily ',
          PM[2.5],
          ' Concentration')),
    subtitle = expression(
      paste(
        PM[2.5], ' concentration measured in ',
        mu, 'g/', m^3
      )
    ))

# add weekday and season to data
air_q_all.byday <- air_q_all.byday %>%
  mutate(DayOfWeek = weekdays(Date),
         Season = ifelse(Month %in% c(12,1,2), 'Winter',
                         ifelse(Month %in% 3:5, 'Spring',
                                ifelse(Month %in% 6:8, 'Summer','Fall')))
         ) %>%
  mutate(DayOfWeek = factor(DayOfWeek, 
                            levels = c('Saturday','Sunday',
                                       'Monday','Tuesday',
                                       'Wednesday','Thursday',
                                       'Friday')),
         Season = factor(Season,
                         levels = c('Spring','Summer',
                                    'Fall','Winter')))


#density plots comparing all year data to years in facet

ggplot(air_q_all.byday, aes(x = RollingAvgPollution))+
  geom_density(data = transform(air_q_all.byday, Year = NULL), 
               aes(colour = 'All Data'), bw = 'SJ')+
  geom_density(aes(colour = 'Year in Facet'), bw = 'SJ')+
  facet_wrap(~Year)+
  xlab(pm_label(log2 = TRUE))+
  scale_x_continuous(trans = log_trans(2))+
  scale_colour_brewer(palette = 'Set1',
                      name = 'Data Type')

#violin plots comparing all data to months in facet

ggplot(air_q_all.byday, aes(x = Month, y = RollingAvgPollution))+
  geom_violin(aes(group = Month),
              draw_quantiles = c(.25, .5, .75))+
  scale_y_continuous(trans = log_trans(2))+
  facet_wrap(~Year)+
  ylab(pm_label(log2 = T))+
  scale_x_continuous(breaks = c(1,seq(3, 12, by = 3)))+
  ggtitle(expression(paste(
    'Violin Plots of ', PM[2.5], ' Concentration by Month'
  )))+
  stat_summary(fun.y = 'mean', geom = 'point', size = 3, 
               colour = 'dodgerblue3')


# ggplot(air_q_all.byday, aes(x = RollingAvg))+
#   geom_density(data = transform(air_q_all.byday, Month = NULL), 
#                aes(colour = 'All Data'), bw = 'SJ')+
#   geom_density(aes(colour = 'Month in Facet'), bw = 'SJ')+
#   facet_wrap(~Month)+
#   xlab(expression(paste('3 Day Rolling Average of  ',
#                         PM[2.5],'  Concentration (',
#                         log[2], ' scale)')))+
#   scale_x_continuous(trans = log_trans(2))+
#   scale_colour_brewer(palette = 'Set1',
#                       name = 'Data Type')

##look at wind speed now

ggplot(air_q_all.byday, aes(x = MeanWindSpeedMPH, y = AvgPollution))+
  geom_boxplot(aes(group = MeanWindSpeedMPH))+
  geom_jitter(alpha = .1, colour = 'dodgerblue2')+
  scale_y_continuous(trans = log_trans(2))+
  facet_wrap(~Year)+
  ylab(pm_label(log2 = TRUE))+
  xlab('Average Daily Wind Speed (mph)')

#create data.frame with error bar coordinates

air_q_all.byday %>%
  group_by(DayOfWeek, Season) %>%
  summarise(Upper50Med = quantile(MedPollution, .75),
            Lower50Med = quantile(MedPollution, .25),
            Upper90Med = quantile(MedPollution, .95),
            Lower90Med = quantile(AvgPollution, .05)) -> error_bar_sum

merge(air_q_all.byday,
      error_bar_sum,
      by = c('DayOfWeek','Season')) %>%
  filter(MedPollution >= Upper90Med |
         MedPollution <= Lower90Med) -> jitter_sum

#story 1
# does it being a work week affect pollution?
# 
ggplot(data = air_q_all.byday,
       aes(x = DayOfWeek, y = MedPollution))+
  geom_point(data = jitter_sum,
              aes(x = DayOfWeek, y = MedPollution),
              pch = 1,
               alpha = .5)+
  geom_errorbar(data = error_bar_sum,
                aes(x = DayOfWeek,
                    ymin = Lower90Med,
                    ymax = Upper90Med),
                size = 1.2, colour = 'grey50',
                inherit.aes = FALSE)+
  geom_errorbar(data = error_bar_sum,
                aes(x = DayOfWeek,
                    ymin = Lower50Med,
                    ymax = Upper50Med),
                size = 1.2, inherit.aes = FALSE)+

  stat_summary(fun.y = 'median', geom = 'point', colour = '#b70101',
               size = 4)+
  facet_wrap(~Season, nrow = 1)+
  scale_y_continuous(trans = log_trans(2),
                     name = expression(paste('Median ', PM[2.5],
                                             ' Concentration (',
                                             log[2], ' scale)')))+
  xlab('')+
  scale_x_discrete(labels = c('Sunday' = 'Sun',
                              'Monday' = 'Mon',
                              'Tuesday' = 'Tue',
                              'Wednesday' = 'Wed',
                              'Thursday' = 'Thu',
                              'Friday' = 'Fri',
                              'Saturday' = 'Sat'))+
  ggtitle('Median PM2.5 Concentration by Day of Week and Season',
          subtitle = 'Data for 2011 - 2016, y-axis is log-scaled')

anova(
  lm(I(log(MedPollution)) ~ Season * DayOfWeek, data = air_q_all.byday))


ggplot(data = air_q_all.byday,
       aes(x = Season, y = MedPollution))+
  geom_point(data = jitter_sum,
             aes(x = Season, y = MedPollution),
             pch = 1,
             alpha = .5)+
  geom_errorbar(data = error_bar_sum,
                aes(x = Season,
                    ymin = Lower50Med,
                    ymax = Upper50Med),
                size = 1.2, inherit.aes = FALSE)+
  geom_errorbar(data = error_bar_sum,
                aes(x = Season,
                    ymin = Lower90Med,
                    ymax = Upper90Med),
                size = 1.2, linetype = 'dashed',
                inherit.aes = FALSE)+
  stat_summary(fun.y = 'mean', geom = 'point', colour = '#b70101',
               size = 4)+
  facet_wrap(~DayOfWeek, nrow = 1)+
  scale_y_continuous(trans = log_trans(2),
                     name = expression(paste('Median ', PM[2.5],
                                             ' Concentration (',
                                             log[2], ' scale)')))+
  xlab('')




#manually generate a paired scatter plot
library(gridExtra)
library(GGally)

cor_table <- air_q_all.byday %>%
  group_by(Season) %>%
  summarise(CorPolHum = cor(log2(RollingAvgPollution), MeanHumid_RollingAvg,
                            use = 'pairwise'),
            CorPolWS = cor(log2(RollingAvgPollution), log2(MeanWindSpeed_RollingAvg),
                           use = 'pairwise'),
            CorPolTemp = cor(log2(RollingAvgPollution), MeanTempF_RollingAvg,
                             use = 'pairwise'))

p02 <- ggplot(cor_table,
              aes(x = 1, y = Season, 
                  label = 
                    paste0(Season, ': ', round(CorPolHum,2))))+
  geom_text(aes(colour = Season), size = 6)+
  # geom_text(aes(x = 1, y = 4.25, 
  #               label = 'Correlations between\nlog2(Pollution) and Humidity:'), 
  #           size = 4, inherit.aes = FALSE)+
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

p03 <- ggplot(cor_table,
              aes(x = 1, y = Season, 
                  label = 
                    paste0(Season, ': ', round(CorPolWS,2))))+
  geom_text(aes(colour = Season), size = 6)+
  # geom_text(aes(x = 1, y = 4.25, 
  #               label = 'Correlations between log2(Pollution) and log2(Wind Speed):'), 
  #           size = 6, inherit.aes = FALSE)+
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

p04 <- ggplot(cor_table,
              aes(x = 1, y = Season, 
                  label = 
                    paste0(Season, ': ', round(CorPolTemp,2))))+
  geom_text(aes(colour = Season), size = 6)+
  # geom_text(aes(x = 1, y = 4.25, 
  #               label = 'Correlations between log2(Pollution) and Temperature:'), 
  #           size = 6, inherit.aes = FALSE)+
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

#histogram
p01 <- ggplot(air_q_all.byday, aes(x = RollingAvgPollution))+
  geom_histogram(fill = 'grey60', colour = 'black',
                 bins = 50)+
  scale_x_continuous(trans = log_trans(2))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

p12 <- ggplot(air_q_all.byday, aes(x = MeanHumid_RollingAvg))+
  geom_histogram(fill = 'grey60', colour = 'black')

p23 <- ggplot(air_q_all.byday, aes(x = MeanWindSpeed_RollingAvg))+
  geom_histogram(fill = 'grey60', colour = 'black')+
  scale_x_continuous(trans = log_trans(2))

p34 <- ggplot(air_q_all.byday, aes(x = MeanTempF_RollingAvg))+
  geom_histogram(fill = 'grey60', colour = 'black')


#pollution on x axis (column 1, rows 1-3)
p11 <- ggplot(air_q_all.byday, aes(x = RollingAvgPollution, y = MeanHumid_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')+
  scale_x_continuous(trans = log_trans(2))+
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1, pch = 15)))

p21  <- ggplot(air_q_all.byday, aes(x = RollingAvgPollution, 
                                    y = MeanWindSpeed_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')+
  scale_x_continuous(trans = log_trans(2))+
  scale_y_continuous(trans = log_trans(2))

p31 <- ggplot(air_q_all.byday, aes(x = RollingAvgPollution, y = MeanTempF_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')+
  scale_x_continuous(trans = log_trans(2))


#humidity on x axis, (column 2, rows0, 2, 3)

# p02 <- ggplot(air_q_all.byday, aes(y = RollingAvgPollution, x = MeanHumid_RollingAvg))+
#   geom_point(aes(colour = Season), pch = 1, alpha = .8)+
#   scale_color_brewer(palette = 'Set1')+
#   scale_y_continuous(trans = log_trans(2))+
#   guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1, pch = 15)))

p22 <- ggplot(air_q_all.byday, aes(x = MeanHumid_RollingAvg, 
                                   y = MeanWindSpeed_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')+
  scale_y_continuous(trans = log_trans(2))


p32 <- ggplot(air_q_all.byday, aes(x = MeanHumid_RollingAvg, y = MeanTempF_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')


# wind speed on x axis(column 3, rows 0,1,3)

# p03 <- ggplot(air_q_all.byday, aes(y = RollingAvgPollution, 
#                                    x = MeanWindSpeed_RollingAvg))+
#   geom_point(aes(colour = Season), pch = 1, alpha = .8)+
#   scale_color_brewer(palette = 'Set1')+
#   scale_x_continuous(trans = log_trans(2))+
#   scale_y_continuous(trans = log_trans(2))

p13 <- ggplot(air_q_all.byday, aes(x = MeanWindSpeed_RollingAvg, 
                                   y = MeanHumid_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')+
  scale_x_continuous(trans = log_trans(2))

p33 <- ggplot(air_q_all.byday, aes(x = MeanWindSpeed_RollingAvg, 
                                   y = MeanTempF_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')+
  scale_x_continuous(trans = log_trans(2))

#temp on x axis (column 4, rows 0 - 3)

# p04 <- ggplot(air_q_all.byday, aes(y = RollingAvgPollution, 
#                                    x = MeanTempF_RollingAvg))+
#   geom_point(aes(colour = Season), pch = 1, alpha = .8)+
#   scale_color_brewer(palette = 'Set1')+
#   scale_y_continuous(trans = log_trans(2))
# 


p14 <- ggplot(air_q_all.byday, aes(y = MeanHumid_RollingAvg, 
                                   x = MeanTempF_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')

p24 <- ggplot(air_q_all.byday, aes(y = MeanWindSpeed_RollingAvg, x = MeanTempF_RollingAvg))+
  geom_point(aes(colour = Season), pch = 1, alpha = .8)+
  scale_color_brewer(palette = 'Set1')+
  scale_y_continuous(trans = log_trans(2))



ggmatrix(list(
  p01,
 # p02, NULL, NULL,
  p02, p03, p04,
  p11, p12, p13, p14,
  p21, p22, p23, p24,
  p31, p32, p33, p34
), nrow = 4, ncol = 4, legend = c(2,1), showStrips = TRUE,
xAxisLabels = 
  c('Pollution [PM2.5] (log base 2)',
    'Humidiity [%]',
    'Wind Speed [MPH] (log base 2)',
    'Temperature [F]'
  ))+
  theme(legend.position = 'top',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


#fit loess to get predictions

l_2016 <- loess(MeanTemperatureF ~ DayOfYear, data = air_q_all.byday,
      subset = Year == 2016, span = .4)

l_2016.predict <- predict(l_2016, newdata = data.frame(DayOfYear = 1:365))

l_11_15 <- loess(MeanTemperatureF ~ DayOfYear, data = air_q_all.byday,
                 subset = Year != 2016, span = .4)

l_11_15.predict <- predict(l_11_15, newdata = data.frame(DayOfYear = 1:365))


p1 <- ggplot(air_q_all.byday, aes(x = DayOfYear, y = MeanTemperatureF, 
                            colour = factor(Year)))+
  scale_colour_manual(values = c('2011' = '#d9d9d9',
                                 '2012' = '#bdbdbd',
                                 '2013' = '#969696',
                                 '2014' = '#737373',
                                 '2015' = '#525252',
                                 '2016' = 'red'),
                      name = 'Year')+
  stat_smooth(se = FALSE, span = .4, method = 'loess', 
              n = 200, size = 1.2)+
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb,
    name = 'Date'
  )+
  scale_y_continuous(limits = c(0, 90),
                     name = 'Temperature (F)')+
  guides(colour = guide_legend(nrow = 1))+theme(legend.position = 'top')+
  ggtitle('Smoothed Line Plots for Daily Average Temperature by Year',
          subtitle = 'Smoothed lines created using loess (alpha = 0.4)')+
  theme(legend.spacing = unit(0.1, 'lines'))




p2 <- ggplot()+
  geom_smooth(data = subset(air_q_all.byday, Year == 2016),
              aes(x = DayOfYear, y = MeanTemperatureF,
                  colour = '2016', fill = '2016'),
              span = .4, method = 'loess',
              level = .99, size = 1.2,
              n = 200, alpha = .3)+
  geom_smooth(data = subset(air_q_all.byday, Year != 2016),
              aes(x = DayOfYear, y = MeanTemperatureF,
                  colour = '2011-2015', fill = '2011-2015'),
              span = .4, method = 'loess',
              level = .99, size = 1.2,
              n = 200, alpha = .3)+
  scale_colour_manual(values = c('2016' = '#E41A1C',
                               '2011-2015' = '#377EB8'),
                    name = 'Year Group')+
  scale_fill_manual(values = c('2016' = '#E41A1C',
                               '2011-2015' = '#377EB8'),
                    name = 'Year Group')+
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = month.abb,
    name = 'Date'
  )+
  scale_y_continuous(limits = c(0, 90),
                     name = 'Temperature (F)')+
  guides(colour = guide_legend(nrow = 1),
         fill = guide_legend(nrow = 1))+theme(legend.position = 'top')+
  ggtitle('Smoothed Line Plots for Daily Average Temperature by Year Group',
          subtitle = paste('There were',
                           sum(l_2016.predict > l_11_15.predict),
                           'days where 2016 predicted values were higher than',
                           'those predicted for the 2011-2015 trend'))

air_q_all.byday %>%
  group_by(Year, Season) %>%
  summarise(`Temperature (F)` = mean(MeanTemperatureF),
            `Pollution (PM2.5)` = mean(AvgPollution),
            `Humidity (%)` = mean(MeanHumidity),
            `Wind Speed (MPH)` = mean(MeanWindSpeedMPH)) -> par_coord_air


library(MASS)
parcoord(par_coord_air[,c(3:6)], 
         col = dput(brewer_pal(palette = 'Set1')(4))[par_coord_air$Season], 
         var.label = TRUE, ylim = c(0, 1.2), lwd = 2,
         main = NULL)
title(main = 'Parallel Coordinates Plot of Seasonal Averages', adj = 0)
mtext(side = 3, line = 0, adj = 0,
      text = 'Each line represents the average of the daily values for each season for a given year')

legend('topleft', c('Spring','Summer','Fall','Winter'),
       col = brewer_pal(palette = 'Set1')(4),
       lty = 1, lwd = 4, ncol = 4, bty = 'n')
   