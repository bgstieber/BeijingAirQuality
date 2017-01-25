source('prepdata.R')

# longer time series plot using rolling average

ggplot(air_q_all.byday, aes(x = Date, y = RollingAvg))+
  geom_point()+
  ylab(expression(paste('3 Day Rolling Average of  ',
                        PM[2.5],'  Concentration')))+
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


#density plots comparing all year data to years in facet

ggplot(air_q_all.byday, aes(x = RollingAvg))+
  geom_density(data = transform(air_q_all.byday, Year = NULL), 
               aes(colour = 'All Data'), bw = 'SJ')+
  geom_density(aes(colour = 'Year in Facet'), bw = 'SJ')+
  facet_wrap(~Year)+
  xlab(expression(paste('3 Day Rolling Average of  ',
                        PM[2.5],'  Concentration (',
                        log[2], ' scale)')))+
  scale_x_continuous(trans = log_trans(2))+
  scale_colour_brewer(palette = 'Set1',
                      name = 'Data Type')

#density plots comparing all data to months in facet

ggplot(air_q_all.byday, aes(x = Month, y = RollingAvg))+
  geom_violin(aes(group = Month),
              draw_quantiles = c(.25, .5, .75))+
  scale_y_continuous(trans = log_trans(2))+
  facet_wrap(~Year)


ggplot(air_q_all.byday, aes(x = RollingAvg))+
  geom_density(data = transform(air_q_all.byday, Month = NULL), 
               aes(colour = 'All Data'), bw = 'SJ')+
  geom_density(aes(colour = 'Month in Facet'), bw = 'SJ')+
  facet_wrap(~Month)+
  xlab(expression(paste('3 Day Rolling Average of  ',
                        PM[2.5],'  Concentration (',
                        log[2], ' scale)')))+
  scale_x_continuous(trans = log_trans(2))+
  scale_colour_brewer(palette = 'Set1',
                      name = 'Data Type')