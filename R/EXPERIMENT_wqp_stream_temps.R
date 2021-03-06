
library(lubridate)
library(lme4)
source('R/sens_seasonal.R')

temp = readRDS('data/temperature_streams/stream_temperature_wqp.rds')

temp = temp[, c('OrganizationIdentifier', 'ActivityStartDateTime', 'ResultMeasure.MeasureUnitCode', 'ActivityMediaSubdivisionName',  'CharacteristicName', 'ResultMeasureValue', 'MonitoringLocationIdentifier')]
gc()

#taking only identifiable data with units
temp = subset(temp, ResultMeasure.MeasureUnitCode %in% c('deg C', 'deg F'))

temp$ResultMeasureValue[temp$ResultMeasure.MeasureUnitCode == 'deg F'] = (temp$ResultMeasureValue[temp$ResultMeasure.MeasureUnitCode == 'deg F'] - 32)/1.8
temp$ResultMeasure.MeasureUnitCode = 'deg C'

temp = subset(temp, ResultMeasureValue < 40)
temp = subset(temp, CharacteristicName == 'Temperature, water')
temp$CharacteristicName = NULL

temp$week = week(temp$ActivityStartDateTime)
temp$year = year(temp$ActivityStartDateTime)

## some figures
boxplot(ResultMeasureValue~week, temp)
hist(temp$week)
hist(temp$year)


stream_lmer = lmer(ResultMeasureValue~year+(1|week)+(1|MonitoringLocationIdentifier), temp)

stream_lmer
summary(stream_lmer)

site_conv = as.factor(temp$MonitoringLocationIdentifier)
id_lookup = unique(data.frame(id_num = as.numeric(site_conv), id_name = as.character(site_conv)))

temp$numer_id = as.numeric(site_conv)


all_slopes = sens_seasonal_site(times=temp$year, data=temp$ResultMeasureValue, season_i=temp$week, sites_i=temp$numer_id)

median(all_slopes$slopes, na.rm=TRUE)






