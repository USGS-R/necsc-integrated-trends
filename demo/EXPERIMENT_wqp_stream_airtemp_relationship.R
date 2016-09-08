
library(lubridate)
library(lme4)
library(reshape2)
source('R/sens_seasonal.R')
source('R/get_prism_temps.R')

temp = readRDS('data/temperature_streams/stream_temperature_wqp.rds')
meta = read.table('data/temperature_streams/metadata_extended.tsv', sep='\t', header=TRUE, as.is=TRUE, comment.char = '', quote='')
meta$X.MonitoringLocationIdentifier. = gsub('\"', '', meta$X.MonitoringLocationIdentifier.)

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
temp$month = month(temp$ActivityStartDateTime)
temp$year = year(temp$ActivityStartDateTime)

## some figures
# boxplot(ResultMeasureValue~week, temp)
# hist(temp$week)
# hist(temp$year)
# 
# 
# stream_lmer = lmer(ResultMeasureValue~year+(1|week)+(1|MonitoringLocationIdentifier), temp)
# 
# stream_lmer
# summary(stream_lmer)

site_conv = as.factor(temp$MonitoringLocationIdentifier)
id_lookup = unique(data.frame(id_num = as.numeric(site_conv), id_name = as.character(site_conv)))

temp$numer_id = as.numeric(site_conv)


site_month_summary = ddply(temp, c('month', 'MonitoringLocationIdentifier'), function(df){length(unique(df$year))})

tokeep = subset(dplyr::arrange(site_month_summary, desc(V1)), V1 > 2)


long_sites = merge(temp, tokeep[,c('month', 'MonitoringLocationIdentifier')], all.y=TRUE)

sites_month_avg = ddply(long_sites, c('year', 'month', 'MonitoringLocationIdentifier'), function(df){
  mean(df$ResultMeasureValue, na.rm=TRUE)
})

sub_meta = subset(meta, X.MonitoringLocationIdentifier. %in% tokeep$MonitoringLocationIdentifier)

site_airt = get_prism_temps(sub_meta$X.MonitoringLocationIdentifier., sub_meta$X.LatitudeMeasure., sub_meta$X.LongitudeMeasure.)

airt_long = melt(site_airt, id.vars = 'DateTime', variable.name = 'MonitoringLocationIdentifier', value.name = 'airtemp')
airt_long$month = month(airt_long$DateTime)
airt_long$year  = year(airt_long$DateTime)
air_wtr_month$airtemp = as.numeric(air_wtr_month$airtemp)
##get air temperature data

air_wtr_month = merge(subset(sites_month_avg, V1 > 0), airt_long, by=c('MonitoringLocationIdentifier', 'month', 'year'))

wtr_slopes = sens_seasonal_site(times=air_wtr_month$year, data=air_wtr_month$V1, season_i=air_wtr_month$month, sites_i=air_wtr_month$MonitoringLocationIdentifier)
air_slopes = sens_seasonal_site(times=air_wtr_month$year, data=air_wtr_month$airtemp, season_i=air_wtr_month$month, sites_i=air_wtr_month$MonitoringLocationIdentifier)
names(wtr_slopes) = c("sites_i","season_i","wtrslopes","start","end","dt","n.obs", "wtravgvals" )
names(air_slopes) = c("sites_i","season_i","airslopes","start","end","dt","n.obs", "airavgvals" )

air_wtr_slopes = merge(air_slopes, wtr_slopes, by=c('sites_i', 'season_i', 'start', 'end'))
air_wtr_slopes$slope_ratio = air_wtr_slopes$wtrslopes/air_wtr_slopes$airslopes
air_wtr_slopes$slope_wminusa = air_wtr_slopes$wtrslopes-air_wtr_slopes$airslopes

#save for posterity
write.table(air_wtr_slopes, 'data/trends/wqp_stream_air_wtr_slopes.tsv', sep='\t', row.names=FALSE)


median(air_wtr_slopes$slope_ratio, na.rm=TRUE)

median(air_wtr_slopes$slope_wminusa, na.rm=TRUE)
mean(air_wtr_slopes$slope_wminusa, na.rm=TRUE)

boxplot(slope_wminusa~season_i, air_wtr_slopes, ylim=c(-0.2,0.2), ylab='water slope - air slope', xlab='month')
abline(h=0)

boxplot(wtrslopes~season_i, air_wtr_slopes, ylim=c(-0.2,0.2), ylab='water slope - air slope', xlab='month')
abline(h=0)

boxplot(airslopes~season_i, air_wtr_slopes, ylim=c(-0.2,0.2), ylab='water slope - air slope', xlab='month')
abline(h=0)




