
library(lubridate)
library(lme4)
source('R/sens_seasonal.R')

temp = readRDS('data/temperature_streams/stream_temperature_wqp.rds')
meta = read.table('data/temperature_streams/metadata_extended.tsv', sep='\t', header=TRUE, as.is=TRUE, comment.char = '', quote='')

meta$X.MonitoringLocationIdentifier. = gsub('\"', "", meta$X.MonitoringLocationIdentifier.)

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

site_week_summary = ddply(temp, c('week', 'MonitoringLocationIdentifier'), function(df){length(unique(df$year))})

tokeep = subset(dplyr::arrange(site_week_summary, desc(V1)), V1 > 2)


long_sites = merge(temp, tokeep[,c('week', 'MonitoringLocationIdentifier')], all.y=TRUE)

long_sites = subset(long_sites, ResultMeasureValue > 0)

sites_week_avg = ddply(long_sites, c('year', 'week', 'MonitoringLocationIdentifier'), function(df){
  mean(df$ResultMeasureValue, na.rm=TRUE)
})

all_slopes = sens_seasonal_site(times=sites_week_avg$year, data=sites_week_avg$V1, season_i=sites_week_avg$week, sites_i=sites_week_avg$MonitoringLocationIdentifier)

median(all_slopes$slopes, na.rm=TRUE)
hist(all_slopes$slopes, breaks=200)
boxplot(slopes~season_i, all_slopes, ylim=c(-0.2,0.2))
abline(h=0)



##experimental stuff

slope_so = merge(meta, all_slopes, by.y='sites_i', by.x='X.MonitoringLocationIdentifier.')
boxplot(slopes~X.StreamOrde., slope_so, ylim=c(-0.2,0.2), xlab='Stream Order', ylab='Trend')
abline(h=0)
boxplot(slopes~floor(X.QA_01./100), slope_so, ylim=c(-0.2,0.2))

hist(slope_so$slopes, breaks=200, xlim=c(-5,5))

boxplot(slopes~dt, slope_so, ylim=c(-0.2,0.2))
abline(h=0)

boxplot(slopes~floor(X.MAXELEVSMO./100), slope_so, ylim=c(-0.2,0.2))

boxplot(slopes~n.obs, slope_so, ylim=c(-0.2,0.2))


boxplot(slopes~end, slope_so, ylim=c(-0.2, 0.2))

temp_so = merge(meta[, c('MonitoringLocationIdentifier', 'X.StreamOrde.')], sites_week_avg, by.y='MonitoringLocationIdentifier', by.x='MonitoringLocationIdentifier')

boxplot(V1~X.StreamOrde., temp_so, ylim=c(10,20))



nobs = table(sites_week_avg$MonitoringLocationIdentifier)
keep = nobs[nobs > 100]


evensamp = data.frame()
for(y in 1979:2014){
  
  hmm = subset(temp_so, year==y)
  evensamp = rbind(evensamp, hmm[sample(1:nrow(hmm), size = 800), ])
}

all_slopes = sens_seasonal_site(times=evensamp$year, data=evensamp$V1, season_i=evensamp$week, sites_i=evensamp$MonitoringLocationIdentifier)

median(all_slopes$slopes, na.rm=TRUE)
hist(all_slopes$slopes, breaks=200)
boxplot(slopes~season_i, all_slopes, ylim=c(-0.2,0.2))
abline(h=0)


trend_so = merge(meta, all_slopes, by.y='sites_i', by.x='MonitoringLocationIdentifier')

