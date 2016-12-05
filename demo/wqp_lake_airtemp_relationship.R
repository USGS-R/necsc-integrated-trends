##wqp_lake_airtemp_relationship

# this retrieves and compares the inter-annual air temperature changes 

library(lakeattributes)
library(lubridate)
library(lme4)
library(reshape2)
source('R/sens_seasonal.R')
source('R/get_daymet_temps.R')

season = 'month'

data(wtemp)

temp = wtemp

if(season == 'month'){
  temp$season_i = month(temp$date)
}else if(season == 'week'){
  temp$season_i = week(temp$date)
}else{
  stop('specify season')
}

temp$year = year(temp$date)

temp = subset(temp, depth < 2)

site_month_summary = ddply(temp, c('season_i', 'site_id'), function(df){length(unique(df$year))})

tokeep = subset(dplyr::arrange(site_month_summary, desc(V1)), V1 > 2)

long_sites = merge(temp, tokeep[,c('season_i', 'site_id')], all.y=TRUE)

sites_season_avg = ddply(long_sites, c('year', 'season_i', 'site_id'), function(df){
  data.frame(temp = mean(df$wtemp, na.rm=TRUE))
})

sub_meta = subset(location, site_id %in% tokeep$site_id)

##get air temperature data
site_airt = get_daymet_temps(sub_meta$site_id, sub_meta$lat, sub_meta$lon)

#airt_long = melt(site_airt, id.vars = 'DateTime', variable.name = 'site_id', value.name = 'temp')
airt_long = site_airt

airt_long$temp = (airt_long$tmax..deg.c. + airt_long$tmin..deg.c.)/2
airt_long$DateTime = ISOdate(airt_long$year, 1, 1, 0, 0, 0) + as.difftime(airt_long$yday-1, units='days')
airt_long$month = month(airt_long$DateTime)
airt_long$year  = year(airt_long$DateTime)



if(season == 'month'){
  airt_long$season_i = month(airt_long$DateTime)
}else if(season == 'week'){
  airt_long$season_i = week(airt_long$DateTime)
}else{
  stop('specify season')
}

airt_long$season_i = month(airt_long$DateTime)
airt_long$year  = year(airt_long$DateTime)


wtr_air_slopes = pair_interannual_variability(sites_season_avg, airt_long)

#save for posterity
write.table(wtr_air_slopes, 'data/trends/wqp_lake_air_wtr_slopes.tsv', sep='\t', row.names=FALSE)

median(wtr_air_slopes$slope_ratio, na.rm=TRUE)

median(wtr_air_slopes$slope_wminusa, na.rm=TRUE)
mean(wtr_air_slopes$slope_wminusa, na.rm=TRUE)
