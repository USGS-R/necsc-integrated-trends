library(lubridate)

lake_d = read.csv('data/temperature_lakes/lake_obs.csv', as.is=TRUE)
metadata = read.csv('data/temperature_lakes/lake_metadata.csv', as.is=TRUE)

lake_d$yday = yday(lake_d$date)
lake_d$month = month(lake_d$date)
lake_d$week  = week(lake_d$date)
lake_d = na.omit(lake_d)

# lets take all the lakes/depths/weeks for the first three years (1980, 1981) and randomly resample relative to that

start_point = subset(lake_d, year %in% c(1980:1981))

#for each subsequent year randomly sample wtemp at the same site, yday, and depth

all_out = data.frame()

for(y in 1982:2015){
  year_d = subset(lake_d, year==y)
  new_i = sample(1:nrow(year_d), size = nrow(start_point), replace=TRUE)
  
  all_out = rbind(all_out, year_d[new_i, ])
  
  
#   for(i in 1:nrow(start_point)){
#     
#     #really what I should do is get matching rows 
#     
#     
#     
#  }
}

calc_slopes = function(lake_d){
  source('R/sens_seasonal.R')
  lake_d$yday = yday(lake_d$date)
  lake_d$month = month(lake_d$date)
  lake_d$week  = week(lake_d$date)
  lake_d = na.omit(lake_d)
  
  #lmer(wtemp~year + (1|site_id) + (1|depth) + (1|yday), data = lake_d)
  
  lake_slopes = sens_seasonal_site(lake_d$year, lake_d$wtemp, lake_d$month, paste0(lake_d$site_id, ":", lake_d$depth))
  
  rm(lake_d)
  
  ds_split = strsplit(as.character(lake_slopes$sites_i), ":")
  lake_slopes$site_id = sapply(ds_split, function(x){x[1]})
  lake_slopes$depth = sapply(ds_split, function(x){x[2]})
  lake_slopes$depth = as.numeric(lake_slopes$depth)
  
  lake_slopes$sites_i = NULL
  
  lake_slopes = subset(lake_slopes, !is.na(slopes) & !is.infinite(slopes))
  return(lake_slopes)
}

lake_slopes = calc_slopes(all_out)


#overall
median(lake_slopes$slopes)

#shallow
median(subset(lake_slopes, depth <=2)$slopes)

#median lake size (are we biasing small?)
median(subset(metadata, site_id %in% unique(lake_d$site_id))$area_m2)
median(subset(metadata, site_id %in% unique(lake_slopes$site_id))$area_m2)


