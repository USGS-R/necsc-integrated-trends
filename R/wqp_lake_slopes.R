#calculate_lake_slopes

wqp_lake_slopes = function(infile){
  
  lake_d = read.csv(infile)
  
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
  
  save(lake_slopes, file='data/temperature_lakes/slope_population.Rdata')
  
}