#Summary lake trends
library(lme4)
library(dplyr)
library(lakeattributes)
library(ggmap)
library(lubridate)
library(leaflet)
source('R/sens_seasonal.R')

data(wtemp)
lake_d = wtemp
lake_d$yday = yday(lake_d$date)
lake_d$fdepth = floor(lake_d$depth)
lake_d$week = week(lake_d$date)
lake_d$month = month(lake_d$date)
lake_d = na.omit(lake_d)

#lmer(wtemp~year + (1|site_id) + (1|depth) + (1|yday), data = lake_d)
week_avg = ddply(lake_d, c('year', 'week', 'site_id', 'fdepth'), function(df){median(df$wtemp, na.rm=TRUE)})

site_week_summary = ddply(week_avg, c('week', 'site_id'), function(df){length(unique(df$year))})
tokeep = subset(site_week_summary, V1 > 2)

totrend = merge(week_avg, tokeep[,c('week', 'site_id')], all.y=TRUE)

lake_slopes = sens_seasonal_site(totrend$year, totrend$V1, totrend$week, paste0(totrend$site_id, ":", totrend$fdepth))

rm(lake_d)

ds_split = strsplit(as.character(lake_slopes$sites_i), ":")
lake_slopes$site_id = sapply(ds_split, function(x){x[1]})
lake_slopes$depth = sapply(ds_split, function(x){x[2]})
lake_slopes$depth = as.numeric(lake_slopes$depth)

lake_slopes$sites_i = NULL

## merge in lat/lon
lake_slopes_ll = merge(lake_slopes, location, all.x=TRUE)
rm(lake_slopes)
lake_slopes = lake_slopes_ll
rm(lake_slopes_ll)

slope_by_lake = ddply(subset(lake_slopes, depth <=2), 'site_id', function(df){median(df$slopes, na.rm=TRUE)})
slope_by_lake = merge(slope_by_lake, location)


# sbbox = make_bbox(lon = slope_by_lake$lon, lat = slope_by_lake$lat, f = .1)
# sq_map = get_map(sbbox, maptype='terrain')
# ggmap(sq_map) + geom_point(data = na.omit(slope_by_lake), mapping = aes(x = lon, y = lat, color=V1)) + scale_color_gradient(low='blue', high='red')


color.gradient <- function(x, colors=c("blue", 'white',"red"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

good_slopes = slope_by_lake
good_slopes$V1[is.infinite(good_slopes$V1) | is.na(good_slopes$V1)] = NA
good_slopes$V1[good_slopes$V1 < -1 | good_slopes$V1 > 1] = NA
good_slopes = na.omit(good_slopes)


m = leaflet() %>% addTiles() %>% 
  addCircleMarkers(lng=good_slopes$lon, lat=good_slopes$lat, fillOpacity=1,
                  fillColor=color.gradient(good_slopes$V1), radius=4, stroke=TRUE, weight=1,
                   popup=paste("ID:", good_slopes$site_id, '<br>Slope:', good_slopes$V1))

library(htmlwidgets)
saveWidget(m, file="laketrends.html")

