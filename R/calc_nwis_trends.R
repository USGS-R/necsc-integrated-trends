

library(plyr)
library(lubridate)
library(zyp)

load('data/temperature_streams/long_nwis_data.Rdata')

nwis_temps$year = year(nwis_temps$Date)
nwis_temps$month = month(nwis_temps$Date)


jas_avgs = ddply(nwis_temps, c('site_no', 'year'), function(df){
  mean(subset(df ,month %in% 6:8)$X_00010_00003, na.rm=TRUE)
  
})

ann_avgs = ddply(nwis_temps,  c('site_no', 'year'), function(df){
  mean(df$X_00010_00003, na.rm=TRUE)
})

jas_trends = ddply(jas_avgs, c('site_no'), function(df){
  if(nrow(df) < 25){
    return(data.frame())
  }
  data.frame(linear=lm(V1~year, df)$coeff[2], sen=zyp.sen(V1~year, df)$coeff[2], dt=diff(range(df$year)))
})


ann_trends = ddply(ann_avgs, c('site_no'), function(df){
  if(nrow(df) < 25){
    return(data.frame())
  }
  data.frame(linear=lm(V1~year, df)$coeff[2], sen=zyp.sen(V1~year, df)$coeff[2], dt=diff(range(df$year)))
})



long_prism = melt(prism_airt, id.vars=c('DateTime'), variable.name='site_no', value.name='airt')
long_prism$month = month(long_prism$DateTime)
long_prism$year  = year(long_prism$DateTime)
long_prism$airt  = as.numeric(long_prism$airt)

jas_airt = ddply(long_prism, c('site_no', 'year'), function(df){
  mean(subset(df ,month %in% 6:8)$airt, na.rm=TRUE)
})

airt_trends = ddply(na.omit(jas_airt), c('site_no'), function(df){
  if(nrow(df) < 10){
    return(data.frame())
  }
  
  data.frame(linear=lm(V1~year, df)$coeff[2], sen=zyp.sen(V1~year, df)$coeff[2], dt=diff(range(df$year)))
})


##stream trends
tmp = merge(airt_trends, site_trends, by='site_no')

plot(tmp$linear.x, tmp$linear.y, ylab='wtr trend', xlab='airt trend', ylim=c(-0.1,0.1), xlim=c(-0.1, 0.1), main='stream vs airt')
abline(h=0)
abline(v=0)

plot(tmp$linear.x, tmp$linear.y, ylab='wtr trend', xlab='airt trend')
plot(tmp$sen.x, tmp$sen.y, ylab='wtr trend', xlab='airt trend')



##lake trends
temps = lapply(get_lake_names(), FUN=function(x,...){tmp=get_surface_temps(x,...);return(tmp)}, type='Lake.Temp.Summer.InSitu')

for(i in 1:length(temps)){
  if(nrow(temps[[i]]) > 0){
    temps[[i]]$site_name = get_lake_names()[i]
  }
  
  if(nrow(temps[[i]]) < 20){
    temps[[i]] = data.frame()
  }
}

all_temps = do.call(rbind, temps)

lake_trends = ddply(all_temps, 'site_name', function(df){
  data.frame(linear=lm(Lake.Temp.Summer.InSitu~year, df)$coeff[2], sen=zyp.sen(Lake.Temp.Summer.InSitu~year, df)$coeff[2], dt=diff(range(df$year)))
})


temps = lapply(get_lake_names(), FUN=function(x,...){tmp=get_climate(x,...);return(tmp)}, type='Air.Temp.Mean.Summer.CRU')

for(i in 1:length(temps)){
  if(nrow(temps[[i]]) > 0){
    temps[[i]]$site_name = get_lake_names()[i]
  }
  
  if(nrow(temps[[i]]) < 20){
    temps[[i]] = data.frame()
  }
}

all_temps = do.call(rbind, temps)

lake_airt_trends = ddply(all_temps, 'site_name', function(df){
  data.frame(linear=lm(Air.Temp.Mean.Summer.CRU~year, df)$coeff[2], sen=zyp.sen(Air.Temp.Mean.Summer.CRU~year, df)$coeff[2], dt=diff(range(df$year)))
})



lake_tmp = merge(lake_trends, lake_airt_trends, by='site_name')

png('output/stream_lake_vs_airt.png', width = 2500, height=2500, res=300)
plot(lake_tmp$linear.x, lake_tmp$linear.y, ylab='wtr trend', xlab='airt trend', ylim=c(-0.2,0.2), xlim=c(-0.2, 0.2), main='lake vs airt', col='blue')
points(tmp$linear.x, tmp$linear.y, col='green')
legend('bottomright', legend = c('Lake', 'Stream'), col = c('Blue', 'Green'), pch=1)
abline(h=0)
abline(v=0)

dev.off()
