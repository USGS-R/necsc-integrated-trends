#Prep_lake_gltc_trends
#FIGURE_gltc_trend_uncertainty
# library(laketemps)
# library(zyp)
# library(plyr)
# library(lubridate)
# library(geoknife)
#source('R/get_prism_temps.R')

gltc_trend_calc = function(fout){
  
  wtr_long = subset(laketemps:::gltc_values, variable=='Lake.Temp.Summer.InSitu')
  air_long = subset(laketemps:::gltc_values, variable=='Air.Temp.Mean.Summer.CRU')
  
  get_prism_gltc = function(siteIDs){
    #  recordID variable year siteID value
    to_get = na.omit(subset(laketemps:::gltc_metadata, siteID %in% siteIDs & location=='United States')[, c('siteID', 'latitude', 'longitude')])
    
    to_get = subset(to_get, latitude < 50)
    temps = get_prism_temps(ids=as.character(to_get$siteID), lats=to_get$latitude, lons=to_get$longitude)
    temps$variable=NULL
    temps$statistic = NULL
    temps = melt(temps, id.vars = 'DateTime', variable.name='siteID')
    temps$month = month(temps$DateTime)
    temps$year  = year(temps$DateTime)
    temps$siteID = as.numeric(as.character(temps$siteID))
    
    jas_air = ddply(subset(temps, month %in% 7:9), c('siteID', 'year'), function(df){
      data.frame(value=mean(df$value, na.rm=TRUE))
    })
    return(jas_air)
  }
  
  prism_air = get_prism_gltc(unique(wtr_long$siteID))
  
  
  calc_airt_trend = function(df){
    data.frame(sen_airt=zyp.sen(value~year, df)$coeff[2])
  }
  
  #just subset to time period, don't remove missing years
  #prism_subset = merge(prism_air, wtr_long[,c('siteID', 'year')], by=c('siteID', 'year'))
  prism_subset = subset(prism_air, year %in% 1985:2009)
  
  #switch these to use PRISM/CRU
  air_trends = ddply(prism_subset, 'siteID', calc_airt_trend)
  #air_trends = ddply(air_long, 'siteID', calc_airt_trend)
  
  siteIDs = unique(wtr_long$siteID)
  nyear = rep(NA, length(siteIDs))
  cis = list()
  trend = rep(NA, length(siteIDs))
  for(i in 1:length(siteIDs)){
    
    tmp = subset(wtr_long, siteID == siteIDs[i])
    
    if(nrow(tmp) < 13){ #gltc cutoff
      cis[[i]] = matrix(c(NA, NA, NA,NA), nrow=2)
      next
    }
    trend[i] = zyp.sen(value~year, data=tmp)$coeff[2]
    
    cis[[i]] = confint(zyp.sen(value~year, data=tmp))
    nyear[i] = nrow(tmp)
  }
  
  lows = sapply(cis, function(x) x[2,1])
  highs = sapply(cis, function(x) x[2,2])
  ranges = highs-lows
  
  td = data.frame(siteID=siteIDs, trend, low=lows, high=highs, range=ranges)
  td = merge(td, air_trends, by='siteID')
  td = merge(laketemps:::gltc_metadata, td, by='siteID')
  td = na.omit(td[order(td$trend),])
  
  write.csv(td, 'data/trends/lakes_gltc_jas.csv', row.names=FALSE)

}