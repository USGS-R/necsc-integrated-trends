#Prep_lake_gltc_trends
#FIGURE_gltc_trend_uncertainty
library(laketemps)
library(randomForest)
library(zyp)
library(plyr)

wtr_long = subset(laketemps:::gltc_values, variable=='Lake.Temp.Summer.InSitu')
air_long = subset(laketemps:::gltc_values, variable=='Air.Temp.Mean.Summer.CRU')

calc_airt_trend = function(df){
  data.frame(sen_airt=zyp.sen(value~year, df)$coeff[2])
}

air_trends = ddply(air_long, 'siteID', calc_airt_trend)


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
