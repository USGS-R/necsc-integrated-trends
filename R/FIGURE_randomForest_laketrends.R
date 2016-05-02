#FIGURE_laketemps vs airtemps

library(laketemps)
library(randomForest)
library(zyp)

all_lakes = laketemps::get_lake_names()

all_wtr = lapply(all_lakes, get_surface_temps, type='Lake.Temp.Summer.InSitu')
bad_i = sapply(all_wtr, function(x) nrow(x) == 0)

#all_wtr[bad_i] = lapply(all_lakes[bad_i], get_surface_temps, type='Lake.Temp.Summer.Satellite')

all_airt = lapply(all_lakes, get_climate, type="Air.Temp.Mean.Summer.CRU")


calc_wtr_trend = function(df){

  if(nrow(df) > 13){
  names(df) = c('year', 'Lake.Temp.Summer.InSitu')
  data.frame(sen_wtr=zyp.sen(Lake.Temp.Summer.InSitu~year, df)$coeff[2])
  }else{
    return(data.frame(sen_wtr=NA))
  }
}

calc_airt_trend = function(df){
  data.frame(sen_wtr=zyp.sen(Air.Temp.Mean.Summer.CRU~year, df)$coeff[2])
}


trends = data.frame(sen_wtr=unlist(lapply(all_wtr, calc_wtr_trend)))
trends$sen_airt = unlist(lapply(all_airt, calc_airt_trend))
trends$Lake.name = all_lakes

trends_meta = merge(trends, get_metadata(trends$Lake.name))
trends_meta = subset(trends_meta, location=='United States')

trends_numeric = trends_meta[,sapply(trends_meta, is.numeric)]


lake.rf = randomForest(sen_wtr~., trends_numeric[,c('sen_wtr','sen_airt')], na.action = na.omit, importance=TRUE)
lake.rf
varImpPlot(lake.rf, type=1)

