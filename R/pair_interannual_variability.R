#' @title Pair inter-annual variability common between two datasets
#' 
#' @param dataset1 Data.frame with columns year, season_i, temp, site_i
#' @param dataset2 Data.frame with columns year, season_i, temp, site_i
#' 
#' 
pair_interannual_variability = function(dataset1, dataset2){
  
  dataset1 = plyr::rename(dataset1, replace = c('temp'='temp1'))
  dataset2 = plyr::rename(dataset2, replace = c('temp'='temp2'))
  
  all_data_merged = merge(dataset1, dataset2, by=c('site_id', 'season_i', 'year'))
  all_data_merged$temp1 = as.numeric(all_data_merged$temp1)
  all_data_merged$temp2 = as.numeric(all_data_merged$temp2)
  
  slopes1 = sens_seasonal_site(times=all_data_merged$year, data=all_data_merged$temp1, season_i=all_data_merged$season_i, sites_i=all_data_merged$site_id)
  slopes2 = sens_seasonal_site(times=all_data_merged$year, data=all_data_merged$temp2, season_i=all_data_merged$season_i, sites_i=all_data_merged$site_id)
  
  names(slopes1) = c("sites_i","season_i","slope1","start","end","dt","n.obs", "avgvals1" )
  names(slopes2) = c("sites_i","season_i","slope2","start","end","dt","n.obs", "avgvals2" )
  
  air_wtr_slopes = merge(slopes1, slopes2, by=c('sites_i', 'season_i', 'start', 'end'))
  air_wtr_slopes$slope_ratio = air_wtr_slopes$slope1/air_wtr_slopes$slope2
  air_wtr_slopes$slope_1min2 = air_wtr_slopes$slope1-air_wtr_slopes$slope2
}
