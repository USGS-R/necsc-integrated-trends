#fetch_lake_metadata

fetch_lake_metadata = function(slopesin){

  load(slopesin)
  
  site_metadata = data.frame(site_id=unique(lake_slopes$site_id))
  rm(lake_slopes)
  gc()
  
  #lat/lon
  site_metadata = merge(site_metadata, lakeattributes::location, by='site_id', all.x = TRUE)
  
  #kd/secchi
  site_kd       = get_kd_avg(site_metadata$site_id, default.if.null=FALSE)
  site_kd$secchi = 1.7/site_kd$kd_avg
  site_metadata = merge(site_metadata, site_kd, by='site_id', all.x=TRUE)
  
  #zmax
  site_metadata$zmax = sapply(site_metadata$site_id, FUN=get_zmax)
  site_metadata$zmax[is.infinite(site_metadata$zmax)] = NA
  
  #lake area
  site_metadata = merge(site_metadata, lakeattributes::area, by='site_id', all.x = TRUE)
  
  
  write.csv(site_metadata, 'data/temperature_lakes/lake_metadata.csv', row.names=FALSE)  
}
