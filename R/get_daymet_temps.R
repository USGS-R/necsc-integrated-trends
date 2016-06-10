#get_prism_temps(lats, lons)
library(geoknife)
#options c('tmax', 'tmin', 'tmean')
get_daymet_temps = function(ids, lats, lons, var='tmax'){
  
  fabric <- webdata()
  
  url(fabric) <- 'http://thredds.daac.ornl.gov/thredds/dodsC/daymet-agg/daymet-agg.ncml'
  #url(fabric) <- 'http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/cru/hadcrut3/var/var.time.stat.nc'
  variables(fabric) <- var
  query(fabric, 'times')
  
  times(fabric) <- as.POSIXct(c('1980-01-01', '2016-01-01'))
  
  
  locations = data.frame(row.names=c('longitude', 'latitude'))
  
  for(i in 1:length(ids)){
    locations[c('longitude', 'latitude'),ids[i]] = c(lons[i], lats[i])
  }
  
  stencil = simplegeom(locations)
  
  job = geoknife(stencil, fabric, wait = TRUE)
  
  result(job)
}
