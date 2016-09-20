#get_prism_temps(lats, lons)
library(geoknife)
get_prism_temps = function(ids, lats, lons){
  
  fabric <- webdata()
  
  url(fabric) <- 'http://cida.usgs.gov/thredds/dodsC/prism'
  #url(fabric) <- 'http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/cru/hadcrut3/var/var.time.stat.nc'
  variables(fabric) <- 'tmx'
  #query(fabric, 'times')
  
  times(fabric) <- as.POSIXct(c('1950-01-01', '2016-01-01'))
  
  
  locations = data.frame(row.names=c('longitude', 'latitude'))
  
  for(i in 1:length(ids)){
    locations[c('longitude', 'latitude'),ids[i]] = c(lons[i], lats[i])
  }
  
  stencil = simplegeom(locations)
  
  job = geoknife(stencil, fabric, wait = TRUE)
  
  result(job)
}
