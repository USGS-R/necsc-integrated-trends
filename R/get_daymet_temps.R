#get_prism_temps(lats, lons)
library(httr)
#options c('tmax', 'tmin', 'tmean')
get_daymet_temps = function(ids, lats, lons){
  
  all_data = list()
  pb = txtProgressBar(min=0, max=length(lats))
  for(i in 1:length(lats)){
    
    year_range = paste(seq(1980, 2015, by = 1), collapse = ",")
    download_string = sprintf("https://daymet.ornl.gov/data/send/saveData?lat=%s&lon=%s&measuredParams=tmax,tmin,dayl,prcp,srad,swe,vp&year=%s", 
                            lats[i], lons[i], year_range)
    
    tryCatch({
      r = RETRY('GET', download_string)
      data_as_csv = content(r, 'text', encoding = 'UTF-8')
      pseudo_file = textConnection(data_as_csv)
      
      all_data[[i]] = read.csv(pseudo_file, header=TRUE, skip=7, as.is=TRUE)
      close(pseudo_file)
      all_data[[i]]$site_id = ids[i]
      
    }, error=function(e){
      print(e)
    })
    
    setTxtProgressBar(pb, i)
  }
  
  return(do.call(rbind, all_data))
}
