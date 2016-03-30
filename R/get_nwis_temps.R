
get_nwis_temps = function(nwis_ids){
  
  all_data = data.frame()
  
  for(i in 1:nrow(nwis_ids)){
    
    tryCatch({
      nwis_data = readNWISdv(nwis_ids$site_no[i], parameterCd="00010", startDate="1950-10-01", endDate="2016-12-01")
      
      all_data = rbind(all_data, nwis_data)
      
    }, 
    error=function(e){warning(e$message)})
  }
  
  return(all_data)
}
