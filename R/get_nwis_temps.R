
get_nwis_temps = function(nwis_ids){
  
  all_data = data.frame()
  
  for(i in 1:nrow(nwis_ids)){
    
    cat(sprintf('%.2f %% done getting stream temps\n', 100*i/nrow(nwis_ids)))
    nwis_data = retry({readNWISdv(nwis_ids$site_no[i], parameterCd="00010", startDate="1950-10-01", endDate="2016-12-01")})
    
    tryCatch({
      all_data = rbind(all_data, nwis_data)
    }, error=function(e){warning('probably weirdo columns on this site.')})
    
  }
  
  return(all_data)
}
