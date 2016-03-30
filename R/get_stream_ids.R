
get_stream_ids = function(state_codes=c('WI', 'MI', 'MN'), min_years=30){
  
  
  all_sites = data.frame()
  
  for(i in 1:length(state_codes)){
    
    x <- readNWISdata(stateCd=state_codes[i], service = "site", siteType="ST", hasDataTypeCd='dv', 
                      parameterCd="00010", seriesCatalogOutput=TRUE)
    
    summaryX <- x %>% filter(parm_cd == "00010") %>%
      mutate(begin_date = as.POSIXct(begin_date),
             end_date = as.POSIXct(end_date),
             range = end_date - begin_date) %>%
              filter(range >= min_years*365*24*60*60)  %>% 
              filter(data_type_cd == 'dv')
    
    all_sites = rbind(all_sites, 
                      summaryX)
    cat(i,'\n')
  }
  return(all_sites)
  #write.table(all_sites,'all_sites_conus.tsv', sep='\t', row.names=FALSE)
}




#library(dplyr)
