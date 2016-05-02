
get_stream_ids = function(state_codes=dataRetrieval::stateCd$STUSAB[1:51], min_years=30, Access="3"){
  
  
  all_sites = data.frame()
  
  for(i in 1:length(state_codes)){
    
    
    x <- retry({readNWISdata(stateCd=state_codes[i], service = "site", siteType="ST", hasDataTypeCd='dv', 
                      parameterCd="00010", seriesCatalogOutput=TRUE, Access=Access)})
    
    
    
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



#taken without shame from http://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error
#library(futile.logger)
#library(utils)

retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(retval)
}

#library(dplyr)
