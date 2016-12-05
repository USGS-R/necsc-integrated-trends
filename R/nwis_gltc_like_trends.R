
nwis_gltc_like_trends = function(nwis_temps, fout){
#nwis temps
#load('data/temperature_streams/long_nwis_data.Rdata')

    nwis_temps$year = year(nwis_temps$Date)
    nwis_temps$month = month(nwis_temps$Date)

    ################################################
    ##grab site metadata
    ################################################
    site_metadata = do.call(rbind, lapply(unique(nwis_temps$site_no), readNWISsite))
    #drop AK sites
    site_metadata = subset(site_metadata, dec_lat_va < 50)


    prism_airt = get_prism_temps(site_metadata$site_no, site_metadata$dec_lat_va, site_metadata$dec_long_va)

    long_prism = melt(prism_airt, id.vars=c('DateTime'), variable.name='site_no', value.name='airt')
    long_prism$month = month(long_prism$DateTime)
    long_prism$year  = year(long_prism$DateTime)
    long_prism$airt  = as.numeric(long_prism$airt)

    jas_airt = ddply(long_prism, c('site_no', 'year'), function(df){
      data.frame(jas_airt=mean(subset(df ,month %in% 7:9)$airt, na.rm=TRUE), year_airt=mean(df$airt, na.rm=TRUE))
    })

    jas_avgs = ddply(nwis_temps, c('site_no', 'year'), function(df){
      data.frame(jas_wtr=mean(subset(df ,month %in% 7:9)$X_00010_00003, na.rm=TRUE), year_wtr=mean(df$X_00010_00003, na.rm=TRUE))
    })

    ##merge in airtemps here
    jas_merged = merge(jas_airt, jas_avgs, all.y=TRUE)
    jas_merged = jas_merged[jas_merged$year %in% 1985:2009,]

    site_trends = ddply(jas_merged, c('site_no'), function(df){
      
      nwtr = sum(!is.na(df$jas_wtr))
      rwtr = range(df$year[!is.na(df$jas_wtr)])
      
      if(diff(rwtr) < 13){ #|| (nwtr/diff(rwtr) < 0.9)){ #must have 90% of years of water data
        return(data.frame())
      }
      
      data.frame(sen_airt=zyp.sen(jas_airt~year, df)$coeff[2], 
                 sen_year_airt=zyp.sen(year_airt~year, df)$coeff[2], 
                 sen_wtr=zyp.sen(jas_wtr~year, df)$coeff[2],
                 sen_year_wtr=zyp.sen(year_wtr~year, df)$coeff[2], 
                 dt=diff(range(df$year)))
    })



    trend_and_meta = merge(site_trends, site_metadata[,c('site_no','dec_lat_va', 'dec_long_va')])

    median(trend_and_meta$sen_wtr)

    write.csv(trend_and_meta, file = fout, row.names=FALSE)
}


