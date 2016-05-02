library(leaflet)

leaflet(data=subset(us_sites, latitude < 50)) %>% addTiles() %>% addCircles(lng=~longitude, lat=~latitude)


leaflet(data=site_metadata) %>% addTiles() %>% addCircles(lng=~dec_long_va, lat=~dec_lat_va)



nwis_clean = subset(nwis_more, !is.na(LAT_CENT))
nwis_clean = merge(nwis_clean, site_metadata[,c('site_no', 'dec_lat_va', 'dec_long_va')])
nwis_clean = subset(nwis_clean, !is.na(dec_lat_va))


l = leaflet(data=nwis_clean) %>% addProviderTiles('Esri.WorldImagery') %>% addCircles(lng=~dec_long_va, lat=~dec_lat_va, popup=paste(nwis_clean$sen_wtr, nwis_clean$site_no, sep='<br>'))# %>% addPopups(lng=~dec_long_va, lat=~dec_lat_va, popup=paste(nwis_clean$sen_wtr, nwis_clean$site_no, sep='<br>'))

for(i in 1:nrow(nwis_clean)){
  
  l %>% setView(lat=nwis_clean$dec_lat_va[i], lng=nwis_clean$dec_long_va[i], zoom=12)
  
  
}


wtf = c('10351650', '10351700')

plot(jas_wtr~year, subset(jas_avgs, site_no==wtf[2]))

