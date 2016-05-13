## snap all WQP stream data to NHD+ flowlines

library(maptools)
library(dataRetrieval)
library(parallel)
library(raster)
source('R/readWQPdataPaged.R')

c1 = parallel::makePSOCKcluster(paste('licon', 1:50), manual=TRUE, port=4046)
parallel::clusterEvalQ(c1, {install.packages(c('rgeos', 'maptools', 'httr'), repos='http://cran.rstudio.com')})
parallel::clusterEvalQ(c1, {library(maptools); library(rgeos); library(httr);})

driver_url = paste0('http://', (Sys.info()["nodename"]),':4041/')
clusterExport(c1, varlist = 'driver_url')

load('d:/flowlines.Rdata')
#wqp_streamtemp = readRDS('data/temperature_streams/stream_temperature_wqp.rds')

meta = whatWQPsites(statecode='US:27', siteType='Stream', characteristicName=c('Temperature, water', 'Temperature', 'Temperature, sample'))
meta = rbind(meta, whatWQPsites(statecode='US:26', siteType='Stream', characteristicName=c('Temperature, water', 'Temperature', 'Temperature, sample')))
meta = rbind(meta, whatWQPsites(statecode='US:55', siteType='Stream', characteristicName=c('Temperature, water', 'Temperature', 'Temperature, sample')))

meta = as.data.frame(meta)

#sum(strtemp$MonitoringLocationIdentifier %in% meta$MonitoringLocationIdentifier)
#nrow(strtemp)

#get rid of bad points, maybe remove later, or maybe just clip US
meta = subset(meta, LatitudeMeasure > 39 & LongitudeMeasure < 0)

site_points = SpatialPointsDataFrame(meta[,c('LongitudeMeasure', 'LatitudeMeasure')], meta, match.ID=FALSE, proj4string=flowlines@proj4string)


#lets try cropping first, otherwise this snap seems too damn slow
sub_fl = crop(flowlines, extent(site_points))



point_mapping = list()
#full length blows up, cut it apart
subsets = base::split(1:nrow(site_points), floor(1:nrow(site_points)/50))

#clusterExport(c1, 'sub_fl')
saveRDS(sub_fl, 'd:/WiLMA_drivers/sub_fl.rds')

clusterEvalQ(c1, {
  fname = tempfile()
  httr::GET(paste0(driver_url, 'sub_fl.rds'), httr::write_disk(fname))
  sub_fl <<- readRDS(fname)
  return(1)
})


snap = function(index, site_points){
  maptools::snapPointsToLines(site_points[index, ], sub_fl, idField='COMID')
}

res = clusterApplyLB(c1, x=subsets, fun=snap, site_points=site_points)


# for(i in 1:length(subsets)){
#   cat(i, '\n')
#   point_mapping[[i]] = maptools::snapPointsToLines(site_points[subsets[[i]], ], sub_fl, idField='COMID')
# }

point_mapping = do.call(rbind, res)

extended_metadata = merge(point_mapping@data, sub_fl@data, by.x='nearest_line_id', by.y='COMID')

write.table(extended_metadata, 'data/temperature_streams/metadata_extended.tsv', sep='\t', row.names=FALSE)


