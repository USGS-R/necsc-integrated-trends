#load and clean stream temp

library(dataRetrieval)
source('R/readWQPdataPaged.R')
source('R/wqp_cleanup_stream_temp_data.R')

mi = readWQPdataPaged(statecode='US:26', siteType='Stream', characteristicName=c('Temperature, water', 'Temperature', 'Temperature, sample'), startDateLo='1979-01-01', startDateHi='2020-01-01')
mn = readWQPdataPaged(statecode='US:27', siteType='Stream', characteristicName=c('Temperature, water', 'Temperature', 'Temperature, sample'), startDateLo='1979-01-01', startDateHi='2020-01-01')
wi = readWQPdataPaged(statecode='US:55', siteType='Stream', characteristicName=c('Temperature, water', 'Temperature', 'Temperature, sample'), startDateLo='1979-01-01', startDateHi='2020-01-01')

strtemp = rbind(mi, mn, wi)

strtemp.clean = wqp_cleanup_stream_temp_data(strtemp)

saveRDS(strtemp.clean, file = 'data/temperature_streams/stream_temperature_wqp.rds')
