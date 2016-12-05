
library(randomForest)
library(reshape2)
library(plyr)
library(lubridate)
library(zyp)
library(dataRetrieval)
library(geoknife)
source('R/get_prism_temps.R')


basins = read.table('data/basinsII/conterm_hydro.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character'))
basins = merge(basins, read.table('data/basinsII/conterm_lc06_mains100.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_topo.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_soils.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_climate.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_flowrec.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_geology.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_regions.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_lc_crops.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_bas_morph.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_pop_infrastr.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))
basins = merge(basins, read.table('data/basinsII/conterm_lc06_basin.txt', sep=',', header=TRUE, as.is=TRUE, colClasses=c(STAID='character')))

#don't know where this empty col came from
basins$X = NULL

#nwis temps
load('data/temperature_streams/long_nwis_data.Rdata')

nwis_temps$year = year(nwis_temps$Date)
nwis_temps$month = month(nwis_temps$Date)

################################################
##grab airtemp data quick
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



site_trends = ddply(jas_merged, c('site_no'), function(df){
  
  nwtr = sum(!is.na(df$jas_wtr))
  rwtr = range(df$year[!is.na(df$jas_wtr)])
  
  if(diff(rwtr) < 20 || (nwtr/diff(rwtr) < 0.9)){ #must have 90% of years of water data
    return(data.frame())
  }
  data.frame(sen_airt=zyp.sen(jas_airt~year, df)$coeff[2], 
             sen_year_airt=zyp.sen(year_airt~year, df)$coeff[2], 
             sen_wtr=zyp.sen(jas_wtr~year, df)$coeff[2],
             sen_year_wtr=zyp.sen(year_wtr~year, df)$coeff[2], 
             dt=diff(range(df$year)))
})

# ###############################################################################
# #merge in airtemps
# ###############################################################################
# site_metadata = do.call(rbind, lapply(unique(site_trends$site_no), readNWISsite))
# #drop AK sites
# site_metadata = subset(site_metadata, dec_lat_va < 50)
# 
# 
# # library(nclimdivr)
# # library(sp)
# # site_metadata$CLIMDIV = climdiv_id_by_geo(site_metadata$dec_lat_va, site_metadata$dec_long_va)
# # airt = get_nclimdiv_var(site_metadata$CLIMDIV, 'tmp')
# 
# prism_airt = get_prism_temps(site_metadata$site_no, site_metadata$dec_lat_va, site_metadata$dec_long_va)
# 
# 
# long_prism = melt(prism_airt, id.vars=c('DateTime'), variable.name='site_no', value.name='airt')
# long_prism$month = month(long_prism$DateTime)
# long_prism$year  = year(long_prism$DateTime)
# long_prism$airt  = as.numeric(long_prism$airt)
# 
# 
# jas_airt = ddply(long_prism, c('site_no', 'year'), function(df){
#   mean(subset(df ,month %in% 7:9)$airt, na.rm=TRUE)
# })
# 
# airt_trends = ddply(na.omit(long_prism), c('site_no', 'month'), function(df){
#   if(nrow(df) < 10){
#     return(data.frame())
#   }
#   
#   data.frame(linear=lm(airt~year, df)$coeff[2], sen=zyp.sen(airt~year, df)$coeff[2], dt=diff(range(df$year)))
# })
# 
# airt_trends = dcast(airt_trends, site_no~month, mean, value.var='sen')

###############################################################################
## Model stream temp trends from all these metrics
###############################################################################
#names(site_trends)[names(site_trends) == 'sen'] = 'sen_wtr'
#names(airt_trends)[names(airt_trends) == 'sen'] = 'sen_airt'
#names(airt_trends)[names(airt_trends) != 'site_no'] = paste0('airt_mon_', names(airt_trends)[names(airt_trends) != 'site_no'])
#airt_trends$dt = NULL
#airt_trends$linear = NULL
#airt_trends = merge(airt_trends, site_metadata[, c('site_no', 'CLIMDIV')])
#site_trends = merge(site_trends, airt_trends)
#site_trends$linear = NULL

nwis_more = merge(site_trends, basins, by.x='site_no', by.y='STAID', all.x=TRUE)

nwis_more$linear = NULL

tmp = nwis_more$site_no
nwis_numeric = nwis_more[,unlist(lapply(nwis_more, function(x){all(is.numeric(x))}))]
nwis_numeric$site_no = as.character(tmp)
nwis_numeric$CLIMDIV = NULL
nwis_numeric$dt = NULL

#nwis_numeric$sen_airt = NULL
randomForest(sen_wtr~., data=nwis_numeric, na.action=na.omit)
varImpPlot(randomForest(sen_wtr~., data=nwis_numeric, na.action=na.omit, importance=TRUE), type=1)

