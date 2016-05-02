
library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)

ldas_wind_debias = function(nldas_path, dbiased_path){
  
  
  nldas = read.csv(nldas_path, header=TRUE)
  nldas$time = as.POSIXct(nldas$time)
  
  after_2001 = nldas$time > as.POSIXct('2001-12-31')
  
  nldas$WindSpeed[after_2001] = nldas$WindSpeed[after_2001] * 0.921
  
  if(missing(dbiased_path)){
    return(nldas)
  }else{
    write.csv(nldas, dbiased_path, row.names=FALSE, quote=FALSE)
  }
}


run_lter_lake = function(site_id, lter_id, fixed_kd, kd_daynums=182:273, years = 1982:2012){
  
  
  run_dir = file.path(tempdir(), lter_id)
  dir.create(run_dir)
  ldas_wind_debias(get_driver_path(site_id), file.path(run_dir, 'nldas_driver.csv'))
  
  #some default parameters from kevin's run
  nml_args=list(
    dt=3600, subdaily=FALSE, nsave=24, 
    timezone=-6,
    csv_point_nlevs=0, 
    meteo_fl='nldas_driver.csv', 
    'min_layer_thick'=0.01)
  
  
  nml_obj = populate_base_lake_nml(site_id, kd=1, driver = 'nldas_driver.csv')
  
  nml_obj = set_nml(nml_obj, arg_list=nml_args)
  
  #get Kds and Ice on/off
  kds = read.table('lter_kd_water_clarity.csv', sep=',', header=TRUE, as.is=TRUE)
  kds = subset(kds, lakeid == lter_id)
  ice = read.table('north_temperate_lakes_lter__ice_duration_-_trout_lake_area.csv', sep=',', header=TRUE, as.is=TRUE)
  ice = subset(ice, lakeid == lter_id)
  
  
  ## write wtemp obs file
#  data(wtemp)
#   nhd_id = site_id
#   obs = filter(wtemp, site_id == nhd_id) %>%
#     transmute(DateTime=date, Depth=depth, temp=wtemp)
#   
#   #argh, drop crystal mix
#   if(site_id == 'nhd_13344284'){
#     obs = subset(obs, DateTime < as.POSIXct('2012-01-01'))
#   }
#   
#   #having a weird issue with resample_to_field, make unique
#   obs = obs[!duplicated(obs[,1:2]), ]
#   
#   write.table(obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
#   
  output = data.frame()
  
  for(y in years){
    
    ice_onoff = subset(ice, year==y)
    kd_year = subset(kds, year4==y & daynum %in% kd_daynums)
    
    nml_obj = set_nml(nml_obj, 'start', ice_onoff$datefirstopen)
    nml_obj = set_nml(nml_obj, 'stop', ice_onoff$datelastopen)
    nml_obj = set_nml(nml_obj, 'out_fn', paste0('output', y))
    
    if(missing(fixed_kd)){
      nml_obj = set_nml(nml_obj, 'Kw', median(kd_year$extcoef, na.rm=TRUE))
    }else{
      nml_obj = set_nml(nml_obj, 'Kw', fixed_kd)
    }
    
    write_nml(nml_obj, file = file.path(run_dir, 'glm2.nml'))
    
    run_glm(run_dir)
    
    output = rbind(output, 
                   resample_to_field(file.path(run_dir, paste0('output', y, '.nc')), 
                                     field_file = file.path(run_dir, 'obs.tsv')))
  }
  
  output$lakeid = lter_id
  
  return(output)
}


sp = 'nhd_13344210'
cr = 'nhd_13344284'
tr = 'nhd_13343906'
bm = 'nhd_13344170'
al = 'nhd_13344056'
tb = 'nhdh_69886158'

