nwis_save_data = function(d, fname){
  nwis_temps = d
  save(nwis_temps, file=fname)
}