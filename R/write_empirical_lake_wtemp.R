##write_empirical_lake_wtemp
#write this every time as we have no indication when lakeattributes will be updated

write_wtemp = function(fout){
  
  data(wtemp)
  write.csv(wtemp, fout, row.names=FALSE)

}
