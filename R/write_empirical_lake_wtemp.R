##write_empirical_lake_wtemp
#write this every time as we have no indication when lakeattributes will be updated

write_wtemp = function(fout){
  
  
  write.csv(lakeattributes::wtemp, fout, row.names=FALSE)

}
