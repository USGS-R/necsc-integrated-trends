#build_lake_summary

build_lake_summary = function(infile, outfile){
  

  
  rmarkdown::render(infile, output_file=basename(outfile), 
                    output_dir=dirname(outfile))
  
  
}