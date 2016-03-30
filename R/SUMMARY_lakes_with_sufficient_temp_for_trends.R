
library(lakeattributes)
library(dplyr)
library(plyr)

wtemp$month = as.POSIXlt(wtemp$date)$mon + 1

events = unique(wtemp[,c('site_id', 'date')])
events$month = as.POSIXlt(events$date)$mon +1
events$year  = as.POSIXlt(events$date)$year + 1900

complete_year = ddply(events, c('site_id', 'year'), function(df){
	all(c(6,7,8,9,10) %in% df$month)
})

complete_year = subset(complete_year, V1)

tmp = ddply(complete_year, 'site_id', function(df){
	sum(df$year %in% 1980:2010)
}) %>% arrange(desc(V1)) 

#66% of 30 year cutoff
filter(tmp, V1 >= 20)

#O'Reilly et al
filter(tmp, V1 >= 13)