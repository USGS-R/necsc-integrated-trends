
library(lme4)
library(dplyr)

stream_d = read.csv('data/temperature_streams/all_data.csv', as.is=TRUE)




#Overall stream temp trends
lmer(temp~year+(1|yday)+(1|site_no), data = stream_d)


