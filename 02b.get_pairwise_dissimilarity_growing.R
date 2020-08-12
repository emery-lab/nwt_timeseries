## Use this script to make pairwise psi values
## Load package 'distantia' 
library(distantia)
## load cleanish data
rm(list = ls())
sn.grow = read.csv("data/07.19.20.sennetdata_growing_season.csv")

## get the average soil moisture for each day, can do sampling time later
sn.grow$date = as.Date(sn.grow$date)
sn.grow_daily = as.data.frame(suppressWarnings(aggregate(sn.grow, by = list(sn.grow$date, sn.grow$sensornode), FUN = mean)))

sn.grow_daily = sn.grow_daily[, c("sensornode", "date","soiltemp_5cm_avg", "soiltemp_30cm_avg",  
                                  "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
                                  "soilmoisture_b_30cm_avg",  "soilmoisture_c_5cm_avg",  "soilmoisture_c_30cm_avg")]




## list sensor nodes / get unique values
sensor_nodes = unique(sn.grow$sensornode)
sensor_nodes = sensor_nodes[!is.na(sensor_nodes)]

include = c("sensornode", "soiltemp_5cm_avg", "soiltemp_30cm_avg", 
            "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
            "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg", "sample")


dates = seq(min(sn.grow$date), max(sn.grow$date), by = 1)
sample = 1:548
dates = as.data.frame(dates)
time.df = cbind(dates, sample)

sn.grow_daily.2 = merge(sn.grow_daily, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
table(sn.grow_daily.2$sample)


sn.compare = sn.grow_daily.2[, include]
sm.seq = prepareSequences(
  sequences = sn.compare,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)

sennet.psi = workflowPsiHP(
  sequences =  sm.seq,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

write.csv(sennet.psi, "data/08.12.20.sennetdissimilarity_growing.csv", row.names = FALSE)





