## Use this script to make interannual dissimilarity values (Psi still)
## Load package 'distantia' 
library(distantia)

# get rid of clutter
rm(list = ls())

## load season data for 2018 and 2019
sn.2018 = read.csv("data/07.19.20.sennetdata_2018.csv")
sn.2019 = read.csv("data/07.19.20.sennetdata_2019.csv")

## add a year column
sn.2018$season = 2018
sn.2019$season = 2019

## combine to one df
sn.grow = rbind(sn.2018, sn.2019)

## make a new grouping column,
sn.grow$new.id = paste(sn.grow$sensornode, sn.grow$season, sep = "-")
table(sn.grow$new.id)

## make a daily average
sn.grow$date = as.Date(sn.grow$date)
sn.grow_daily = as.data.frame(suppressWarnings(aggregate(sn.grow, by = list(sn.grow$date, sn.grow$new.id), FUN = mean)))


## list sensor nodes / get unique values
sensor_nodes = unique(sn.grow_daily$sensornode)
sensor_nodes = sensor_nodes[!is.na(sensor_nodes)]

## vector of include columns for later
include = c("Group.2", "soiltemp_5cm_avg", "soiltemp_30cm_avg", 
            "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
            "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg", "sample")

## give each entry a record, 
dates = seq(min(sn.grow_daily$date), max(sn.grow_daily$date), by = 1)
sample = 1:548
dates = as.data.frame(dates)
time.df = cbind(dates, sample)

sn.grow_daily.2 = merge(sn.grow_daily, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
table(sn.grow_daily.2$sample)

sn.compare = sn.grow_daily.2[, include]
sm.seq = prepareSequences(
  sequences = sn.compare,
  grouping.column = "Group.2",
  time.column = "sample",
  if.empty.cases = "omit"
)

sennet.psi = workflowPsiHP(
  sequences =  sm.seq,
  grouping.column = "Group.2",
  time.column = "sample",
  parallel.execution = TRUE
  
)

sennet.psi$first.sensor = NA
sennet.psi$second.sensor = NA

for(i in 1:nrow(sennet.psi)){
  sennet.psi$first.sensor[i] = unlist(strsplit(sennet.psi$A[i], split = "-"))[1]
  sennet.psi$second.sensor[i] = unlist(strsplit(sennet.psi$B[i], split = "-"))[1]
  
}

sennet.interannual = sennet.psi[sennet.psi$first.sensor == sennet.psi$second.sensor, ]

png("figures/interannual.png", width = 9, height = 5.5, units = "in", res = 400)
par(mfrow = c(1,1))
plot(sennet.interannual$first.sensor, 
     sennet.interannual$psi,
     xlab = "Sensor Node",
     ylab = "interannual dissimilarity (Psi)",
     main = "Interannual Node Dissimilarity",
     pch = 21,
     col = "dark blue",
     bg = "light blue",
     cex = 1.8,
     cex.lab = 1.2,
     cex.main = 1.5,
     xaxt = "n",
     bty = "l")
axis(1, c(6:21))
text(x = 17, y = 5, labels = 5.18)
dev.off()

sennet.interannual = sennet.interannual[order(sennet.interannual$first.sensor), c("first.sensor", "second.sensor", "psi")]
write.csv(sennet.interannual, "data/08.12.20.sennet_interannual_psi.csv", row.names = FALSE)
