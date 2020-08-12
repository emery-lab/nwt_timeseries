## Use this script to make pairwise psi values
## Load package 'distantia' 
library(distantia)
## load cleanish data
sn.01 = read.csv("data/07.08.20.sennetdata_cleanish.csv", stringsAsFactors = F)

# this function is good for viewing the soil moisture at 5 and 30cm for all three nodes. Good to check if they are potentially switched.
plot.raw.sm = function(data = sn.01, sensor = 6){
  sensor.tp = sn.01[sn.01$sensornode %in% sensor, ]
  sensor.tp$date = as.POSIXct(sensor.tp$date, format = "%Y-%m-%d %H:%M:%S")
  
  par(mfrow = c(1, 3))
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_a_5cm_avg, 
       type = "p", 
       col = "light blue",
       cex = 0.1,
       pch = 19,
       ylim = c(0, 1),
       main = "subnode-A",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_a_30cm_avg, 
         col = "blue",
         cex = 0.1,
         pch = 19)
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_b_5cm_avg, 
       type = "p", 
       col = "pink",
       cex = 0.1,
       pch = 19,
       ylim = c(0, 1),
       main = "subnode-B",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_b_30cm_avg, 
         col = "red",
         cex = 0.1,
         pch = 19)
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_c_5cm_avg, 
       type = "p", 
       col = "light green",
       cex = 0.1,
       pch = 19,
       ylim = c(0, 1),
       main = "subnode-C",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_c_30cm_avg, 
         col = "dark green",
         cex = 0.1,
         pch = 19)
  
    
}

# check on out. 
plot.raw.sm(sensor = 9)


## get the average soil moisture for each day, can do sampling time later
sn.01$TIMESTAMP = as.POSIXct(sn.01$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
sn.01$date = as.Date(sn.01$date)
sn.01_daily = as.data.frame(suppressWarnings(aggregate(sn.01, by = list(sn.01$date, sn.01$sensor), FUN = mean)))

## list sensor nodes / get unique values
sensor_nodes = unique(sn.01$sensornode)
sensor_nodes = sensor_nodes[!is.na(sensor_nodes)]

include = c("sensornode",  "soiltemp_5cm_avg", "soiltemp_30cm_avg", 
            "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
            "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg", "sample")


dates = seq(min(sn.01$date), max(sn.01$date), by = 1)
sample = 1:883
dates = as.data.frame(dates)
time.df = cbind(dates, sample)

sn.01_daily.2 = merge(sn.01_daily, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
table(sn.01_daily.2$sample)


sn.compare = sn.01_daily.2[, include]
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

write.csv(all.psi, "data/08.12.20.sennetdissimilarity.csv", row.names = FALSE)

# compare to old (wrong) way
old.psi = read.csv("data/06.28.20.sennetdissimilarity.csv")

table(sennet.psi$A)
colnames(sennet.psi) = c("first.sensor", "second.sensor", "new.psi")
table(old.psi$first.sensor)

all.psi = merge(sennet.psi, old.psi)
table(all.psi$first.sensor)
table(all.psi$second.sensor)
par(mfrow = c(1,1))
plot(all.psi$first.sensor, 
     all.psi$second.sensor, 
     xaxt = "n", 
     yaxt = "n", 
     cex = abs(all.psi$new.psi - all.psi$Psi) * 0.1)
axis(1, c(6:21))
axis(2, c(6:21))
abline(0, 1)

## this takes literal years to run, idk when I'll do this. 
sennet.null.psi = workflowNullPsiHP(
  sequences =  sm.seq,
  grouping.column = "sensornode",
  time.column = "sample",
  repetitions = 999,
  parallel.execution = TRUE
  
)



##################################
########### FORSAKEN #############
##################################

## get list of all the possible node pairs, then remove identical pairs i.e. 21-21
# combos = expand.grid(sensor_nodes, sensor_nodes)
# combos = combos[combos$Var1 != combos$Var2, ]
# combos$Psi = NA # raw Psi column


## this loops through combos and generates the Psi value for eacch of the plot combos. This includes the soil moisture and temperature data (see script 00)
# system.time( ## runs for like 11 minutes on my Mac (WR)
# for(i in 1:nrow(combos)){
#   print(i)
#   tmp1 = sn.01_daily[sn.01_daily$sensornode == combos[i, "Var1"],] 
#   tmp2 = sn.01_daily[sn.01_daily$sensornode == combos[i, "Var2"],] 
#   
#   tmp1 = tmp1[, 5:12]
#   tmp2 = tmp2[, 5:12]
#   
#   AB.sequence = prepareSequences(sequence.A = tmp1, 
#                                  sequence.A.name = paste("sensor", combos[i, "Var1"]),
#                                  sequence.B = tmp2,
#                                  sequence.B.name = paste("sensor", combos[i, "Var2"]),
#                                  merge.mode = "complete",
#                                  if.empty.cases = "omit")
#   
#   # AB.distance.matrix <- distanceMatrix(
#   #   sequences = AB.sequence,
#   #   method = "euclidean"
#   # )
#   # 
#   combos[i, "Psi"] = workflowPsi(AB.sequence, grouping.column = 'id')[3]
#   
# })
# 
# colnames(combos) = c("first.sensor", "second.sensor", "Psi")
# # write.csv(combos, "data/06.28.20.sennetdissimilarity.csv", row.names = FALSE ) # uncomment to re-write these (cleaner/more data)
# 
# 
# system.time( ## runs for like 11 minutes on my Mac (WR)
#   for(i in 1:nrow(combos)){
#     print(i)
#     tmp1 = sn.01_daily[sn.01_daily$sensornode == combos[i, "Var1"],] 
#     tmp2 = sn.01_daily[sn.01_daily$sensornode == combos[i, "Var2"],] 
#     
#     tmp1 = tmp1[, 5:12]
#     tmp2 = tmp2[, 5:12]
#     
#     AB.sequence = prepareSequences(sequence.A = tmp1, 
#                                    sequence.A.name = paste("sensor", combos[i, "Var1"]),
#                                    sequence.B = tmp2,
#                                    sequence.B.name = paste("sensor", combos[i, "Var2"]),
#                                    merge.mode = "complete",
#                                    if.empty.cases = "omit")
#     
#     
#   })



