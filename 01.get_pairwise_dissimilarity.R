## Use this script to make pairwise psi values
## Load package 'distantia' 
library(distantia)
## load cleanish data
load("data/06.28.20.sennetdata_cleanish.Rdata")

## get the average soil moisture for each day, can do sampling time later
sn.01_daily = as.data.frame(suppressWarnings(aggregate(sn.01, by = list(sn.01$date, sn.01$sensor), FUN = mean)))

## list sensor nodes / get unique values
sensor_nodes = unique(sn.01$sensornode)
sensor_nodes = sensor_nodes[!is.na(sensor_nodes)]

## get list of all the possible node pairs, then remove identical pairs i.e. 21-21
combos = expand.grid(sensor_nodes, sensor_nodes)
combos = combos[combos$Var1 != combos$Var2, ]
combos$Psi = NA # raw Psi column


## this loops through combos and generates the Psi value for eacch of the plot combos. This includes the soil moisture and temperature data (see script 00)
system.time( ## runs for like 11 minutes on my Mac (WR)
for(i in 1:nrow(combos)){
  print(i)
  tmp1 = sn.01_daily[sn.01_daily$sensornode == combos[i, "Var1"],] 
  tmp2 = sn.01_daily[sn.01_daily$sensornode == combos[i, "Var2"],] 
  
  tmp1 = tmp1[, 5:12]
  tmp2 = tmp2[, 5:12]
  
  AB.sequence = prepareSequences(sequence.A = tmp1, 
                                 sequence.A.name = paste("sensor", combos[i, "Var1"]),
                                 sequence.B = tmp2,
                                 sequence.B.name = paste("sensor", combos[i, "Var2"]),
                                 merge.mode = "complete",
                                 if.empty.cases = "omit")
  
  # AB.distance.matrix <- distanceMatrix(
  #   sequences = AB.sequence,
  #   method = "euclidean"
  # )
  # 
  combos[i, "Psi"] = workflowPsi(AB.sequence, grouping.column = 'id')[3]
  
})

colnames(combos) = c("first.sensor", "second.sensor", "Psi")
write.csv(combos, "data/06.28.20.sennetdissimilarity.csv", row.names = FALSE )

