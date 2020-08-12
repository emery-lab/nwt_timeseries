## Use this script to make pairwise psi values
## Load package 'distantia' 
library(distantia)
## load cleanish data
sn.grow = read.csv("data/07.19.20.sennetdata_growing_season.csv")

## get the average soil moisture for each day, can do sampling time later
sn.grow$date = as.Date(sn.grow$date)
sn.grow_daily = as.data.frame(suppressWarnings(aggregate(sn.grow, by = list(sn.grow$date, sn.grow$sensornode), FUN = mean)))

sn.grow_daily = sn.grow_daily[, c("sensornode", "soiltemp_5cm_avg", "soiltemp_30cm_avg",  
                                  "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
                                  "soilmoisture_b_30cm_avg",  "soilmoisture_c_5cm_avg",  "soilmoisture_c_30cm_avg")]




## list sensor nodes / get unique values
sensor_nodes = unique(sn.grow$sensornode)
sensor_nodes = sensor_nodes[!is.na(sensor_nodes)]

## get list of all the possible node pairs, then remove identical pairs i.e. 21-21
combos = expand.grid(sensor_nodes, sensor_nodes)
combos = combos[combos$Var1 != combos$Var2, ]
combos = combos[combos$Var1 != 18 & combos$Var1 != 1 & combos$Var2 != 18 & combos$Var2 != 1,]
combos$Psi = NA # raw Psi column




## this loops through combos and generates the Psi value for eacch of the plot combos. This includes the soil moisture and temperature data (see script 00)
system.time( ## runs for like 11 minutes on my Mac (WR)
  for(i in 1:nrow(combos)){
    print(i)
    tmp1 = sn.grow_daily[sn.grow_daily$sensornode == combos[i, "Var1"],] 
    tmp2 = sn.grow_daily[sn.grow_daily$sensornode == combos[i, "Var2"],] 
    
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

colnames(combos) = c("s1", "s2", "Psi")
write.csv(combos, "data/07.19.20.sennetdissimilarity_growing.csv", row.names = FALSE ) # uncomment to re-write these (cleaner/more data)


