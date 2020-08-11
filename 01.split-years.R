## Use this script to make pairwise psi values
## Load package 'distantia' 
library(distantia)
## load cleanish data
sn.2018 = read.csv("data/07.19.20.sennetdata_2018.csv")
sn.2019 = read.csv("data/07.19.20.sennetdata_2019.csv")
## get the average soil moisture for each day, can do sampling time later


sn.2018$date = as.Date(sn.2018$date)
sn.2019$date = as.Date(sn.2019$date)
sn.2018_daily = as.data.frame(suppressWarnings(aggregate(sn.2018, by = list(sn.2018$date, sn.2018$sensornode), FUN = mean)))
sn.2019_daily = as.data.frame(suppressWarnings(aggregate(sn.2019, by = list(sn.2019$date, sn.2019$sensornode), FUN = mean)))

sn.2018_daily = sn.2018_daily[, c("sensornode", "soiltemp_5cm_avg", "soiltemp_30cm_avg",  
                                  "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
                                  "soilmoisture_b_30cm_avg",  "soilmoisture_c_5cm_avg",  "soilmoisture_c_30cm_avg")]

sn.2019_daily = sn.2019_daily[, c("sensornode", "soiltemp_5cm_avg", "soiltemp_30cm_avg",  
                                  "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
                                  "soilmoisture_b_30cm_avg",  "soilmoisture_c_5cm_avg",  "soilmoisture_c_30cm_avg")]


## list sensor nodes / get unique values
sensor_nodes = unique(sn.2018$sensornode)
sensor_nodes = sensor_nodes[!is.na(sensor_nodes)]

## get list of all the possible node pairs, then remove identical pairs i.e. 21-21
combos = as.data.frame(matrix(nrow = 15, ncol = 2))
colnames(combos) = c("sensor", "Psi")
combos$Psi = NA # raw Psi column




## this loops through combos and generates the Psi value for eacch of the plot combos. This includes the soil moisture and temperature data (see script 00)
system.time( ## runs for like 11 minutes on my Mac (WR)
  for(i in sensor_nodes){
    print(i)
    tmp1 = sn.2018_daily[sn.2018_daily$sensornode == i,] 
    tmp2 = sn.2019_daily[sn.2019_daily$sensornode == i,] 
    
    AB.sequence = prepareSequences(sequence.A = tmp1, 
                                   sequence.A.name = paste("2018"),
                                   sequence.B = tmp2,
                                   sequence.B.name = paste("2019"),
                                   merge.mode = "complete",
                                   if.empty.cases = "omit")
    
    # AB.distance.matrix <- distanceMatrix(
    #   sequences = AB.sequence,
    #   method = "euclidean"
    # )
    # 
    combos[i, "Psi"] = workflowPsi(AB.sequence, grouping.column = 'id')[3]
    combos[i, "sensor"] = i
  })

plot(combos$Psi ~ combos$sensor, xlab = "Sensor", ylab = "Interannual dissimilarity (Psi)")
