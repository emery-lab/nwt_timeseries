## Use this script to make pairwise psi values
## Load package 'distantia' 
library(distantia)
## load cleanish data
phenocam = read.csv("data/07.13.20.nwt_sennet_phenocams.csv")

sensor_nodes = unique(phenocam$node)

## get list of all the possible node pairs, then remove identical pairs i.e. 21-21
combos = expand.grid(sensor_nodes, sensor_nodes)
combos = combos[combos$Var1 != combos$Var2, ]
combos$Psi = NA # raw Psi column


## this loops through combos and generates the Psi value for eacch of the plot combos. This includes the soil moisture and temperature data (see script 00)
system.time( ## runs for like 11 minutes on my Mac (WR)
  for(i in 1:nrow(combos)){
    print(i)
    tmp1 = phenocam[phenocam$node == combos[i, "Var1"],] 
    tmp2 = phenocam[phenocam$node == combos[i, "Var2"],] 
    
    tmp1 = tmp1[, c("doy", "gcc")]
    tmp2 = tmp2[, c("doy", "gcc")]
    
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

combos$type = "psi_greeness"
combos = combos[, c(1,2,4,3)]


colnames(combos) = c("s1", "s2", "type","distance")
xx1 = read.csv("data/bray_psi_metrics.csv")
xx2 = rbind(xx1, combos)

# write.csv(combos, "data/06.28.20.sennetdissimilarity.csv", row.names = FALSE ) # uncomment to re-write these (cleaner/more data)
