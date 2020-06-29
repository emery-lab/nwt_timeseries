## read in the geo data and the pairwise dissimilarity 
locs = read.csv("data/06.29.2020.sensorlocations.csv")
dissim = read.csv("data/06.28.20.sennetdissimilarity.csv")

## plot the sensors
plot(locs$long, locs$lat, asp = 1, pch = 19, cex = 2)
text(locs$long, locs$lat, labels = locs$sensor, pos = 3)

##  lazy distance matrix
library(geosphere)
library(reshape2)
## distane matrix
distances_m = distm(as.matrix(locs[, 2:3]), as.matrix(locs[, 2:3]), fun = distHaversine)
## rename to sensors
row.names(distances_m) = unique(locs$sensor)
colnames(distances_m) = unique(locs$sensor)
distances_m

## make a data frame
distances_melted = melt(distances_m)
colnames(distances_melted) = c("first.sensor", "second.sensor", "distance")
dissim_distance = merge(dissim, distances_melted) ## run to here if you want this data. might be useful. 
#write.csv(dissim_distance, "data/06.29.20.sensordissimilarity_wdistance.csv, rownames.false")

##  anything here? , no
plot(dissim_distance$Psi ~ dissim_distance$distance)

# plot the senors again
plot(locs$long, locs$lat, asp = 1, pch = 19, cex = 2)
text(locs$long, locs$lat, labels = locs$sensor, pos = 3)

# plot lines with widths of the *inverse* of dissimilarity bigger is more similar. 
for(i in 1:nrow(dissim_distance)){
  lines(x = c(locs[locs$sensor %in% dissim_distance$first.sensor[i], "long"], locs[locs$sensor %in% dissim_distance$second.sensor[i], "long"]),
        y = c(locs[locs$sensor %in% dissim_distance$first.sensor[i], "lat"], locs[locs$sensor %in% dissim_distance$second.sensor[i], "lat"]),
        lwd = 1/dissim_distance$Psi[i], col = rgb(0.1, 0.1, 0.3, 0.5))
}
