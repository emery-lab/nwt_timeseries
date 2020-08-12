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
colnames(distances_melted) = c("s1", "s2", "real_distance")
dissim_distance = merge(xx2, distances_melted) ## run to here if you want this data. might be useful. 
dissim_distance[order(dissim_distance$type, dissim_distance$s1, dissim_distance$s2),]

write.csv(dissim_distance, "data/07.14.20.sensordissimilarity_wdistance.csv", row.names = F)



# plot the senors again
png("panel_plot.png", height = 8, width = 12, units = "in", res = 300)
par(mfrow = c(2,3), mar = c(4,4,1,1))

locs = locs[locs$sensor != 1, ]

plot(locs$long, locs$lat, asp = 1, pch = 19, cex = 1.3, ylim = c(40.05, 40.056))
text(locs$long, locs$lat, labels = locs$sensor, pos = 3)

# plot lines with widths of the *inverse* of dissimilarity bigger is more similar. 

community = dissim_distance[dissim_distance$type == "dis", ]


for(i in 1:nrow(community)){
  lines(x = c(locs[locs$sensor %in% community$s1[i], "long"], locs[locs$sensor %in% community$s2[i], "long"]),
        y = c(locs[locs$sensor %in% community$s1[i], "lat"], locs[locs$sensor %in% community$s2[i], "lat"]),
        lwd = community$distance[i], col = rgb(0.1, 0.1, 0.5, 0.3))
}


plot(locs$long, locs$lat, asp = 1, pch = 19, cex = 1.3, ylim = c(40.05, 40.056))
text(locs$long, locs$lat, labels = locs$sensor, pos = 3)

greeness = dissim_distance[dissim_distance$type == "psi_greeness", ]


for(i in 1:nrow(greenness)){
  lines(x = c(locs[locs$sensor %in% greeness$s1[i], "long"], locs[locs$sensor %in% greeness$s2[i], "long"]),
        y = c(locs[locs$sensor %in% greeness$s1[i], "lat"], locs[locs$sensor %in% greeness$s2[i], "lat"]),
        lwd = greenness$distance[i] * 0.1, col = rgb(0.1, 0.5, 0.1, 0.3))
}

psi = dissim_distance[dissim_distance$type == "psi", ]

plot(locs$long, locs$lat, asp = 1, pch = 19, cex = 1.3, ylim = c(40.05, 40.056))
text(locs$long, locs$lat, labels = locs$sensor, pos = 3)

for(i in 1:nrow(psi)){
  lines(x = c(locs[locs$sensor %in% psi$s1[i], "long"], locs[locs$sensor %in% psi$s2[i], "long"]),
        y = c(locs[locs$sensor %in% psi$s1[i], "lat"], locs[locs$sensor %in% psi$s2[i], "lat"]),
        lwd = psi$distance[i] * 0.05, col = rgb(0.5, 0.1, 0.1, 0.3))
}

plot(community$distance ~ community$real_distance, pch = 19, col = community$s2)
plot(greeness$distance ~ greeness$real_distance, pch = 19, col = greeness$s2)
plot(psi$distance ~ psi$real_distance, pch = 19, col = psi$s2)

dev.off()

## look at the difference when I just include the growing season
# note: this includes air temp and relative humitidy (for now at least). I think this is probably making them more similar overall.
dissim_grow = read.csv("data/07.19.20.sennetdissimilarity_growing.csv")

distance_dissim_grow = merge(dissim_grow, distances_melted)

plot(locs$long, locs$lat, asp = 1, pch = 19, cex = 1.3, ylim = c(40.05, 40.056))
text(locs$long, locs$lat, labels = locs$sensor, pos = 3)

for(i in 1:nrow(distance_dissim_grow)){
  lines(x = c(locs[locs$sensor %in% distance_dissim_grow$s1[i], "long"], locs[locs$sensor %in% distance_dissim_grow$s2[i], "long"]),
        y = c(locs[locs$sensor %in% distance_dissim_grow$s1[i], "lat"], locs[locs$sensor %in% distance_dissim_grow$s2[i], "lat"]),
        lwd = distance_dissim_grow$Psi[i] * 0.05, col = rgb(0.5, 0.1, 0.1, 0.3))
}

plot(distance_dissim_grow$Psi ~ distance_dissim_grow$real_distance, pch = 19, col = "gray", xlab = "Distance (m)", ylab = "Environmental dissimilarity (Psi)")
mod = lm(distance_dissim_grow$Psi ~ distance_dissim_grow$real_distance)
summary(mod)
abline(mod$coefficients, lwd = 2)
cor(distance_dissim_grow$Psi, distance_dissim_grow$real_distance)
#0.588

bray = read.csv("data/bray_psi_metrics.csv")
bray = bray[bray$type == "dis", ]
bray = bray[, c(1,2,4)]
all = merge(bray, dissim_grow)
plot(all$distance ~ all$Psi, pch = 19, col = "gray", xlab = "Environmental dissimilarity (Psi)", ylab = "Community Distance (bray-curtis)")
cor(all$Psi, all$distance)
mod1 = lm(all$distance ~ all$Psi)
summary(mod1)
abline(mod1$coefficients, lwd = 2)
