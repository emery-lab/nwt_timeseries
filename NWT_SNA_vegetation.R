# NWT SNA Time Series Analysis

library(vegan)
library(picante)
library(tidyverse)
library(lubridate)
library(cowplot)
library(sf)
library(ecodist)

# plot time-series psi metric
psi <- read.csv("data/08.12.20.sennetdissimilarity.csv")
head(psi)
colnames(psi) <- c("s1", "s2", "psi")
length(unique(psi$s1)) # 16
length(unique(psi$s2)) # 16

#ggplot(psi, aes(x = s1, y = psi))+
#  geom_point()+
#  theme_classic()


psi.matrix <- spread(psi, s1, psi)
psi.matrix[is.na(psi.matrix)] <- 0
psi.matrix$s2 # contains a plots 18, but no plot 1
dim(psi.matrix)

# remove s2 column
psi.matrix <- psi.matrix[,-1]
dim(psi.matrix)

# remove plot 18 to match the veg data
psi.matrix <- psi.matrix[,-13] # remove column
psi.matrix <- psi.matrix[rownames(psi.matrix) != "13",] # remove row
plots <- c(6:17, 19:21) # remake plots to not contain 18

psi.matrix <- as.matrix(psi.matrix)
row.names(psi.matrix) <- plots
psi.matrix
dim(psi.matrix)

# plot plant community data
veg <- read.csv("06.28.20_sennet_plantcommunity_raw.csv") 
head(veg)
unique(veg$sensor_node) # contains a plot 1, but not 18
unique(veg$date) # multiple years of data, subset to 2019
veg <- subset(veg, year == 2019 & abundance!= 0.5 & multi.hit_or_top.hit == "top-hit-of-multihit-protocol")
head(veg)

# subset to columns of interet  (plot, species, abundance)
veg <- veg[,c("sensor_node", "abundance", "species")]

# remove plot 1 
veg <- subset(veg, sensor_node != 1)
unique(veg$sensor_node) %in% plots # matches plots in psi metric

# make df a community matrix
veg.matrix <- sample2matrix(veg)
dim(veg.matrix)
veg.matrix

# remove 'non-veg' rows
colnames(veg.matrix) #Rock, Unknown_seedling, Unknown_graminoid, Soil, Litter, Lichen, Moss
non_veg <- c("Rock", "Unknown_seedling", "Unknown_graminoid", "Soil", "Soil ","Litter", "Lichen", "Moss")

veg.matrix <- veg.matrix[, !colnames(veg.matrix) %in% non_veg]


# calculate dissimilarity 
veg.dissimilarity <- vegdist(x = veg.matrix, method = "bray", upper = T, diag = T)
veg.dissimilarity.matrix <- as.matrix(veg.dissimilarity)
dim(veg.dissimilarity.matrix)
veg.dissimilarity.matrix

# 2 matrices
psi.matrix; dim(psi.matrix)
veg.dissimilarity.matrix; dim(veg.dissimilarity.matrix)

vegan::mantel(ydis = psi.matrix, xdis = veg.dissimilarity.matrix)
# almost 40% correlated! sig = 0.026

# bring in physical distance
physical.distance <- read.csv("06.29.2020.sensorlocations.csv")
physical.distance

# remove plot 1 
physical.distance <- physical.distance[-1,]
physical.distance
physical.distance <- sf::st_as_sf(physical.distance, coords = c("long", "lat"), crs = 4326)
physical.distance

distance <- st_distance(physical.distance) # no longer working due to package issues
distance
dim(distance)

# three matrices, all 15 by 15
distance; dim(distance)
psi.matrix; dim(psi.matrix)
veg.dissimilarity.matrix; dim(veg.dissimilarity.matrix)

# Fromm MMRRTutorial.R
Xmats <- list(ecology=psi.matrix)
str(Xmats)
MMRR(veg.dissimilarity.matrix,Xmats, nperm = 10000)

##### Turn them into data frames ####
veg.dissimilarity.df <- gather(veg.dissimilarity, "s2", "dis")
veg.dissimilarity.df$s1 <- colnames(veg.dissimilarity) 
veg.dissimilarity.df <- veg.dissimilarity.df[, c("s1", "s2", "dis")]
veg.dissimilarity.df

psi
combined <- merge(veg.dissimilarity.df, psi, by = c("s1", "s2"))
combined

summary(lm(dis ~ psi, data = combined))
plot(combined$psi, combined$dis)

combined <- gather(combined, key = "type", value = "distance", - s1, -s2)
combined

veg.plot <- ggplot(combined[combined$type == "dis",], aes(s1,s2, fill = distance))+
  geom_raster()+
  theme_classic()

psi.plot <- ggplot(combined[combined$type == "psi",], aes(s1,s2, fill = distance))+
  geom_raster()+
  theme_classic()

plot_grid(veg.plot, psi.plot)

head(combined)

#write.csv(combined, "bray_psi_metrics.csv", row.names = F)



#### Make ordination of community data ####
head(veg.matrix)

NMDS<-metaMDS(veg.matrix, distance = "bray",trymax=100)
stressplot(NMDS)
summary(NMDS)

ordiplot(NMDS,type = "n")
#priSpp <- diversity(veg.matrix, index = "invsimpson", MARGIN = 2)
#orditorp(NMDS,display = "species",priority=priSpp, cex = .8) 

ordiplot(NMDS,type = "n")
orditorp(NMDS,display = "species") 
orditorp(NMDS,display = "sites", col = "red") 

