# NWT SNA Time Series Analysis

#packages
library(vegan)
library(picante)
library(tidyverse)
library(lubridate)
library(cowplot)
library(sf)
library(ecodist)

#### PSI (combined climate) #### ####
# bring in all time-series psi metrics
load("data/09.03.2020.all.psi.Rdata")
head(all.psi)

# PSI annual 
psi.annual <- all.psi[,1:3]
head(psi.annual)
# update column names 
colnames(psi.annual) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.dup.annual <- psi.annual %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.annual) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.dup.annual$s1 <- as.numeric(psi.dup.annual$s1)
psi.dup.annual$s2 <- as.numeric(psi.dup.annual$s2)
# turn psi df into a matrix
psi.matrix.annual <- 
  psi.dup.annual %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.matrix.annual <- as.matrix(psi.matrix.annual)
row.names(psi.matrix.annual) <- colnames(psi.matrix.annual)
psi.matrix.annual

# PSI annual scaled
psi.annual.s <- all.psi[,c(1,2,4)]
head(psi.annual.s)
# update column names 
colnames(psi.annual.s) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.dup.annual.s <- psi.annual.s %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.annual.s) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.dup.annual.s$s1 <- as.numeric(psi.dup.annual.s$s1)
psi.dup.annual.s$s2 <- as.numeric(psi.dup.annual.s$s2)
# turn psi df into a matrix
psi.matrix.annual.s <- 
  psi.dup.annual.s %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.matrix.annual.s<- as.matrix(psi.matrix.annual.s)
row.names(psi.matrix.annual.s) <- colnames(psi.matrix.annual.s)
psi.matrix.annual.s

# PSI grow 
psi.grow <- all.psi[,c(1,2,5)]
head(psi.grow)
# update column names 
colnames(psi.grow) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.dup.grow <- psi.grow %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.grow) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.dup.grow$s1 <- as.numeric(psi.dup.grow$s1)
psi.dup.grow$s2 <- as.numeric(psi.dup.grow$s2)
# turn psi df into a matrix
psi.matrix.grow <- 
  psi.dup.grow %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.matrix.grow <- as.matrix(psi.matrix.grow)
row.names(psi.matrix.grow) <- colnames(psi.matrix.grow)
psi.matrix.grow

# PSI grow scaled
psi.grow.s <- all.psi[,c(1,2,6)]
head(psi.grow.s)
# update column names 
colnames(psi.grow.s) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.dup.grow.s <- psi.grow.s %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.grow.s) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.dup.grow.s$s1 <- as.numeric(psi.dup.grow.s$s1)
psi.dup.grow.s$s2 <- as.numeric(psi.dup.grow.s$s2)
# turn psi df into a matrix
psi.matrix.grow.s <- 
  psi.dup.grow.s %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.matrix.grow.s<- as.matrix(psi.matrix.grow.s)
row.names(psi.matrix.grow.s) <- colnames(psi.matrix.grow.s)
psi.matrix.grow.s

# PSI winter 
psi.winter <- all.psi[,c(1,2,7)]
head(psi.winter)
# update column names 
colnames(psi.winter) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.dup.winter <- psi.winter %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.winter) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.dup.winter$s1 <- as.numeric(psi.dup.winter$s1)
psi.dup.winter$s2 <- as.numeric(psi.dup.winter$s2)
# turn psi df into a matrix
psi.matrix.winter <- 
  psi.dup.winter %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.matrix.winter <- as.matrix(psi.matrix.winter)
row.names(psi.matrix.winter) <- colnames(psi.matrix.winter)
psi.matrix.winter

# PSI winter scaled
psi.winter.s <- all.psi[,c(1,2,8)]
head(psi.winter.s)
# update column names 
colnames(psi.winter.s) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.dup.winter.s <- psi.winter.s %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.winter.s) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.dup.winter.s$s1 <- as.numeric(psi.dup.winter.s$s1)
psi.dup.winter.s$s2 <- as.numeric(psi.dup.winter.s$s2)
# turn psi df into a matrix
psi.matrix.winter.s <- 
  psi.dup.winter.s %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.matrix.winter.s<- as.matrix(psi.matrix.winter.s)
row.names(psi.matrix.winter.s) <- colnames(psi.matrix.winter.s)
psi.matrix.winter.s

#### PSI (separated climate) ####
load("data/09.28.2020.psi.breakdown.Rdata")
head(psi.breakdown)

# PSI temp annual 
psi.temp.annual <- psi.breakdown[, c(1,2,8)]
head(psi.temp.annual)
# update column names 
colnames(psi.temp.annual) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.temp.annual <- psi.temp.annual %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.temp.annual) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.temp.annual$s1 <- as.numeric(psi.temp.annual$s1)
psi.temp.annual$s2 <- as.numeric(psi.temp.annual$s2)
# turn into a matrix
psi.temp.annual.matrix <- 
  psi.temp.annual %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.temp.annual.matrix  <- as.matrix(psi.temp.annual.matrix )
row.names(psi.temp.annual.matrix ) <- colnames(psi.temp.annual.matrix )
psi.temp.annual.matrix 
image(psi.temp.annual.matrix)

# PSI moist annual
psi.moist.annual <- psi.breakdown[,c(1,2,4)]
head(psi.moist.annual)
# update column names 
colnames(psi.moist.annual) <- c("s1", "s2", "psi")
# duplicate all sensor nodes so that all combinations exist
psi.moist.annual <- psi.moist.annual %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.moist.annual) %>% 
  rbind(data.frame(s1 = c(6:14, 16, 17, 19:21), s2 = c(6:14, 16, 17, 19:21), psi = 0))
psi.moist.annual$s1 <- as.numeric(psi.moist.annual$s1)
psi.moist.annual$s2 <- as.numeric(psi.moist.annual$s2)
# turn into a matrix
psi.moist.annual.matrix <- 
  psi.moist.annual %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.moist.annual.matrix  <- as.matrix(psi.moist.annual.matrix )
row.names(psi.moist.annual.matrix ) <- colnames(psi.moist.annual.matrix )
psi.moist.annual.matrix

#### VEG ####
# Plant community data
veg <- read.csv("raw_data/06.28.20_sennet_plantcommunity_raw.csv") 
head(veg)
unique(veg$sensor_node) %in% colnames(psi.matrix.annual)

# subset veg to plots with psi calculated
veg <- subset(veg,veg$sensor_node %in% colnames(psi.matrix.annual))

# subset data to 2019 etc.
veg <- subset(veg, year == 2019 & abundance!= 0.5 & multi.hit_or_top.hit == "top-hit-of-multihit-protocol")
head(veg)

# subset to columns of interest (plot, species, abundance)
veg <- veg[,c("sensor_node", "abundance", "species")]

# make df a community matrix
veg.matrix <- sample2matrix(veg)
dim(veg.matrix)
veg.matrix

# remove 'non-veg' rows
colnames(veg.matrix) #Rock, Unknown_seedling, Unknown_graminoid, Soil, Litter, Lichen, Moss
non_veg <- c("Rock", "Unknown_seedling", "Unknown_graminoid", "Soil", "Soil ","Litter", "Lichen", "Moss")
veg.matrix <- veg.matrix[, !colnames(veg.matrix) %in% non_veg]

# calculate Bray-Curtis dissimilarity 
veg.dissimilarity <- vegdist(x = veg.matrix, method = "bray", upper = T, diag = T)
veg.dissimilarity.matrix <- as.matrix(veg.dissimilarity)
dim(veg.dissimilarity.matrix)
veg.dissimilarity.matrix

#### PHYSICAL DISTANCE ####
# bring in physical distance
physical.distance <- read.csv("data/06.29.2020.sensorlocations.csv")
physical.distance

# remove plots 1 & 15
physical.distance <- physical.distance[-1,]
physical.distance <- physical.distance[-10,]
physical.distance

#make a spatial object
physical.distance <- sf::st_as_sf(physical.distance, coords = c("long", "lat"), crs = 4326)
physical.distance

distance <- st_distance(physical.distance)
dim(distance)
distance

#### Community-level MODELS ####


# mantel test
vegan::mantel(ydis = psi.matrix.annual, xdis = veg.dissimilarity.matrix)
vegan::mantel(ydis = psi.matrix.annual.s, xdis = veg.dissimilarity.matrix)
vegan::mantel(ydis = psi.matrix.grow, xdis = veg.dissimilarity.matrix)
vegan::mantel(ydis = psi.matrix.grow.s, xdis = veg.dissimilarity.matrix)
vegan::mantel(ydis = psi.matrix.winter, xdis = veg.dissimilarity.matrix)
vegan::mantel(ydis = psi.matrix.winter.s, xdis = veg.dissimilarity.matrix)

# Function from MMRRTutorial.R
Xmats <- list(temperature = psi.temp.annual.matrix, moisture = psi.moist.annual.matrix, physical = distance)
MMRR(veg.dissimilarity.matrix,Xmats, nperm = 10000)

#### Psi by species relationships ####
veg.matrix

veg.sp <- matrix2sample(veg.matrix)
head(veg.sp)


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

