# NWT SNA Time Series Analysis

#packages
library(vegan)
library(picante)
library(tidyverse)
library(lubridate)
library(cowplot)
library(sf)
library(ecodist)

# read in time-series psi metric
psi.df <- read.csv("data/08.12.20.sennetdissimilarity.csv")
head(psi.df)

# remove 'old psi column
psi.df <- psi.df [, -4]

# update column names 
colnames(psi.df) <- c("s1", "s2", "psi")

#look at sensor node numbers
unique(psi.df$s1)
length(unique(psi.df$s1)) # 14
unique(psi.df$s2)
length(unique(psi.df$s2)) # 14
psi.df

# duplicate all sensor nodes so that all combinations exist
psi.dup <- psi.df %>% 
  mutate(temp = s2, s2 = s1, s1 = temp) %>% 
  select(-temp) %>%
  rbind(psi.df) %>% 
  rbind(data.frame(s1 = c(6:17, 19:21), s2 = c(6:17, 19:21), psi = 0))
psi.dup

# turn psi df into a matrix
psi.matrix <- 
  psi.dup %>% 
  arrange(s1,s2) %>% 
  pivot_wider(id_cols = s1, names_from = s2, values_from = psi) %>% 
  select(-s1)
psi.matrix
dim(psi.matrix)

psi.matrix <- as.matrix(psi.matrix)
row.names(psi.matrix) <- colnames(psi.matrix)

##### still need to rearrange the veg matrix to now match the psi.matrix

# plot plant community data
veg <- read.csv("raw_data/06.28.20_sennet_plantcommunity_raw.csv") 
head(veg)
unique(veg$sensor_node) %in% colnames(psi.matrix)
length(unique(veg$sensor_node))# 17 nodes here -- > need to pair down to 14 from which psi is calculated

# subset veg to plots with psi calculated
veg <- subset(veg,veg$sensor_node %in% colnames(psi.matrix))

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

# 2 matrices
psi.matrix; dim(psi.matrix)
veg.dissimilarity.matrix; dim(veg.dissimilarity.matrix)
image(psi.matrix)
image(veg.dissimilarity.matrix)


vegan::mantel(ydis = psi.matrix, xdis = veg.dissimilarity.matrix)
#40% correlated! sig = 0.004

# bring in physical distance
physical.distance <- read.csv("data/06.29.2020.sensorlocations.csv")
physical.distance

# remove plot 1 
physical.distance <- physical.distance[-1,]
physical.distance
physical.distance <- sf::st_as_sf(physical.distance, coords = c("long", "lat"), crs = 4326)
physical.distance

distance <- st_distance(physical.distance)
distance
dim(distance)

# three matrices, all 15 by 15
distance; dim(distance)
psi.matrix; dim(psi.matrix)
veg.dissimilarity.matrix; dim(veg.dissimilarity.matrix)

ggplot(as.data.frame(psi.matrix))+
  geom_raster()

# Function from MMRRTutorial.R
Xmats <- list(ecology=psi.matrix, physical = distance)
str(Xmats)
MMRR(veg.dissimilarity.matrix,Xmats, nperm = 10000) # R.sq ~ 16%, only psi signficant

##### Turn them into data frames - doesn't currently work ###

veg.dissimilarity.df <- spread(veg.dissimilarity, "s2", "dis")
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

