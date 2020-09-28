#### 0. GET IT READY #### 
## Load package 'distantia' 
library(distantia)
library(ggplot2)
## might need to update this to a better date. I might remove the  dates altogether... 
## with the file.to.load object it shouldn't matter what the date is.
## remove old ones later? 
## load file (.Rdata, three objects)
file.to.load1 = list.files("data/")[grep("sn.04", list.files("data/"))]
load(paste0("data/", file.to.load1))
rm(file.to.load1)

file.to.load2 = list.files("data/")[grep("sn.05", list.files("data/"))]
load(paste0("data/", file.to.load2))
rm(file.to.load2)

file.to.load3 = list.files("data/")[grep("sn.06", list.files("data/"))]
load(paste0("data/", file.to.load3))
rm(file.to.load3)


#### 1. Load functions ####
source('functions.R')


#### 2. SPLIT TO SINGLE VARIABLE ####
## .04 (All data)
sn.04_temp = sn.04[, c('date', 'sensornode', 'soiltemp_5cm_avg', "soiltemp_30cm_avg")]
sn.04_moisture = sn.04[, c(c("date", "sensornode",
                             "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
                             "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg"))]

## .05 (Growing Season data)
sn.05_temp = sn.05[, c('date', 'sensornode', 'soiltemp_5cm_avg', "soiltemp_30cm_avg")]
sn.05_moisture = sn.05[, c(c("date", "sensornode",
                             "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
                             "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg"))]

## .06 (Winter data)
sn.06_temp = sn.06[, c('date', 'sensornode', 'soiltemp_5cm_avg', "soiltemp_30cm_avg")]
sn.06_moisture = sn.06[, c(c("date", "sensornode",
                             "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", 
                             "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg"))]

#### 3. Calculate PSI ####
sn.04_t = do.psi(sn.04_temp)
sn.04_m = do.psi(sn.04_moisture)

sn.05_t = do.psi(sn.05_temp)
sn.05_m = do.psi(sn.05_moisture)

sn.06_t = do.psi(sn.06_temp)
sn.06_m = do.psi(sn.06_moisture)


#### 4. COMBINE ALL PSI DFS ####
colnames(sn.04_t) = c("A", "B", "Psi_all_temp")
psi = merge.psi(sn.04_t, sn.04_m)
colnames(psi) = c("A", "B", "Psi_all_temp", "Psi_all_moist")
psi = merge.psi(psi, sn.05_t)
colnames(psi) = c("A", "B", "Psi_all_temp", "Psi_all_moist", "Psi_growing_temp")
psi = merge.psi(psi, sn.05_m)
colnames(psi) = c("A", "B", "Psi_all_temp", "Psi_all_moist", "Psi_growing_temp", "Psi_growing_moisture")
psi = merge.psi(psi, sn.06_t)
colnames(psi) = c("A", "B", "Psi_all_temp", "Psi_all_moist", "Psi_growing_temp", "Psi_growing_moisture", "Psi_winter_temp")
psi = merge.psi(psi, sn.06_m)
colnames(psi) = c("A", "B", "Psi_all_temp", "Psi_all_moist", "Psi_growing_temp", "Psi_growing_moisture", "Psi_winter_temp", "Psi_winter_moisture")

psi.breakdown = psi
file.name = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".psi.breakdown.Rdata")
old.file = list.files("data/")[grep("psi.breakdown", list.files("data/"))]
file.remove(paste0("data/",old.file))
save(psi.breakdown, file = file.name)
rm(file.name, old.file)


par(mfrow = c(1,2))
plot.new()
plot.window(ylim = c(0,.5), xlim = c(0,25), bty = "l")
box()
axis(1)
axis(2)
lines(density(psi$Psi_growing_temp), col = "light green", lwd = 3.5)
lines(density(psi$Psi_all_temp), col = "gray", lwd = 3.5)
lines(density(psi$Psi_winter_temp), col = "light blue", lwd = 3.5)

plot.new()
plot.window(ylim = c(0,.5), xlim = c(0,25), bty = "l")
box()
axis(1)
axis(2)
lines(density(psi$Psi_growing_moisture), col = "light green", lwd = 3.5)
lines(density(psi$Psi_all_moist), col = "gray", lwd = 3.5)
lines(density(psi$Psi_winter_moisture), col = "light blue", lwd = 3.5)


psi1 = psi[, c(1,2,3)]
colnames(psi1) = c('A', 'B', "Psi")
psi1$type = "all_temp"

psi2 = psi[, c(1,2,4)]
colnames(psi2) = c('A', 'B', "Psi")
psi2$type = "all_moist"

psi3 = psi[, c(1,2,5)]
colnames(psi3) = c('A', 'B', "Psi")
psi3$type = "grow_temp"

psi4 = psi[, c(1,2,6)]
colnames(psi4) = c('A', 'B', "Psi")
psi4$type = "grow_moist"

psi5 = psi[, c(1,2,7)]
colnames(psi5) = c('A', 'B', "Psi")
psi5$type = "winter_temp"

psi6 = psi[, c(1,2,8)]
colnames(psi6) = c('A', 'B', "Psi")
psi6$type = "winter_moist"

psi.long = rbind(psi1, psi2, psi3, psi4, psi5, psi6)
str(psi.long)
psi.long$type = as.factor(psi.long$type)


psi.long.moist = rbind(psi2, psi4, psi6)
psi.long.temp = rbind(psi1, psi3, psi5)




png("/Users/Will/Desktop/psi.dist.moist.png", height = 5, width = 5, units = "in", res = 200)
ggplot(data = psi.long.moist, aes(x = type, y = Psi, fill = type)) +
  geom_violin(trim = FALSE) + 
  scale_fill_manual(values = c("Gray", "light green", "light blue")) + 
  geom_boxplot(width = .1, fill = "white") +
  theme_classic() +
  theme(legend.position = "none")
dev.off()

png("/Users/Will/Desktop/psi.dist.temp.png", height = 5, width = 5, units = "in", res = 200)
ggplot(data = psi.long.temp, aes(x = type, y = Psi, fill = type)) +
  geom_violin(trim = FALSE) + 
  scale_fill_manual(values = c("Gray", "light green", "light blue")) + 
  geom_boxplot(width = .1, fill = "white") +
  theme_classic() +
  theme(legend.position = "none")
dev.off()
