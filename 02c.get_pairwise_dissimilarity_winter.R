## 02a.get_pairwise_dissimilarity
## This script will make pairwise Psi values WINTER (Inverse dates of growing season)

#### 0. GET IT READY #### 
## Load package 'distantia' 
library(distantia)
## might need to update this to a better date. I might remove the  dates altogether... 
## with the file.to.load object it shouldn't matter what the date is.
## remove old ones later? 
## load file (.Rdata, three objects)
file.to.load = list.files("data/")[grep("sn.06", list.files("data/"))]
load(paste0("data/", file.to.load))
rm(file.to.load)

## get rid of season column
sn.06 = sn.06[, -which(names(sn.06) %in% c("season"))]
sn.06s = sn.06s[, -which(names(sn.06s) %in% c("season"))]

#### 1. ADD SAMPLE/TIME COLUMN ####
## add a 'time' column, because distantia can't handle dates. Dumb.
dates = seq(min(sn.06$date), max(sn.06$date), by = 1) # range of dates
sample = 1:547 
dates = as.data.frame(dates) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df = cbind(dates, sample) # full df with actual date and useable sample column
rm(dates, sample) 

## merge time.df with the main df. I might save this as the base data frame in the future. 
sn.06 = merge(sn.06, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
sn.06s = merge(sn.06s, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
## Check to make sure there is actually samples in each one. (and 15 or less)
#table(sn.01_daily.2$sample)
rm(time.df)

## remove date column
sn.06 = sn.06[, -which(names(sn.06) %in% c("date"))]
sn.06s = sn.06s[, -which(names(sn.06s) %in% c("date"))]

#### 2. PREPARE SEQUENCES ####
seq.06 = prepareSequences(
  sequences = sn.06,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)

seq.06s = prepareSequences(
  sequences = sn.06s,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)


#### 3. CALCULATE PSI ####
psi.06 = workflowPsiHP(
  sequences =  seq.06,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

psi.06s = workflowPsiHP(
  sequences =  seq.06s,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)


rm(seq.06, sn.06, sn.06s, seq.06s)

#### 5. ADD TO PSI DATAFRAME ####
source("functions.R")
source("02a.get_pairwise_dissimilarity_all.R")
source("02b.get_pairwise_dissimilarity_growing.R")

all.psi = merge.psi(all.psi, psi.06)
colnames(all.psi) = colnames(all.psi) = c("A", "B", "psi.all", "psi.all.scaled", "psi.growing", "psi.growing.scaled", "psi.winter")
all.psi = merge.psi(all.psi, psi.06s)
colnames(all.psi) = colnames(all.psi) = c("A", "B", "psi.all", "psi.all.scaled", "psi.growing", "psi.growing.scaled", "psi.winter.scaled")

rm(psi.06, psi.06s)

#### 6. MAKE A PLOT TO SEE ####
# png("figures/compare_all_v2.png", height = 2.5, width = 7.5, res = 200, units = "in")
# par(mfrow = c(1,3), mar = c(4, 4.5, 1, 1)) # change margins to be prettier
# plot(x = all.psi$psi.all,
#      y = all.psi$psi.growing,
#      xlim = c(0,36),
#      ylim = c(0,36),
#      xlab = expression(psi[" all"]),
#      ylab = expression(paste(psi[" summer"])),
#      asp = 1,
#      pch = 21,
#      cex = 1.4,
#      col = "dark gray",
#      bg = rgb(212/255, 212/255, 212/255, 0.6),
#      bty = "l",
#      cex.lab = 1.6,
#      cex.axis = 1.4,
#      xaxt = "n",
#      yaxt = "n")
# 
# axis(1, labels = c(0,10,20,30), at = c(0,10,20,30))
# axis(2, labels = c(0,10,20,30), at = c(0,10,20,30))
# 
# plot(x = all.psi$psi.all,
#      y = all.psi$psi.winter,
#      xlim = c(0,36),
#      ylim = c(0,36),
#      xlab = expression(psi[" all"]),
#      ylab = expression(paste(psi[" winter"])),
#      asp = 1,
#      pch = 21,
#      cex = 1.4,
#      col = "dark gray",
#      bg = rgb(212/255, 212/255, 212/255, 0.6),
#      bty = "l",
#      cex.lab = 1.6,
#      cex.axis = 1.4,
#      xaxt = "n",
#      yaxt = "n")
# 
# axis(1, labels = c(0,10,20,30), at = c(0,10,20,30))
# axis(2, labels = c(0,10,20,30), at = c(0,10,20,30))
# 
# plot(x = all.psi$psi.growing,
#      y = all.psi$psi.winter,
#      xlim = c(0,36),
#      ylim = c(0,36),
#      xlab = expression(psi[" growing"]),
#      ylab = expression(paste(psi[" winter"])),
#      asp = 1,
#      pch = 21,
#      cex = 1.4,
#      col = 'dark gray',
#      bg = rgb(212/255, 212/255, 212/255, 0.6),
#      bty = "l",
#      cex.lab = 1.6,
#      cex.axis = 1.4,
#      xaxt = "n",
#      yaxt = "n")
# 
# axis(1, labels = c(0,10,20,30), at = c(0,10,20,30))
# axis(2, labels = c(0,10,20,30), at = c(0,10,20,30))
# 
# 
# dev.off()
# 
# ## ur basic plotting 
# plot(x = all.psi$psi.all, 
#      y = all.psi$psi.winter,
#      xlab = expression(psi[" all"]),
#      ylab = expression(paste(psi[" winter"])),
#      asp = 1,
#      pch = 21,
#      col = "gray",
#      bg = "light gray",
#      bty = "l",
#      cex.lab = 1.4)
# 
# mod2 = lm(psi.winter ~ psi.all, data = all.psi)
# summary(mod2)
# 
# ## add line
# abline(mod2, lty = 2, lwd = 2)
# ## add R2
# text(x = 20, y = 6, expression("R"^2), adj = 0)
# text(x = 22, y = 5.8, "= 0.7074", adj = 0)
# rm(mod2)
# dev.off()
 
#### 7. SAVE 'all.psi' OBJECT AS .RDATA ####
old.file = list.files("data/")[grep("all.psi", list.files("data/"))]
file.remove(paste0("data/",old.file))
file.name = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".all.psi.Rdata")
save(all.psi, file = file.name)
rm(all.psi, file.name, old.file)
