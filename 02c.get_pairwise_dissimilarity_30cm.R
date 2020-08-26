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

#### 1. ADD SAMPLE/TIME COLUMN ####
## add a 'time' column, because distantia can't handle dates. Dumb.
dates = seq(min(sn.06$date), max(sn.06$date), by = 1) # range of dates
sample = 1:547 
dates = as.data.frame(dates) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df = cbind(dates, sample) # full df with actual date and useable sample column
rm(dates, sample) 

## merge time.df with the main df. I might save this as the base data frame in the future. 
sn.06 = merge(sn.06, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
## Check to make sure there is actually samples in each one. (and 15 or less)
#table(sn.01_daily.2$sample)
rm(time.df)

## remove date column
sn.06 = sn.06[, -which(names(sn.06) %in% c("date"))]

#### 2. PREPARE SEQUENCES ####
seq.06 = prepareSequences(
  sequences = sn.06,
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

rm(seq.06, sn.06)

#### 5. ADD TO PSI DATAFRAME ####
source("functions.R")
source("02a.get_pairwise_dissimilarity_all.R")
source("02b.get_pairwise_dissimilarity_growing.R")

all.psi = merge.psi(all.psi, psi.06)
colnames(all.psi) = c("A", "B", "psi.all", "psi.growing", "psi.winter")
rm(psi.06)


#### 6. MAKE A PLOT TO SEE ####
png("figures/compare_all_winter.png", height = 5, width = 6, res = 300, units = "in")
par(mar = c(4, 4.5, 2, 2)) # change margins to be prettier
## ur basic plotting 
plot(x = all.psi$psi.all, 
     y = all.psi$psi.winter,
     xlab = expression(psi[" all"]),
     ylab = expression(paste(psi[" winter"])),
     asp = 1,
     pch = 21,
     col = "gray",
     bg = "light gray",
     bty = "l",
     cex.lab = 1.4)

mod2 = lm(psi.winter ~ psi.all, data = all.psi)
summary(mod2)

## add line
abline(mod2, lty = 2, lwd = 2)
## add R2
text(x = 20, y = 6, expression("R"^2), adj = 0)
text(x = 22, y = 5.8, "= 0.7074", adj = 0)
rm(mod2)
dev.off()

#### 7. SAVE 'all.psi' OBJECT AS .RDATA ####
old.file = list.files("data/")[grep("all.psi", list.files("data/"))]
file.remove(paste0("data/",old.file))
file.name = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".all.psi.Rdata")
save(all.psi, file = file.name)
rm(all.psi, file.name, old.file)
