## 02a.get_pairwise_dissimilarity_growing.R
## This script will make pairwise Psi values for the growing season (APRIL 1st to OCTOBER 1st)

#### 0. GET IT READY #### 
## Load package 'distantia' 
library(distantia)
## might need to update this to a better date. I might remove the  dates altogether... 
## with the file.to.load object it shouldn't matter what the date is.
## remove old ones later? 
## load file (.Rdata, three objects)
file.to.load = list.files("data/")[grep("sn.05", list.files("data/"))]
load(paste0("data/", file.to.load))
rm(file.to.load)

## get rid of season column
sn.05 = sn.05[, -which(names(sn.05) %in% c("season"))]

#### 1. ADD SAMPLE/TIME COLUMN ####
## add a 'time' column, because distantia can't handle dates. Dumb.
dates = seq(min(sn.05$date), max(sn.05$date), by = 1) # range of dates
sample = 1:548 # there are 882 'replace' dates with this
dates = as.data.frame(dates) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df = cbind(dates, sample) # full df with actual date and useable sample column
rm(dates, sample) 

## merge time.df with the main df. I might save this as the base data frame in the future. 
sn.05 = merge(sn.05, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
## Check to make sure there is actually samples in each one. (and 15 or less)
#table(sn.01_daily.2$sample)
rm(time.df)

## remove date column
sn.05 = sn.05[, -which(names(sn.05) %in% c("date"))]

#### 2. PREPARE SEQUENCES ####
seq.05 = prepareSequences(
  sequences = sn.05,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)

#### 3. CALCULATE PSI ####
psi.05 = workflowPsiHP(
  sequences =  seq.05,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

rm(seq.05, sn.05)

#### 4. ADD TO PSI DATAFRAME ####
source("functions.R")
source("02a.get_pairwise_dissimilarity_all.R")

all.psi = merge.psi(psi.04, psi.05)
colnames(all.psi) = c("A", "B", "psi.all", "psi.growing")
rm(psi.04, psi.05)

#### 5. FIGURE AND STATISTICS COMPARING - ALL vs. GROWING ####
## write png 
png("figures/compare_all_growing.png", height = 5, width = 6, res = 300, units = "in")
par(mar = c(4, 4.5, 2, 2)) # change margins to be prettier

## ur basic plotting 
plot(x = all.psi$psi.all, 
     y = all.psi$psi.growing,
     xlab = expression(psi[" all"]),
     ylab = expression(paste(psi[" growing season"])),
     asp = 1,
     pch = 21,
     col = "gray",
     bg = "light gray",
     bty = "l",
     cex.lab = 1.4)

## to get adjusted R2
mod1 = lm(psi.growing ~ psi.all, data = all.psi)
summary(mod1)
## add line
abline(mod1, lty = 2, lwd = 2)
## add R2
text(x = 15, y = 5, expression("R"^2), adj = 0)
text(x = 16.5, y = 4.9, "= 0.7961", adj = 0)
rm(mod1)
dev.off()
