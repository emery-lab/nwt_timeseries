## 02a.get_pairwise_dissimilarity
## This script will make pairwise Psi values for the full duration that the sensor network has been running.
## for this script it just uses sn.03r because that seems to be the best
## see 01.compare_cleaning_methods

#### 0. GET IT READY #### 
## Load package 'distantia' 
library(distantia)
## might need to update this to a better date. I might remove the  dates altogether... 
## with the file.to.load object it shouldn't matter what the date is.
## remove old ones later? 
## load file (.Rdata, three objects)
file.to.load = list.files("data/")[grep("sn.04", list.files("data/"))]
load(paste0("data/", file.to.load))
rm(sn.04a, sn.04n)
rm(file.to.load)

## get rid of 5cm columns
sn.04r.30cm = sn.04r[, -grep("5cm", names(sn.04r))]
rm(sn.04r)
## get rid of season column
sn.04r.30cm = sn.04r.30cm[, -which(names(sn.04r.30cm) %in% c("season"))]

#### 1. ADD SAMPLE/TIME COLUMN ####
## add a 'time' column, because distantia can't handle dates. Dumb.
dates = seq(min(sn.04r.30cm$date), max(sn.04r.30cm$date), by = 1) # range of dates
sample = 1:548 # there are 882 'replace' dates with this
dates = as.data.frame(dates) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df = cbind(dates, sample) # full df with actual date and useable sample column
rm(dates, sample) 

## merge time.df with the main df. I might save this as the base data frame in the future. 
sn.04r.30cm = merge(sn.04r.30cm, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
## Check to make sure there is actually samples in each one. (and 15 or less)
#table(sn.01_daily.2$sample)
rm(time.df)

## remove date column
sn.04r.30cm = sn.04r.30cm[, -which(names(sn.04r.30cm) %in% c("date"))]

#### 2. PREPARE SEQUENCES ####
seq.04r.30cm = prepareSequences(
  sequences = sn.04r.30cm,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)

#### 3. CALCULATE PSI ####
psi.04r.30cm = workflowPsiHP(
  sequences =  seq.04r.30cm,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

rm(seq.04r.30cm, sn.04r.30cm)

#### 5. ADD TO PSI DATAFRAME ####
source("functions.R")
source("02a.get_pairwise_dissimilarity_all.R")
source("02b.get_pairwise_dissimilarity_growing.R")

psi.030430cmr = merge.psi(psi.0304r, psi.04r.30cm)
colnames(psi.030430cmr) = c(names(psi.0304r), "psi.30cm")
rm(psi.04r.30cm, psi.0304r)

## ur basic plotting 
plot(x = psi.030430cmr$psi.growing, 
     y = psi.030430cmr$psi.30cm,
     xlab = expression(psi[" growing"]),
     ylab = expression(paste(psi[" 30cm"])),
     asp = 1,
     pch = 21,
     col = "gray",
     bg = "light gray",
     bty = "l",
     cex.lab = 1.4)

mod030430cm = lm(psi.30cm ~ psi.growing, data = psi.030430cmr)
summary(mod030430cm)


