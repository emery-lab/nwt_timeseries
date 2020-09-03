## 02a.get_pairwise_dissimilarity
## This script will make pairwise Psi values for the full duration that the sensor network has been running.

#### 0. GET IT READY #### 
## Load package 'distantia' 
library(distantia)
## might need to update this to a better date. I might remove the  dates altogether... 
## with the file.to.load object it shouldn't matter what the date is.
## remove old ones later? 
## load file (.Rdata, three objects)
file.to.load = list.files("data/")[grep("sn.04", list.files("data/"))]
load(paste0("data/", file.to.load))
rm(file.to.load)

#### 1. ADD SAMPLE/TIME COLUMN ####
## add a 'time' column, because distantia can't handle dates. Dumb.
dates = seq(min(sn.04$date), max(sn.04$date), by = 1) # range of dates
sample = 1:883 # there are 882 'replace' dates with this
dates = as.data.frame(dates) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df = cbind(dates, sample) # full df with actual date and useable sample column
rm(dates, sample) 

## merge time.df with the main df. I might save this as the base data frame in the future. 
sn.04 = merge(sn.04, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
sn.04s = merge(sn.04s, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
## Check to make sure there is actually samples in each one. (and 15 or less)
#table(sn.01_daily.2$sample)
rm(time.df)

## remove date column
sn.04 = sn.04[, -which(names(sn.04) %in% c("date"))]
sn.04s = sn.04s[, -which(names(sn.04s) %in% c("date"))]


#### 2. PREPARE SEQUENCES ####
seq.04 = prepareSequences(
            sequences = sn.04,
            grouping.column = "sensornode",
            time.column = "sample",
            if.empty.cases = "omit"
)

seq.04s = prepareSequences(
  sequences = sn.04s,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)


#### 3. CALCULATE PSI ####
psi.04 = workflowPsiHP(
            sequences =  seq.04,
            grouping.column = "sensornode",
            time.column = "sample",
            parallel.execution = TRUE
                
)

psi.04s = workflowPsiHP(
  sequences =  seq.04s,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

psi.04.all = merge(psi.04, psi.04s, by = c("A", "B"))
colnames(psi.04.all) = c("A", "B", "psi.all", "psi.all.scaled")
rm(sn.04, sn.04s, psi.04, psi.04s)

#### 4. IMPORTANCE OF EACH VARIABLE IN TS ####
# importance.03r = workflowImportanceHP(
#                 sequences = seq.03r,
#                 grouping.column = "sensornode",
#                 time.column = "sample",
#                 parallel.execution = TRUE
# )



rm(seq.04, seq.04s)



