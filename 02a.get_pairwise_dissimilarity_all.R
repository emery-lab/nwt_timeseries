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
file.to.load = list.files("data/")[grep("sn.03", list.files("data/"))]
load(paste0("data/", file.to.load))
rm(sn.03a, sn.03n)
rm(file.to.load)

#### 1. ADD SAMPLE/TIME COLUMN ####
## add a 'time' column, because distantia can't handle dates. Dumb.
dates = seq(min(sn.03r$date), max(sn.03r$date), by = 1) # range of dates
sample = 1:882 # there are 882 'replace' dates with this
dates = as.data.frame(dates) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df = cbind(dates, sample) # full df with actual date and useable sample column
rm(dates, sample) 

## merge time.df with the main df. I might save this as the base data frame in the future. 
sn.03r = merge(sn.03r, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
## Check to make sure there is actually samples in each one. (and 15 or less)
#table(sn.01_daily.2$sample)
rm(time.df)

## remove date column
sn.03r = sn.03r[, -which(names(sn.03r) %in% c("date"))]

#### 2. PREPARE SEQUENCES ####
seq.03r = prepareSequences(
            sequences = sn.03r,
            grouping.column = "sensornode",
            time.column = "sample",
            if.empty.cases = "omit"
)

#### 3. CALCULATE PSI ####
psi.03r = workflowPsiHP(
            sequences =  seq.03r,
            grouping.column = "sensornode",
            time.column = "sample",
            parallel.execution = TRUE
                
)

rm(seq.03r, sn.03r)

#### 4. IMPORTANCE OF EACH VARIABLE IN TS ####
# importance.03r = workflowImportanceHP(
#                 sequences = seq.03r,
#                 grouping.column = "sensornode",
#                 time.column = "sample",
#                 parallel.execution = TRUE
# )








