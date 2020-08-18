## 02a.get_pairwise_dissimilarity
## This script will make pairwise Psi values for the full duration that the sensor network has been running.

#### 0. GET IT READY #### 
## Load package 'distantia' 
library(distantia)
## might need to update this to a better date. I might remove the  dates altogether... 
load("data/08.17.2020.sn.03.Rdata")

## list sensor nodes / get unique values
sensor_nodes = unique(sn.03a$sensornode)

## make a list of columns to include, this should be most of them except for site and local site etc
include = c("sensornode",  
            "soiltemp_5cm_avg", 
            "soiltemp_30cm_avg", 
            "soilmoisture_a_5cm_avg", 
            "soilmoisture_a_30cm_avg", 
            "soilmoisture_b_5cm_avg", 
            "soilmoisture_b_30cm_avg", 
            "soilmoisture_c_5cm_avg", 
            "soilmoisture_c_30cm_avg", 
            "sample")

## add a 'time' column, because distantia can't handle dates. Dumb.
dates = seq(min(sn.03a$date), max(sn.03a$date), by = 1) # range of dates
sample = 1:882 # there are 882 'replace' dates with this
dates = as.data.frame(dates) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df = cbind(dates, sample) # full df with actual date and useable sample column
rm(dates, sample) 

## merge time.df with the main df. I might save this as the base data frame in the future. 
sn.03a = merge(sn.03a, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
## Check to make sure there is actually samples in each one. (and 15 or less)
#table(sn.01_daily.2$sample)

sn.compare = sn.01_daily.2[, include]


sm.seq = prepareSequences(
            sequences = sn.compare,
            grouping.column = "sensornode",
            time.column = "sample",
            if.empty.cases = "omit"
)

sennet.psi = workflowPsiHP(
                sequences =  sm.seq,
                grouping.column = "sensornode",
                time.column = "sample",
                parallel.execution = TRUE
                
)

write.csv(all.psi, "data/08.12.20.sennetdissimilarity.csv", row.names = FALSE)

# compare to old (wrong) way
old.psi = read.csv("data/06.28.20.sennetdissimilarity.csv")

table(sennet.psi$A)
colnames(sennet.psi) = c("first.sensor", "second.sensor", "new.psi")
table(old.psi$first.sensor)

all.psi = merge(sennet.psi, old.psi)
table(all.psi$first.sensor)
table(all.psi$second.sensor)
par(mfrow = c(1,1))
plot(all.psi$first.sensor, 
     all.psi$second.sensor, 
     xaxt = "n", 
     yaxt = "n", 
     cex = abs(all.psi$new.psi - all.psi$Psi) * 0.1)
axis(1, c(6:21))
axis(2, c(6:21))
abline(0, 1)

## this takes literal years to run, idk when I'll do this. 
sennet.null.psi = workflowNullPsiHP(
  sequences =  sm.seq,
  grouping.column = "sensornode",
  time.column = "sample",
  repetitions = 999,
  parallel.execution = TRUE
  
)





