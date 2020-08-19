## 01.compare_cleaning_methods.R
## This script checks out the differences in Psi from the different cleaning methods. 
## OPTIONS:
## A. arbitrary (removes sm between 0 and 1)
## B. real (removes sm out of detection limits -0.03 and 0.60), why would it be negative tho??
## C. NA insertion (doesn't remove the whole record for that timestamp, just inserts an NA in the problem column)
library(distantia)

## load file (.Rdata, three objects)
file.to.load = list.files("data/")[grep("sn.03", list.files("data/"))]
load(paste0("data/", file.to.load))
rm(file.to.load)

## add a 'time' column, because distantia can't handle dates. Dumb.
dates.a = seq(min(sn.03a$date), max(sn.03a$date), by = 1) # range of dates
dates.n = seq(min(sn.03n$date), max(sn.03n$date), by = 1) 
dates.r = seq(min(sn.03r$date), max(sn.03r$date), by = 1) 
## PERFECT! (they are all the same :)...jk, one is different..) I think its okay to just use 883?

sample1 = 1:883 # there are 882 'replace' dates with this
dates1 = as.data.frame(dates.n) # turn into data frame or R will lose its mind (turns dates into nonsense integers)
time.df1 = cbind(dates1, sample1) # full df with actual date and useable sample column
colnames(time.df1) = c("dates", "sample")

sample2 = 1:882
dates2 = as.data.frame(dates.a)
time.df2 = cbind(dates2, sample2)
colnames(time.df2) = c("dates", "sample")

sample3 = 1:882
dates3 = as.data.frame(dates.r)
time.df3 = cbind(dates3, sample3)
colnames(time.df3) = c("dates", "sample")

rm(dates.n, 
   dates.r, 
   dates.a,
   dates1,
   dates2,
   dates3,
   sample1,
   sample2,
   sample3) 

## merge all three data frames with the time.df
sn.03a = merge(sn.03a, time.df2, by.x = "date", by.y = "dates", all.x = TRUE)
sn.03n = merge(sn.03n, time.df1, by.x = "date", by.y = "dates", all.x = TRUE)
sn.03r = merge(sn.03r, time.df3, by.x = "date", by.y = "dates", all.x = TRUE)

rm(time.df1, time.df2, time.df3)

## check to see that they all got a dates
any(is.na(sn.03a$sample))
any(is.na(sn.03n$sample))
any(is.na(sn.03r$sample))

sn.03a = sn.03a[, -which(names(sn.03a) %in% c("date"))]
sn.03n = sn.03n[, -which(names(sn.03n) %in% c("date"))]
sn.03r = sn.03r[, -which(names(sn.03r) %in% c("date"))]

#### 1. PREPARE SEQUENCES ####ß
## prepare .a 
seq.03a = prepareSequences(
  sequences = sn.03a,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)

## prepare .n
seq.03n = prepareSequences(
  sequences = sn.03n,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)

## prepare .r
seq.03r = prepareSequences(
  sequences = sn.03r,
  grouping.column = "sensornode",
  time.column = "sample",
  if.empty.cases = "omit"
)

#### 2. COMPUTE PSI METRICS ####
## psi for .a
psi.03a = workflowPsiHP(
  sequences =  seq.03a,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

## psi for .n
psi.03n = workflowPsiHP(
  sequences =  seq.03n,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

## psi for .r
psi.03r = workflowPsiHP(
  sequences =  seq.03r,
  grouping.column = "sensornode",
  time.column = "sample",
  parallel.execution = TRUE
  
)

rm(seq.03a, seq.03n, seq.03r)

## check to see how it orgainizes the sensor nodes
## this might be a pain.
table(psi.03a$A)
table(psi.03r$A)
table(psi.03n$A)

#### 3. MERGE PSI VALUES ####
psi.03.an = merge.psi(psi.03a, psi.03n)
colnames(psi.03.an) = c("A", "B", "psi.a", "psi.n")

psi.03.anr = merge.psi(psi.03.an, psi.03r)
colnames(psi.03.anr) = c("A", "B", "psi.a", "psi.n", "psi.r")

rm(psi.03.an)

#### 4. PLOT PSI VALUES ####
par(mfrow = c(1,3))
plot(psi.03.anr$psi.a ~ psi.03.anr$psi.n,
     xlab = "psi.n",
     ylab = "psi.a")

plot(psi.03.anr$psi.r ~ psi.03.anr$psi.n,
     xlab = "psi.n",
     ylab = "psi.r")

plot(psi.03.anr$psi.a ~ psi.03.anr$psi.r,
     xlab = "psi.r",
     ylab = "psi.a")

#### 
rm(list = ls())



## based on these plots it doesn't really seem to matter between psi.a
## (arbitrary vs. real cleaning methods). However, the NA insertion method
## does seem to have an effect (at least a little) It would be interesting
## to see how this interacts with the empty.case because I think that might
## remove everything if there is a single missing record in any of the time
## series for that time point. 
## 
## for now I'm going to use the real cleaning method.. talk to Meagan 
## about this tomorrow! 
