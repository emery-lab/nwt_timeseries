## briefly clean sensor node data
## load the *mostly* raw data
## after talking to Meagan, We've decided to move to only the real method of cleaning, where we remove
## data above and below 0.6 and -0.03 respectivley 
source("functions.R")
sn.tmp = read.csv("raw_data/06.07.20.sennetdata_raw.csv")

#### 0. MISC HOUSE KEEPING ####
sn.tmp$TIMESTAMP = as.POSIXct(sn.tmp$date, format ="%Y-%m-%d %H:%M:%S") # convert timestamp to not dumb character
sn.tmp$date = as.Date(sn.tmp$date) # convert date to overall date, for aggregating later


## remove sensors 1 and 18
table(sn.tmp$sensornode)
sn.tmp = sn.tmp[!sn.tmp$sensornode %in% c(1,15,18), ]
table(sn.tmp$sensornode)

#### 1. SOIL MOISTURE CLEANING ####
# str(sn.tmp)
# table(sn.tmp$soilmoisture_a_5cm_avg > 0, sn.tmp$sensornode)
# table(sn.tmp$soilmoisture_a_5cm_avg < 1, sn.tmp$sensornode)
##               1      6      7      8      9     10     11     12     13     14     15     16     17     19     20     21
## Below 0       0    459      1    995      0    306      0      1      0      0      0   7659      0      0      0      0
## Above 0   21148  71879 115140 117958 100862 110678 107624 108570  92341 113998 113235 106494  86544  87453 117241 103643

#               1      6      7      8      9     10     11     12     13     14     15     16     17     19     20     21
# Above 1       0      0      0   4541      0      0      0      0      0      0      0      0      0      0      0      0
# Below 1   21148  72338 115141 114412 100862 110984 107624 108571  92341 113998 113235 114153  86544  87453 117241 103643


## the QA/QC (flag) column for each record indicates that the sensor detection is between -0.03 and 0.60
sn.2 = sn.tmp 
## 5cm
sn.2$soilmoisture_a_5cm_avg = ifelse(sn.2$soilmoisture_a_5cm_avg > -0.03 & sn.2$soilmoisture_a_5cm_avg < 0.6, sn.2$soilmoisture_a_5cm_avg, NA)              
sn.2$soilmoisture_b_5cm_avg = ifelse(sn.2$soilmoisture_b_5cm_avg > -0.03 & sn.2$soilmoisture_b_5cm_avg < 0.6, sn.2$soilmoisture_b_5cm_avg, NA) 
sn.2$soilmoisture_c_5cm_avg = ifelse(sn.2$soilmoisture_c_5cm_avg > -0.03 & sn.2$soilmoisture_c_5cm_avg < 0.6, sn.2$soilmoisture_c_5cm_avg, NA)

## 30 cm 
sn.2$soilmoisture_a_30cm_avg = ifelse(sn.2$soilmoisture_a_30cm_avg > -0.03 & sn.2$soilmoisture_a_30cm_avg < 0.6, sn.2$soilmoisture_a_30cm_avg, NA)              
sn.2$soilmoisture_b_30cm_avg = ifelse(sn.2$soilmoisture_b_30cm_avg > -0.03 & sn.2$soilmoisture_b_30cm_avg < 0.6, sn.2$soilmoisture_b_30cm_avg, NA) 
sn.2$soilmoisture_c_30cm_avg = ifelse(sn.2$soilmoisture_c_30cm_avg > -0.03 & sn.2$soilmoisture_c_30cm_avg < 0.6, sn.2$soilmoisture_c_30cm_avg, NA)

## spot check/ test the function (mostly number 2)
#plot.raw.sm(sn.real, 9)
#plot.raw.sm(sn.art, 9)


#### 2. SOIL TEMP CLEANING  ####
sn.2$soiltemp_5cm_avg = ifelse(sn.2$soiltemp_5cm_avg > -35 & sn.2$soiltemp_5cm_avg < 50, sn.2$soiltemp_5cm_avg, NA)
sn.2$soiltemp_30cm_avg = ifelse(sn.2$soiltemp_30cm_avg > -35 & sn.2$soiltemp_30cm_avg < 50, sn.2$soiltemp_30cm_avg, NA)


#### 3. REMOVE FLAG COLUMNS ####
# dput(names(sn.2))
include = c("LTER_site", "local_site", "sensornode", "date","soiltemp_5cm_avg", 
            "soiltemp_30cm_avg", "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", 
            "soilmoisture_b_5cm_avg", "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", 
            "soilmoisture_c_30cm_avg", "TIMESTAMP")

## remove columns we don't want
sn.2 = sn.2[, include]

#### 4. RENAME ####
sn.01 = sn.2
rm(sn.2, sn.tmp, include)

#### 5. SUBSET TO GROWING SEASON ####
## Do artificial clean first (sn.01a)
sn.2018 = sn.01[sn.01$TIMESTAMP > as.POSIXct("2018-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                 sn.01$TIMESTAMP < as.POSIXct("2018-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2018$season = 2018


sn.2019 = sn.01[sn.01$TIMESTAMP > as.POSIXct("2019-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                 sn.01$TIMESTAMP < as.POSIXct("2019-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2019$season = 2019

sn.02 = rbind(sn.2018, sn.2019)
rm(sn.2018, sn.2019)

#### 6. SUBSET TO WINTER ####
## Just using inverse dates of growing season

sn.2018 = sn.01[sn.01$TIMESTAMP < as.POSIXct("2018-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                  sn.01$TIMESTAMP > as.POSIXct("2017-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2018$season = 2018


sn.2019 = sn.01[sn.01$TIMESTAMP < as.POSIXct("2019-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                  sn.01$TIMESTAMP > as.POSIXct("2018-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2019$season = 2019

sn.03 = rbind(sn.2018, sn.2019)
rm(sn.2018, sn.2019)

#### 8a. MAKE AGGREGATED DATA FRAMES (full season) ####
colnames(sn.01) # see what the names are, should be the same for all sn.02 dfs 
sn.04 = aggregate(sn.01[, -which(names(sn.01) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.01$sensornode, sn.01$date), FUN = mean)
sn.04 = sn.04[, -which(names(sn.04) %in% c("Group.1", "Group.2"))]


#### 8b. MAKE AGGREGATED DATA FRAMES (growing season) ####
colnames(sn.02) # see what the names are, should be the same for all sn.02 dfs 
sn.05 = aggregate(sn.02[, -which(names(sn.02) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.02$sensornode, sn.02$date), FUN = mean)
sn.05 = sn.05[, -which(names(sn.05) %in% c("Group.1", "Group.2"))]

#### 8c. MAKE AGGREGATED DATA FRAMES (Winter) ####
colnames(sn.03) # see what the names are, should be the same for all sn.02 dfs 
sn.06 = aggregate(sn.03[, -which(names(sn.03) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.03$sensornode, sn.03$date), FUN = mean)
sn.06 = sn.06[, -which(names(sn.06) %in% c("Group.1", "Group.2"))]


#### 10. SCALE AND SAVE ####
## sn.01 - Scale it to mean 0; sd 1
## set up factors 
sn.01$sensornode = as.factor(sn.01$sensornode)

sn.01s = scale.df(sn.01)
file.name1 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.01.Rdata")
old.file1 = list.files("data/")[grep("sn.01", list.files("data/"))]
file.remove(paste0("data/", old.file1))
save(sn.01, sn.01s, file = file.name1)
rm(file.name1, old.file1) # clean up the workspace

## sn.02 - Scale it to mean 0; sd 1
sn.02$sensornode = as.factor(sn.02$sensornode)

sn.02s = scale.df(sn.02)
## Save these as .Rdata
file.name2 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.02.Rdata")
old.file2 = list.files("data/")[grep("sn.02", list.files("data/"))]
file.remove(paste0("data/",old.file2))
save(sn.02, sn.02s, file = file.name2)
rm(file.name2, old.file2)

## sn.03 -  Scale it to mean 0; sd 1
sn.03$sensornode = as.factor(sn.03$sensornode)

sn.03s = scale.df(sn.03)
file.name3 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.03.Rdata")
old.file3 = list.files("data/")[grep("sn.03", list.files("data/"))]
file.remove(paste0("data/",old.file3))
save(sn.03, sn.03s, file = file.name3)
rm(file.name3, old.file3)

## sn.04 - Scale it to mean 0; sd 1
sn.04$sensornode = as.factor(sn.04$sensornode)

sn.04s = scale.df(sn.04)
file.name4 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.04.Rdata")
old.file4 = list.files("data/")[grep("sn.04", list.files("data/"))]
file.remove(paste0("data/",old.file4))
save(sn.04, sn.04s, file = file.name4)
rm(file.name4, old.file4)

## sn.05 Scale it to mean 0; sd 1
sn.05$sensornode = as.factor(sn.05$sensornode)
sn.05$season = as.factor(sn.05$season)

sn.05s = scale.df(sn.05)
file.name5 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.05.Rdata")
old.file5 = list.files("data/")[grep("sn.05", list.files("data/"))]
file.remove(paste0("data/",old.file5))
save(sn.05, sn.05s, file = file.name5)
rm(file.name5, old.file5)

## sn.06 - Scale it to mean 0; sd 1
sn.06$sensornode = as.factor(sn.06$sensornode)
sn.06$season = as.factor(sn.06$season)

sn.06s = scale.df(sn.06)
file.name6 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.06.Rdata")
old.file6 = list.files("data/")[grep("sn.06", list.files("data/"))]
file.remove(paste0("data/",old.file6))
save(sn.06, sn.06s, file = file.name6)
rm(file.name6, old.file6)

#### 11. ACTUALLY CLEAR ENVIRONMENT ####
rm(list = ls())


