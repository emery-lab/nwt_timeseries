## briefly clean sensor node data
## load the *mostly* raw data
sn.tmp = read.csv("raw_data/06.07.20.sennetdata_raw.csv")

# this function is good for viewing the soil moisture at 5 and 30cm for all three nodes. Good to check if they are potentially switched.
plot.raw.sm = function(data = sn.01, sensor = 6){
  sensor.tp = data[data$sensornode %in% sensor, ]
  sensor.tp$date = as.POSIXct(sensor.tp$date, format = "%Y-%m-%d %H:%M:%S")
  
  par(mfrow = c(1, 3), mar = c(4,4,4,1))
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_a_5cm_avg, 
       type = "p", 
       col = "light blue",
       cex = 0.1,
       pch = 19,
       ylim = c(0, 0.6),
       main = "subnode-A",
       xlab = "Date",
       ylab = "Volumetric Soil Moisture",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_a_30cm_avg, 
         col = "blue",
         cex = 0.1,
         pch = 19)
  
  abline(v = as.Date("2019-04-01", format = "%Y-%m-%d"))
  
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_b_5cm_avg, 
       type = "p", 
       col = "pink",
       cex = 0.1,
       pch = 19,
       ylim = c(0, 0.6),
       main = "subnode-B",
       xlab = "Date",
       ylab = "",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_b_30cm_avg, 
         col = "red",
         cex = 0.1,
         pch = 19)
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_c_5cm_avg, 
       type = "p", 
       col = "light green",
       cex = 0.05,
       pch = 19,
       ylim = c(0, 0.6),
       main = "subnode-C",
       xlab = "Date",
       ylab = "",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_c_30cm_avg, 
         col = "dark green",
         cex = 0.05,
         pch = 19)
  
  
}

#### 0. MISC HOUSE KEEPING ####
sn.tmp$TIMESTAMP = as.POSIXct(sn.tmp$date, format ="%Y-%m-%d %H:%M:%S") # convert timestamp to not dumb character
sn.tmp$date = as.Date(sn.tmp$date) # convert date to overall date, for aggregating later

## remove sensors 1 and 18
table(sn.tmp$sensornode)
sn.tmp = sn.tmp[!sn.tmp$sensornode %in% c(1,18), ]
table(sn.tmp$sensornode)

#### 1a.SOIL MOISTURE CLEANING METHOD 1 ####
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
## but I want to check the difference between 1 and 0 and the QA/QC values. 

## between 0 and 1
## remove obviously wrong records from the 5cm sensors should be between 0 and 1 (0% and 100%)
sn.art = sn.tmp[which(sn.tmp$soilmoisture_a_5cm_avg > 0 & sn.tmp$soilmoisture_a_5cm_avg < 1), ]  
sn.art = sn.art[which(sn.art$soilmoisture_b_5cm_avg > 0 & sn.art$soilmoisture_b_5cm_avg < 1), ]  
sn.art = sn.art[which(sn.art$soilmoisture_c_5cm_avg > 0 & sn.art$soilmoisture_c_5cm_avg < 1), ]  

## "" from the 30 cm sensors
sn.art = sn.art[which(sn.art$soilmoisture_a_30cm_avg > 0 & sn.art$soilmoisture_a_30cm_avg < 1), ]  
sn.art = sn.art[which(sn.art$soilmoisture_b_30cm_avg > 0 & sn.art$soilmoisture_b_30cm_avg < 1), ]  
sn.art = sn.art[which(sn.art$soilmoisture_c_30cm_avg > 0 & sn.art$soilmoisture_c_30cm_avg < 1), ]  

## should we remove all records that if one of the three sensors (A, B, C are above/below the thresholds??) ##
sn.real = sn.tmp[which(sn.tmp$soilmoisture_a_5cm_avg > -0.03 & sn.tmp$soilmoisture_a_5cm_avg < 0.6), ]  
sn.real = sn.real[which(sn.real$soilmoisture_b_5cm_avg > -0.03 & sn.real$soilmoisture_b_5cm_avg < 0.6), ]  
sn.real = sn.real[which(sn.real$soilmoisture_c_5cm_avg > -0.03 & sn.real$soilmoisture_c_5cm_avg < 0.6), ]  

## "" from the 30 cm sensors
sn.real = sn.real[which(sn.real$soilmoisture_a_30cm_avg > -0.03 & sn.real$soilmoisture_a_30cm_avg < 0.6), ]  
sn.real = sn.real[which(sn.real$soilmoisture_b_30cm_avg > -0.03 & sn.real$soilmoisture_b_30cm_avg < 0.6), ]  
sn.real = sn.real[which(sn.real$soilmoisture_c_30cm_avg > -0.03 & sn.real$soilmoisture_c_30cm_avg < 0.6), ] 


#### 1b. SOIL MOISTURE CLEANING METHOD 2 ####
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


#### 2a. SOIL TEMP CLEANING METHOD 1 #### 
sn.real = sn.real[which(sn.real$soiltemp_5cm_avg > -35 & sn.real$soiltemp_5cm_avg < 50), ]
sn.real = sn.real[which(sn.real$soiltemp_30cm_avg > -35 & sn.real$soiltemp_30cm_avg < 50), ]

sn.art = sn.art[which(sn.art$soiltemp_5cm_avg > -35 & sn.art$soiltemp_5cm_avg < 50), ]
sn.art = sn.art[which(sn.art$soiltemp_30cm_avg > -35 & sn.art$soiltemp_30cm_avg < 50), ]

#### 2b. SOIL TEMP CLEANING METHOD 2 ####
sn.2$soiltemp_5cm_avg = ifelse(sn.2$soiltemp_5cm_avg > -35 & sn.2$soiltemp_5cm_avg < 50, sn.2$soiltemp_5cm_avg, NA)
sn.2$soiltemp_30cm_avg = ifelse(sn.2$soiltemp_30cm_avg > -35 & sn.2$soiltemp_30cm_avg < 50, sn.2$soiltemp_30cm_avg, NA)


#### 3. REMOVE FLAG COLUMNS ####
dput(names(sn.2))
include = c("LTER_site", "local_site", "sensornode", "date","soiltemp_5cm_avg", 
            "soiltemp_30cm_avg", "soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", 
            "soilmoisture_b_5cm_avg", "soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", 
            "soilmoisture_c_30cm_avg", "TIMESTAMP")

## remove columns we don't want
sn.2 = sn.2[, include]
sn.real = sn.real[, include]
sn.art = sn.art[, include]

#### 4. RENAME AND SAVE .RDATA file ####
## This is better than CSVs because CSVs are effin huge. 
sn.01n = sn.2
sn.01r = sn.real 
sn.01a = sn.art

file.name1 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.01.Rdata")
save(sn.01a, sn.01r, sn.01n, file = file.name1)
rm(sn.2, sn.real, sn.art, sn.tmp, include, file.name1) # clean up the workspace


#### 5. SUBSET TO GROWING SEASON ####
## Do artificial clean first (sn.01a)
sn.2018 = sn.01a[sn.01a$TIMESTAMP > as.POSIXct("2018-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                 sn.01a$TIMESTAMP < as.POSIXct("2018-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2018$season = 2018


sn.2019 = sn.01a[sn.01a$TIMESTAMP > as.POSIXct("2019-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                 sn.01a$TIMESTAMP < as.POSIXct("2019-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2019$season = 2019

sn.02a = rbind(sn.2018, sn.2019)
rm(sn.2018, sn.2019)

## Do real next (sn.01r)
sn.2018 = sn.01r[sn.01r$TIMESTAMP > as.POSIXct("2018-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                   sn.01r$TIMESTAMP < as.POSIXct("2018-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2018$season = 2018


sn.2019 = sn.01r[sn.01r$TIMESTAMP > as.POSIXct("2019-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                   sn.01r$TIMESTAMP < as.POSIXct("2019-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2019$season = 2019

sn.02r = rbind(sn.2018, sn.2019)
rm(sn.2018, sn.2019)

## lastly do the NA method (sn.01n)
sn.2018 = sn.01n[sn.01n$TIMESTAMP > as.POSIXct("2018-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                   sn.01n$TIMESTAMP < as.POSIXct("2018-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2018$season = 2018


sn.2019 = sn.01n[sn.01n$TIMESTAMP > as.POSIXct("2019-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                   sn.01n$TIMESTAMP < as.POSIXct("2019-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]
sn.2019$season = 2019

sn.02n = rbind(sn.2018, sn.2019)
rm(sn.2018, sn.2019)

## Save these as .Rdata
file.name2 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.02.Rdata")
save(sn.02a, sn.02r, sn.02n, file = file.name2)
rm(file.name2)

#### 6. Clear environment ####
## uncomment if  you want to check these objects
## otherwise skip to #8 to aggregate and get daily
## data frames
#rm(list = ls())

#### 7. Load objects to see ####
#load("data/08.17.20.sn.01.Rdata")
#load("data/08.17.20.sn.02.Rdata")

#### 8a. MAKE AGGREGATED DATA FRAMES (full season) ####
colnames(sn.01a) # see what the names are, should be the same for all sn.02 dfs 
## do a first (arbitrary)
sn.03a = aggregate(sn.01a[, -which(names(sn.01a) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.01a$sensornode, sn.01a$date), FUN = mean) ## only do columns where a mean makes sense (i.e not LTER_site)
sn.03a = sn.03a[, -which(names(sn.03a) %in% c("Group.1", "Group.2"))] # get rid of the stupid aggregate columns. thisis a dumb feature of this function, maybe I'm doing it wrong?

## do r next (real)
## see above comments for workins
sn.03r = aggregate(sn.01r[, -which(names(sn.01r) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.01r$sensornode, sn.01r$date), FUN = mean)
sn.03r = sn.03r[, -which(names(sn.03r) %in% c("Group.1", "Group.2"))]

## do n next (input NA's)
## see above comments for workins
sn.03n = aggregate(sn.01n[, -which(names(sn.01n) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.01n$sensornode, sn.01n$date), FUN = mean)
sn.03n = sn.03n[, -which(names(sn.03n) %in% c("Group.1", "Group.2"))]

file.name3 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.03.Rdata")
save(sn.03a, sn.03r, sn.03n, file = file.name3)
rm(file.name3)


#### 8b. MAKE AGGREGATED DATA FRAMES (growing season) ####
colnames(sn.02a) # see what the names are, should be the same for all sn.02 dfs 
## do a first (arbitrary)
sn.04a = aggregate(sn.02a[, -which(names(sn.02a) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.02a$sensornode, sn.02a$date), FUN = mean) ## only do columns where a mean makes sense (i.e not LTER_site)
sn.04a = sn.04a[, -which(names(sn.04a) %in% c("Group.1", "Group.2"))] # get rid of the stupid aggregate columns. thisis a dumb feature of this function, maybe I'm doing it wrong?

## do r next (real)
## see above comments for workins
sn.04r = aggregate(sn.02r[, -which(names(sn.02r) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.02r$sensornode, sn.02r$date), FUN = mean)
sn.04r = sn.04r[, -which(names(sn.04r) %in% c("Group.1", "Group.2"))]

## do n next (input NA's)
## see above comments for workins
sn.04n = aggregate(sn.02n[, -which(names(sn.02n) %in% c("LTER_site", "local_site", "TIMESTAMP"))], list(sn.02n$sensornode, sn.02n$date), FUN = mean)
sn.04n = sn.04n[, -which(names(sn.04n) %in% c("Group.1", "Group.2"))]

file.name4 = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".sn.04.Rdata")
save(sn.04a, sn.04r, sn.04n, file = file.name4)
rm(file.name4)

#### 9. ACTUALLY CLEAR ENVIRONMENT ####
rm(list = ls())


