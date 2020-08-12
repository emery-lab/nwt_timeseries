library(ggplot2)

## briefly clean sensor node data
## load the *mostly* raw data
sn.tmp = read.csv("raw_data/06.07.20.sennetdata_raw.csv")

# this function is good for viewing the soil moisture at 5 and 30cm for all three nodes. Good to check if they are potentially switched.
plot.raw.sm = function(data = sn.01, sensor = 6){
  sensor.tp = data[data$sensornode %in% sensor, ]
  sensor.tp$date = as.POSIXct(sensor.tp$date, format = "%Y-%m-%d %H:%M:%S")
  
  par(mfrow = c(1, 3))
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_a_5cm_avg, 
       type = "p", 
       col = "light blue",
       cex = 0.1,
       pch = 19,
       ylim = c(0, 1),
       main = "subnode-A",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_a_30cm_avg, 
         col = "blue",
         cex = 0.1,
         pch = 19)
  
  flag.a5 = sensor.tp[sensor.tp$flag_soilmoisture_a_5cm_avg == "s", ]
  print(head(flag.a5))
  
  points(x = flag.a5$date,
         y = flag.a5$soilmoisture_a_30cm_avg,
         col = "black",
         cex = 0.5)
  
  
  plot(x = sensor.tp$date, 
       y = sensor.tp$soilmoisture_b_5cm_avg, 
       type = "p", 
       col = "pink",
       cex = 0.1,
       pch = 19,
       ylim = c(0, 1),
       main = "subnode-B",
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
       cex = 0.1,
       pch = 19,
       ylim = c(0, 1),
       main = "subnode-C",
       cex.main = 2)
  
  points(x = sensor.tp$date, 
         y = sensor.tp$soilmoisture_c_30cm_avg, 
         col = "dark green",
         cex = 0.1,
         pch = 19)
  
  
}

# check one out. 
plot.raw.sm(data = sn.tmp, sensor = 8)




sn.tmp

sn.tmp$TIMESTAMP = as.POSIXct(sn.tmp$date, format ="%Y-%m-%d %H:%M:%S")
#sn.tmp = sn.tmp[order(sn.tmp$sensornode, sn.tmp$TIMESTAMP), ]


str(sn.tmp)
table(sn.tmp$soilmoisture_a_5cm_avg > 0)
table(sn.tmp$soilmoisture_a_5cm_avg < 1)

## remove obviously wrong records from the 5cm sensors should be between 0 and 1 (0% and 100%)
sn.tmp = sn.tmp[which(sn.tmp$soilmoisture_a_5cm_avg > 0 & sn.tmp$soilmoisture_a_5cm_avg < 1), ]  
sn.tmp = sn.tmp[which(sn.tmp$soilmoisture_b_5cm_avg > 0 & sn.tmp$soilmoisture_b_5cm_avg < 1), ]  
sn.tmp = sn.tmp[which(sn.tmp$soilmoisture_c_5cm_avg > 0 & sn.tmp$soilmoisture_c_5cm_avg < 1), ]  


## "" from the 30 cm sensors
sn.tmp = sn.tmp[which(sn.tmp$soilmoisture_a_30cm_avg > 0 & sn.tmp$soilmoisture_a_30cm_avg < 1), ]  
sn.tmp = sn.tmp[which(sn.tmp$soilmoisture_b_30cm_avg > 0 & sn.tmp$soilmoisture_b_30cm_avg < 1), ]  
sn.tmp = sn.tmp[which(sn.tmp$soilmoisture_c_30cm_avg > 0 & sn.tmp$soilmoisture_c_30cm_avg < 1), ]  


# initial plotting. 
# ggplot(sn.tmp) +
#   geom_point(aes(y = soilmoisture_a_5cm_avg, x = TIMESTAMP)) +
#   geom_point(aes(y = soilmoisture_b_5cm_avg, x = TIMESTAMP)) +
#   geom_point(aes(y = soilmoisture_c_5cm_avg, x = TIMESTAMP)) +
#   facet_wrap(facets = 'sensornode')
# 
# ggplot(sn.tmp) +
#   geom_line(aes(y = soilmoisture_a_5cm_avg, x = TIMESTAMP)) +
#   geom_line(aes(y = soilmoisture_b_5cm_avg, x = TIMESTAMP)) +
#   geom_line(aes(y = soilmoisture_c_5cm_avg, x = TIMESTAMP)) +
#   facet_wrap(facets = 'sensornode')


write.csv(sn.tmp, file = "data/07.08.20.sennetdata_cleanish.csv", row.names = FALSE)

## subset dates 
min(sn.tmp$TIMESTAMP)

sn.2018 = sn.tmp[sn.tmp$TIMESTAMP > as.POSIXct("2018-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                 sn.tmp$TIMESTAMP < as.POSIXct("2018-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]

sn.2019 = sn.tmp[sn.tmp$TIMESTAMP > as.POSIXct("2019-04-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") &
                 sn.tmp$TIMESTAMP < as.POSIXct("2019-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), ]

sn.grow = rbind(sn.2018, sn.2019)
write.csv(sn.grow, file = "data/07.19.20.sennetdata_growing_season.csv", row.names = FALSE)
write.csv(sn.2018, file = "data/07.19.20.sennetdata_2018.csv", row.names = FALSE)
write.csv(sn.2019, file = "data/07.19.20.sennetdata_2019.csv", row.names = FALSE)


