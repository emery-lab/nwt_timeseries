library(ggplot2)

## briefly clean sensor node data
## load the *mostly* raw data
sn.tmp = read.csv("raw_data/06.07.20.sennetdata_raw.csv")


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


