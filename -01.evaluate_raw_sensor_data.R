library(ggplot2)

## soil moisture data from 2017-10-17 through 2019-01-16
files1 = list.files("raw_data/knb-lter-nwt.210.1/")

## read all csvs (from first locale)
for (i in 1:length(files1)){
  file1 = read.csv(paste0("/Users/Will/Desktop/knb-lter-nwt.210.1/", files1[i]))
  file1 = file1[, c("date", "sensornode", "airtemp_max", "flag_airtemp_max", "time_airtemp_max", 
                  "airtemp_min", "flag_airtemp_min", "time_airtemp_min", "airtemp_avg", 
                  "flag_airtemp_avg", "rh_max", "flag_rh_max", "time_rh_max", "rh_min", 
                  "flag_rh_min", "time_rh_min", "rh_avg", "flag_rh_avg", "soiltemp_5cm_avg", 
                  "flag_soiltemp_5cm_avg", "soiltemp_30cm_avg", "flag_soiltemp_30cm_avg", 
                  "soilmoisture_a_5cm_avg", "flag_soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", 
                  "flag_soilmoisture_a_30cm_avg", "soilmoisture_b_5cm_avg", "flag_soilmoisture_b_5cm_avg", 
                  "soilmoisture_b_30cm_avg", "flag_soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", 
                  "flag_soilmoisture_c_5cm_avg", "soilmoisture_c_30cm_avg", "flag_soilmoisture_c_30cm_avg")]
  print(colnames(file1))
  if (i == 1){
    holder1 = as.data.frame(matrix(nrow = 0, ncol = dim(file1)[2]))
    colnames(holder1) = colnames(file1)

  }
  
  holder1 = rbind(holder1, file1)
}

table(holder1$sensornode)
holder1 = holder1[!is.na(holder1$date), ]
min(holder1$date)
max(holder1$date)


holder1$date = as.POSIXct(holder1$date, format ="%Y-%m-%d %H:%M:%S")

ggplot(holder1) +
  geom_line(aes(y = soilmoisture_b_5cm_avg, x = date)) +
  facet_wrap(facets = 'sensornode')


# soil moisture data from 2017-08-01 through 2019-12-31                    
files2 = list.files("/Users/Will/Desktop/knb-lter-nwt.210.2/")

for (i in 1:length(files2)){

  file2 = read.csv(paste0("raw_data/knb-lter-nwt.210.2/", files2[i]), stringsAsFactors = FALSE)
  file2 = file2[, c("LTER_site", "local_site", "sensornode", "date", "airtemp_max", 
                     "airtemp_min", "airtemp_avg", "rh_max",  "rh_min", "flag_rh_min", 
                    "rh_avg", "flag_rh_avg", "soiltemp_5cm_avg", "flag_soiltemp_5cm_avg", 
                    "soiltemp_30cm_avg", "flag_soiltemp_30cm_avg", "soilmoisture_a_5cm_avg", 
                    "flag_soilmoisture_a_5cm_avg", "soilmoisture_a_30cm_avg", "flag_soilmoisture_a_30cm_avg", 
                    "soilmoisture_b_5cm_avg", "flag_soilmoisture_b_5cm_avg", "soilmoisture_b_30cm_avg", 
                    "flag_soilmoisture_b_30cm_avg", "soilmoisture_c_5cm_avg", "flag_soilmoisture_c_5cm_avg", 
                    "soilmoisture_c_30cm_avg", "flag_soilmoisture_c_30cm_avg")]
  
  

  if (i == 1){
    holder2 = as.data.frame(matrix(nrow = 0, ncol = dim(file2)[2]))
    colnames(holder2) = colnames(file2)
    
  }
  
  holder2 = rbind(holder2, file2)
}


holder2$date = as.POSIXct(holder2$date, format ="%Y-%m-%d %H:%M:%S")
str(holder2)


table(holder2$sensornode)
table(holder2$flag_soilmoisture_c_30cm_avg)

ggplot(holder2) +
  geom_line(aes(y = soilmoisture_b_5cm_avg, x = date)) +
  geom_line(aes(y = soilmoisture_c_5cm_avg, x = date)) +
  facet_wrap(facets = 'sensornode')

holder2 = holder2[!is.na(holder2$date), ]
min(holder2$date)
max(holder2$date)

table(sensor)


write.csv(holder2, "raw_data/06.07.20.sennetdata_raw.csv", row.names = FALSE)
