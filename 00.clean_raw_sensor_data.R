## briefly clean sensor node data
## load the *mostly* raw data
load("raw_data/18.12.19.sennetdata_raw.Rdata")

## add date column
sn.01$date = as.Date(sn.01$TIMESTAMP)


sn.01 = sn.01[, c("TIMESTAMP","sensornode", "soiltemp_5cm_Avg", "soiltemp_30cm_Avg", 
                  "vwca_5cm_Avg", "vwca_30cm_Avg", "vwcb_5cm_Avg", "vwcb_30cm_Avg", 
                  "vwcc_5cm_Avg", "vwcc_30cm_Avg", 
                  "date")]

## remove obviously wrong records from the 5cm sensors should be between 0 and 1 (0% and 100%)
sn.01 = sn.01[sn.01$vwca_5cm_Avg > 0 & sn.01$vwca_5cm_Avg < 1, ]  
sn.01 = sn.01[sn.01$vwcb_5cm_Avg > 0 & sn.01$vwcb_5cm_Avg < 1, ]
sn.01 = sn.01[sn.01$vwcc_5cm_Avg > 0 & sn.01$vwcc_5cm_Avg < 1, ]

## "" from the 30 cm sensors
sn.01 = sn.01[sn.01$vwca_30cm_Avg > 0 & sn.01$vwca_30cm_Avg < 1, ]
sn.01 = sn.01[sn.01$vwcb_30cm_Avg > 0 & sn.01$vwca_30cm_Avg < 1, ]
sn.01 = sn.01[sn.01$vwcc_30cm_Avg > 0 & sn.01$vwca_30cm_Avg < 1, ]

save(file = "data/06.28.20.sennetdata_cleanish.Rdata", sn.01)


