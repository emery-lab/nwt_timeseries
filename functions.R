#### plot.raw.sm() ####
## this function is good for viewing the soil moisture at 5 and 30cm for all three nodes. 
## Good to check if they are potentially switched.
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

#### merge.psi() ####
## This function will merge two psi dataframes even if the sensors are not
## in the same columns. Useful because distantia does it randomly apparently??
merge.psi = function(df1, df2, col1 = "A", col2 = "B"){
  if(nrow(df1) != nrow(df2)){
    stop("dataframes do not have the same number of rows")
  }
  
  ncols1 = dim(df1)[2]
  ncols2 = dim(df2)[2]
  
  names = names(df1)
  
  df1[, ncols1+1] = NA
  colnames(df1) = c(names, "ADDED.PSI")
  
  
  for(i in 1:nrow(df1)){
    x = df1[i, col1]
    y = df1[i, col2]
    
    for(j in 1:nrow(df2)){
      
      if(df2[j, col1] == x & df2[j, col2] == y){
        df1[i, "ADDED.PSI"] = df2[j, ncols2]  
        
      } else if(df2[j, col1] == y & df2[j, col2] == x){
        df1[i, "ADDED.PSI"] = df2[j, ncols2]  
        
      }
      
      
    }
    
    
    
  }
  message("Psi values merged, new psi labeled as 'ADDED.PSI'")
  return(df1)
}


#### scale.df() ####
## scales an entire df, or atleast the numeric columns
scale.df = function(df){
  for (i in 1:ncol(df)){
    if(is.numeric(df[, i])){
      df[,i] = as.numeric(scale(df[, i]))
    }
    
  }
  return(df)
}

#### do.psi() ####
## this function condenses the workflow of calculating psi, and modifying the
## data frames to work with the distantia functions. I got sick of copying and
## pasting all that code. 

do.psi = function(data, add.time = TRUE){
  
  # first add time column if needed
  # this does integer timing (counts) since distantia can't handle real dates
  # need real dates though. 
  if(add.time == TRUE){
    dates = seq(min(data$date), max(data$date), by = 1) # range of dates
    sample = 1:length(dates)
    dates = as.data.frame(dates)
    time.df = cbind(dates, sample) 
    
    # merge times with the dataframe using 'date' column
    data = merge(data, time.df, by.x = "date", by.y = "dates", all.x = TRUE)
    
  }
  
  # now get rid  of the date column
  data = data[, -which(names(data) %in% c("date"))]
  
  # prepate sequences - from distantia
  sequence = prepareSequences(
    sequences = data,
    grouping.column = "sensornode",
    time.column = "sample",
    if.empty.cases = "omit"
  )
  
  # calculate psi 
  psi = workflowPsiHP(
    sequences =  sequence,
    grouping.column = "sensornode",
    time.column = "sample",
    parallel.execution = TRUE
    
  )
  
  # plot the distribution of psi, although this is density.
  plot(density(psi$psi))
  return(psi)
  
  
}


