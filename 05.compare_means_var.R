#### compare means and variances to dissimilarity ####
## Load package 'distantia' 
library(distantia)
## load clean data
file.to.load = list.files("data/")[grep("sn.04", list.files("data/"))]
load(paste0("data/", file.to.load))
rm(sn.04a, sn.04n)
rm(file.to.load)


## get the variance of each node
var.04r = aggregate(sn.04r, by = list(sn.04r$sensornode), FUN = var)

nodes = unique(var.04r$Group.1)
st5 = as.matrix(dist(var.04r$soiltemp_5cm_avg, diag = TRUE, upper = T))
colnames(st5) = nodes
rownames(st5) = nodes
st5

st30 = as.matrix(dist(var.04r$soiltemp_30cm_avg, diag = TRUE, upper = T))
colnames(st30) = nodes
rownames(st30) = nodes
st30

sma5 = as.matrix(dist(var.04r$soilmoisture_a_5cm_avg, diag = TRUE, upper = T))
colnames(sma5) = nodes
rownames(sma5) = nodes
sma5

sma30 = as.matrix(dist(var.04r$soilmoisture_a_30cm_avg, diag = TRUE, upper = T))
colnames(sma30) = nodes
rownames(sma30) = nodes
sma30

smb5 = as.matrix(dist(var.04r$soilmoisture_b_5cm_avg, diag = TRUE, upper = T))
colnames(smb5) = nodes
rownames(smb5) = nodes
smb5

smb30 = as.matrix(dist(var.04r$soilmoisture_b_30cm_avg, diag = TRUE, upper = T))
colnames(smb30) = nodes
rownames(smb30) = nodes
smb30

smc5 = as.matrix(dist(var.04r$soilmoisture_c_5cm_avg, diag = TRUE, upper = T))
colnames(smc5) = nodes
rownames(smc5) = nodes
smc5

smc30 = as.matrix(dist(var.04r$soilmoisture_c_30cm_avg, diag = TRUE, upper = T))
colnames(smc30) = nodes
rownames(smc30) = nodes
smc30

file.name = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".var.04r.Rdata")
save(sma30, sma5, smb30, smb5, smc30, smc5, st30, st5, file = file.name)

rm(sma30, sma5, smb30, smb5, smc30, smc5, st30, st5)
rm(file.name, var.04r)

## get the mean  for each node
mean.04r = aggregate(sn.04r, by = list(sn.04r$sensornode), FUN = mean)

st5 = as.matrix(dist(mean.04r$soiltemp_5cm_avg, diag = TRUE, upper = T))
colnames(st5) = nodes
rownames(st5) = nodes
st5

st30 = as.matrix(dist(mean.04r$soiltemp_30cm_avg, diag = TRUE, upper = T))
colnames(st30) = nodes
rownames(st30) = nodes
st30

sma5 = as.matrix(dist(mean.04r$soilmoisture_a_5cm_avg, diag = TRUE, upper = T))
colnames(sma5) = nodes
rownames(sma5) = nodes
sma5

sma30 = as.matrix(dist(mean.04r$soilmoisture_a_30cm_avg, diag = TRUE, upper = T))
colnames(sma30) = nodes
rownames(sma30) = nodes
sma30

smb5 = as.matrix(dist(mean.04r$soilmoisture_b_5cm_avg, diag = TRUE, upper = T))
colnames(smb5) = nodes
rownames(smb5) = nodes
smb5

smb30 = as.matrix(dist(mean.04r$soilmoisture_b_30cm_avg, diag = TRUE, upper = T))
colnames(smb30) = nodes
rownames(smb30) = nodes
smb30

smc5 = as.matrix(dist(mean.04r$soilmoisture_c_5cm_avg, diag = TRUE, upper = T))
colnames(smc5) = nodes
rownames(smc5) = nodes
smc5

smc30 = as.matrix(dist(mean.04r$soilmoisture_c_30cm_avg, diag = TRUE, upper = T))
colnames(smc30) = nodes
rownames(smc30) = nodes
smc30

file.name = paste0("data/", format(Sys.Date(), "%m.%d.%Y"), ".mean.04r.Rdata")
save(sma30, sma5, smb30, smb5, smc30, smc5, st30, st5, file = file.name)

rm(sma30, sma5, smb30, smb5, smc30, smc5, st30, st5)
rm(file.name, mean.04r)
