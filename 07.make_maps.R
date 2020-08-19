library(elevatr)
library(raster)


locs = read.csv("data/06.29.2020.sensorlocations.csv")
min(locs$long)
max(locs$long)
min(locs$lat)
max(locs$lat)

# Generate a data frame of lat/long coordinates.
ex.df <- data.frame(x=seq(from=-105.596, to=-105.587, length.out=100), 
                    y=seq(from=40.048, to=40.057, length.out=100))

# Specify projection.
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Use elevatr package to get elevation data for each point.
elev <- get_elev_raster(ex.df, prj = prj_dd, z = 10, clip = "bbox")

png("figures/sensorMap.png", height = 6, width = 6, res = 200, units = "in")
par(mfrow = c(1,1), mar = c(4.5,4.5,1,1))
raster::contour(elev,
                maxpixels = 100000,
                asp = 1, 
                xlab = "Longitude",
                ylab = "Latitude", 
                xaxt = "n", 
                yaxt = "n",
                lty = 2, 
                ylim = c(40.049, 40.056),
                xlim = c(-105.5955, -105.58725),
                cex.lab = 1.5,
                col = rgb(0, 0, 0, 0.5))
points(x = locs$long, y = locs$lat, pch = 21, bg = "gray", col = "black", cex = 1.3)
text(x = locs$long, y = locs$lat, labels = locs$sensor, pos = 3, cex = 0.6)
axis(1)
axis(2)
dev.off()
