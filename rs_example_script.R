#Import terra lib
library(terra)

# sets working dir to ur folder
setwd("Z:/students/zahmed")

# rasterizes data from our file
p <- rast("Z:/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

#plots sattelite data
plot(p)

#wrong bit depth so it doesn't work
plotRGB(p, r =3, g = 2, b =1)

#fixed version below
plotRGB(p, r =3, g = 2, b =1,
        scale = 65535,
        stretch = "hist")

#read the canopy cover
tree <- read.csv("Z:/data/rs_data/siberia_stand_data.csv",
             header = T)

#Convert to vector object using terra package
gtree <- vect(tree, geom = c("Long","Lat"), "epsg:4326")

# project the data to match the coordinate system of the raster layer
gtree2 <- project(gtree, p)

#create polygon from the extent of the points
#We have to specify the crs so it can be carried over again
b <- as.lines(ext(gtree), "epsg:4326")

# re project the polygons to the same projection as our raster
b2 <- project(b, crs(p))

# buffer the extent by 200m
b3 <- buffer(b2, width = 200)
