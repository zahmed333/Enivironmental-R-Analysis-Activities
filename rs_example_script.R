library(terra)
setwd("Z:/students/zahmed")

plotRGB(p, r =3, g = 2, b =1)

plotRGB(p, r =3, g = 2, b =1,
        scale = 65535,
        stretch = "hist")

tree <- read.csv("Z:/data/rs_data/siberia_stand_data.csv",
             header = T)

gtree <- vect(tree, geom = c("Long","Lat"), "espg:4326")