
#read the air quality data
datA <- read.csv("~/Desktop/Air_Quality.csv")
head(datA)

#import to reading geojson library
library(geojsonio)
spdf <- geojson_read("~/Desktop/uhf_42_dohmh_2009.geojson",  what = "sp")

#Filter so we only see the Bronx GIS data
spdf <- spdf[spdf$borough == "Bronx", ]

#plot test
library(sp)
par(mar=c(0,0,0,0))
plot(spdf, col="grey")


#Remove old data that is not needed for our time period
library(dplyr)
datA$Time.Period <- na_if(datA$Time.Period, '2005-2007')
na.omit(datA)
is.na(datA$Time.Period)

###########Merging data############

#rename the data so merging can take place
datA = datA %>% rename(
    uhf_neigh = Geo.Place.Name)

# inner joining the 2 datasets to make everything match
#datA = spdf %>% inner_join(datA,by="uhf_neigh", copy = TRUE)
datA <- sp::merge(spdf, datA, by="uhf_neigh", all.x = TRUE, duplicateGeoms=TRUE)
#The final dataset should have everything we need to start exploring
datA

par(mar=c(0,0,0,0))


## I'm just trying to subset so i can try to plot
subsetdatA <- datA[datA$Indicator.ID == "646", ]
#NAs not permitted in row index, I don't know how to fix this


## I would subset for each figure I want to make using the ID
## I will show the toxic molecule and the data value of it too
## I don't even know if this is the right way to plot with gis
library(ggplot2)
ggplot2::ggplot(subsetdatA, aes(fill = subsetdatA$Data.Value)) + 
  ggplot2::geom_sf() + 
  ggplot2::scale_fill_viridis_c() +
  ggplot2::theme_void()
