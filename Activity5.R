#GEOG331 Activity 3
# Z.A. 03/23/2022
library(lubridate)
#read in streamflow data
datH <- read.csv("Z:\\data\\streamflow\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)   

datP <- read.csv("data/2049867.csv")                            
head(datP)