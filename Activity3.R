#GEOG331 Activity 3
# Z.A. 02/21/2022

#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("Z:\\data\\bewkes\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:\\data\\bewkes\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#use install.packages to install lubridate
#install.packages(c("lubridate"))

library(lubridate)
#commented out afterwards

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")


#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularly confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

# #check the values at the extreme range of the data
# #and throughout the percentiles
# quantile(datW$air.tempQ1)
# 
# #look at days with really low air temperature
# datW[datW$air.tempQ1 < 8,] 
# 
# #look at days with really high air temperature
# datW[datW$air.tempQ1 > 33,] 


#QUESTION 5

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


#proving you can subset values not in the datW dataframe
assert(length(datW$precipitation) == length(datW$lightning.acvitivy),
       "error")


#QUESTION 6
#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))



# wind speed repetition
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#assert check to check for NA values
windSNA <- which(is.na(datW$wind.speedQ1))
airTNA <- which(is.na(datW$air.tempQ2))

# Counter to loop through the values in windSNA
counter <- 1
#For loop to loop through and check if all locations of NA match both
#Wind speed and Air Temp that we filtered
for (val in windSNA) {
  assert(windSNA[counter] == airTNA[counter],
         "there seems to be a suspicious value")
  counter = counter + 1
}
  




#make it empty to start and add in features
plot(datW$doy , datW$wind.speedQ1, xlab = "Day of Year", ylab = "wind speed",
     type="o")
#plot precipitation points only when there is wind speed 
#make the points semi-transparent
points(datW$doy, datW$wind.speedQ1,
       col= rgb(95/255,158/255,160/255,.5), pch=15)
#adding a line of best fit.
abline(lm(datW$wind.speedQ1 ~ datW$doy))


#Question 7

# check days leading up to outage
# Manually comparing the data in the frame
datW[datW$doy > 183,]
datW[datW$doy > 184,]
datW[datW$doy > 186,]
datW[datW$doy > 188,]
datW[datW$doy > 190,]
datW[datW$doy > 191,]

# compare using the dates with the plots aligned
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#question 8
# average calculations
#right amount of digits according to amount of numbers by rounding
  avgWS <- round(mean(datW$wind.speed, na.rm = TRUE), digits = 2)
  avgAT <- round(mean(datW$air.temperature, na.rm = TRUE), digits = 1)
  avgSM <- round(mean(datW$soil.moisture, na.rm = TRUE), digits = 3)
  avgST <- round(mean(datW$soil.temp, na.rm = TRUE), digits = 1)
  avgP <- round(mean(datW$precipitation, na.rm = TRUE), digits = 3)

#creating the data frame with all the averages
  requestedAvgTable <- data.frame(avgWS, avgAT, avgSM, avgST, avgP)
  
#Question 9

#4 way graph
par(mfrow=c(2,2))

# compare using the dates with the plots aligned
#ylab describes which plot it is
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil temperature (degrees C)")

plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")

