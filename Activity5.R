#GEOG331 Activity 3
# Z.A. 03/23/2022
library(lubridate)
library(dplyr)
#read in streamflow data
datH <- read.csv("Z:\\data\\streamflow\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)   

datP <- read.csv("Z:\\data\\streamflow\\2049867.csv")                            
head(datP)

datD <- datH[datH$discharge.flag == "A",]

#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)
#calculate month
datD$month <- month(datesD)


#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))


plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))




#QUESTION 5

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Months", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
#2017 line
lines(datD$discharge[datD$year == "2017"],
     col = "blue", pch = 15)

legend("topright", c("mean","1 standard deviation", "2017 stream flow"), #legend items
       lwd=c(3,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "blue"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border






####Question 7###
#Unique dates
datP$uDate <- paste(yday(dateP), year (dateP))


#uses unique dates to calc sum
sumOfHours <- summarise(group_by(datP, uDate), year(dateP))
#label columns
colnames(sumOfHours) <- c("doy", "hours")

#join used from last hw
datP <- left_join(datP, sumOfHours, by =c("uDate" = "doy"))

#Checks which day was measured all day long
datP$allDayM <- ifelse(datP$HR == sum(c(0:23)), "24 hours", "not 24 hours")

#match the same amount of dates as in datP
datD$uDate <- paste(yday(datesD), year(datesD))

#creating the x-axis ---- separated by .
datD$xaxis <- as.numeric(paste(year(datesD), yday(datesD), sep = "."))

datQ7 <- left_join(datD, datP, 
                   by="uDate")

#Plotting the discharge measurements
#Symbolizing days that have precipitation measurements with a green color
plot(datQ7$xaxis, datQ7$discharge,
     main = "24-hour Percipitation Measurement Across Years",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     xlab="Year")

points(datQ7$xaxis[datQ7$allDayM=="24 hours"],
       datQ7$discharge[datQ7$allDayM=="24 hours"],
       col = "blue")











#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]


min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#############QUESTION 8##############

hydroD <- datD[datD$doy >= 53 & datD$doy < 55 & datD$year == 2009,]
hydroP <- datP[datP$doy >= 53 & datP$doy < 55 & datP$year == 2009,]


min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}




###########Question 9##############
library(ggplot2)

#isloate 2016 into data frame
filter16 <- data.frame(datD$discharge[datD$year == 2016])
filter16$doy <- data.frame(datD$doy[datD$year == 2016])
filter16$year <- data.frame(datD$year[datD$year == 2016])
filter16$month <- data.frame(datD$month[datD$year == 2016])

colnames(filter16) <- c("discharge", "doy", "year", "month")

#isloate 2017 into data frame
filter17 <- data.frame(datD$discharge[datD$year==2017],
                      datD$doy[datD$year==2017],
                      datD$year[datD$year==2017],
                      datD$month[datD$year==2017])
colnames(filter17) <- c("discharge", "doy", "year", "month")

#fully filter by season for every
filter16$season <- ifelse(filter16$doy < 32, "Winter",
                       ifelse(filter16$doy < 153, "Spring",
                              ifelse(filter16$doy < 245, "Summer",
                                     ifelse(filter16$doy < 336, "Fall", "Winter"))))

filter17$season <- ifelse(filter17$doy < 32, "Winter",
                       ifelse(filter17$doy < 153, "Spring",
                              ifelse(filter17$doy < 245, "Summer",
                                     ifelse(filter17$doy < 336, "Fall", "Winter"))))
ggplot(data= filter16, aes(x = season, y = discharge, fill = season)) + 
  geom_violin() + 
  ggtitle("2016 Discharge per Season") +
  xlab("Seasons") +
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))
 
ggplot(data= filter17, aes(x = season,y = discharge, fill = season)) + 
  ggtitle("2017 Discharge per Season") +
  geom_violin() + 
  xlab("Seasons") +
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))
 

