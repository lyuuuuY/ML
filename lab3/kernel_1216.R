set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

#smooth coefficients/width
h_distance <- 400000    # These three values are up to the students
h_date <-30
h_time <-2

#latitude to be predicted  
a <- 58.4274  # The point to predict (up to the students)
#longitude to be predicted  
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)

#times <- c("04:00:00", "06:00:00", ..., "24:00:00")
#temp <- vector(length=length(times))
datetime <- seq(from=as.POSIXct("2013-11-04 04:00:00"),
                to=as.POSIXct("2013-11-04 24:00:00"),
                by="2 hours")

temp <- vector(length=length(datetime))


#Combines date and time into a datetime object
st$datetime <- as.POSIXct(paste(st$date,st$time),format="%Y-%m-%d %H:%M:%S")
#set a filter 
filter <- as.POSIXct("2013-11-04 04:00:00",format="%Y-%m-%d %H:%M:%S")
st_filtered <- st[st$datetime<filter,]


#PHYSICAL kernel

library(geosphere)

 #make sure c(lon,lat)
target_point <- c(b,a) 

latitude <- st_filtered$latitude
longitude <- st_filtered$longitude

 #compute the distance for each station
distance <- mapply(function(lon,lat){
  geosphere::distHaversine(c(lon,lat),target_point)},longitude,latitude
)
st_filtered$distance <- distance

 #check if there is NA in col distance and remove them
sum(is.na(st_filtered$distance))  
st_filtered <- subset(st_filtered,!is.na(distance))


 #compute the physical distance kernel value
k_distance <- exp(-st_filtered$distance^2/(2*h_distance^2))
st_filtered$k_distance <- k_distance
st_filtered <- st_filtered[!is.na(st_filtered$distance), ]

 #A plot of the kernel value as a function of physical distance
plot(st_filtered$distance,st_filtered$k_distance,type="p",xlab="physical distance"
     ,ylab="kernel value",main="Kernel Value as a Function of Distance")

#DAY kernel

 #convert the history date into days in a year
st_filtered$converted_date<- as.numeric(format(st_filtered$datetime,"%j"))
 #convert the prediction date into days in a year
date <- as.POSIXct(date)
predict_converted <- as.numeric(format(date,"%j"))

 #compute the day distance for each data
Raw_day_Distance <- abs(st_filtered$converted_date-predict_converted)
st_filtered$day_distance <- pmin(Raw_day_Distance,365-Raw_day_Distance)

 #compute day kernel value
h_date <-30
k_Daydistance <- exp(-st_filtered$day_distance^2/(2*h_date^2))
st_filtered$k_Daydistance <- k_Daydistance

#plot
plot(st_filtered$day_distance,st_filtered$k_Daydistance,type="p",xlab="day distance"
     ,ylab="kernel value",main="Kernel Value as a Function of Day Distance")


#HOUR kernal

#extract history and predict hour
st_filtered$hour<- as.numeric(format(st_filtered$datetime,"%H"))
times_hour <- as.numeric(format(datetime,"%H"))


#存储每个预测点核值的列表
hourDistance <- rep(list(), length(times_hour))
k_hourDistanceValue <- rep(list(), length(times_hour))

h_time <- 2

for (i in seq_along(times_hour)) {
  #计算当前预测点与所有历史点的小时差
  raw_hour_difference <- abs(st_filtered$hour-times_hour[i])
  #确保小时差在0到12之间
  hour_distance <- pmin(raw_hour_difference,24-raw_hour_difference)
  #保存小时差到列表
  hourDistance[[i]] <- hour_distance
  
  #计算核值
  k_hourDistance <- exp(-hour_distance^2/(2*h_time^2))
  k_hourDistanceValue[[i]] <- k_hourDistance
  
  #将每个预测点的核值保存到st_filtered
  st_filtered[[paste0("k_hourDistance_",times_hour[i])]] <- k_hourDistance
}

#plot 
choosen_hourIndex <- 6
plot(hourDistance[[choosen_hourIndex]],k_hourDistanceValue[[choosen_hourIndex]],type="p",
     xlab = "Hour distance",ylab="Kernel Value",
     main = paste("Kernal Value as a Function of Hour Distance\nPredicted Hour:",datetime[choosen_hourIndex]))

#sum 
  
for(i in seq_along(temp)){
  k_hour <- unlist(k_hourDistanceValue[i])  #k_hour value for each predicted time
  k_sum <- k_hour+st_filtered$k_Daydistance+st_filtered$k_distance  #每个历史点加和
  k_normalized <- k_sum/sum(k_sum)    #核值归一化
  temp[i] <- sum(k_normalized*st_filtered$air_temperature,na.rm=TRUE)
}

#multiply
temp_multiply <- vector(length=length(datetime))

for(i in seq_along(temp)){
  k_hour <- unlist(k_hourDistanceValue[i])
  k_multiply <- st_filtered$k_Daydistance*st_filtered$k_distance*k_hour
  k_normalized <- k_multiply/sum(k_multiply)
  temp_multiply[i] <- sum(k_normalized*st_filtered$air_temperature,na.rm=TRUE)
}

temp
temp_multiply

time_tables <- seq(from=4,by=2,length.out=length(temp))
plot(temp, type = "o",col="red" ,xaxt = "n",
     xlab = "Time ",ylab = "Temperature(Celsius)",main= "Temperature prediction\n(2013-11-04)",
     ylim = range(c(temp, temp_multiply))
)  # 禁用默认 x 轴
axis(1, at = 1:length(temp_multiply), labels = paste0(time_tables,":00"))  
lines(temp_multiply,type = "o",col="blue")
par(xpd = TRUE)
legend("bottom",col = c("red","blue"),legend=c("Sum of Kernel","Multiply of Kernel"),lty = 1, pch = 1)

