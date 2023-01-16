library(move)

library(tidyverse)
orig <- dataframe1


dataframe1 <- read.csv(file.choose())
summary(dataframe1)
head(dataframe1)

dataframe1$datetime<-as.POSIXct(paste0(dataframe1$Year_,"-",dataframe1$Month_,"-",dataframe1$Day_," ", dataframe1$Hour_), format = "%Y-%m-%d %H", tz = "GMT")

dataframe1 <- dataframe1[order(dataframe1$Bear_ID, dataframe1$datetime),]

rownames(dataframe1) <- 1:nrow(dataframe1)

#getDuplicatedTimestamps(x=as.factor(dataframe1$Bear_ID), 
#                       timestamps=dataframe1$datetime)

#dataframe1CLEAN<-distinct(dataframe1,Bear_ID , datetime, .keep_all= TRUE)

#getDuplicatedTimestamps(x=as.factor(dataframe1CLEAN$Bear_ID), 
 #                       timestamps=dataframe1CLEAN$datetime)


dataframe1[209:210,]

movedata <- move::move(x=dataframe1$LONGITUDE, 
                       y = dataframe1$LATITUDE, 
                       time=dataframe1$datetime, 
                       proj = sf::st_crs(4326)$proj4string, 
                       animal = dataframe1$Bear_ID )

plot(movedata, xlab="Longitude", ylab="Latitude",type="l", pch=16, cex=0.5)
points(movedata, cex = 0.5)

movedata$distance <- unlist(lapply(distance(movedata), c, NA))
movedata$speed <- unlist(lapply(speed(movedata),c, NA ))
movedata$timeLag <- unlist(lapply(timeLag(movedata, units="hours"),  c, NA))

head(movedata)


myStackDF <- as.data.frame(movedata)
head(myStackDF)
myStackDF %>% 
  group_by(trackId) %>% 
  summarise(
    MeanSpeed = mean(speed, na.rm=TRUE),
    MeanLag = mean(timeLag, na.rm=TRUE),
    MeanDist = mean(distance, na.rm=TRUE),
    timeRangeb = range(time)[1],
    timeRangee = range(time)[2]
  )


library(gganimate)
myStackDF <- as.data.frame(movedata)
ggplot(data = myStackDF, aes(x = x, y = y, color = trackId)) + 
  geom_path(aes(x = x, y = y, color = trackId)) + 
  geom_point(aes(x = x, y = y, color = trackId),size = 1) + 
  theme_bw() + coord_cartesian()+
  transition_reveal(timestamps) +
  labs(title = 'Date: {frame_along}')

myStackDF %>% 
  group_by(trackId) %>% 
  summarise(
    timeRangeb = range(time)[1],
    timeRangee = range(time)[2]
  )
myStackDF <- myStackDF %>% 
  filter(trackId == c("CF10", "CF2"))

###################
dataframe1$DATE_ <- lubridate::date(dataframe1$datetime)
p <- ggplot()+ 
  geom_path(data = dataframe1, 
            aes(x = LONGITUDE , y = LATITUDE, color = Bear_ID), 
            alpha = 0.3)+
  geom_point(data = dataframe1, 
             aes(x = LONGITUDE, y = LATITUDE, color = Bear_ID),
             alpha = 0.7, shape=19, size = 2)+
  theme_bw()+
  theme(legend.position = "none")+
  transition_reveal(DATE_) +
  labs(title = 'Date: {frame_along}') 


p
###################
timeLags <- move::timeLag(movedata, units="hours")

distances <- move::distance(movedata)

speeds <- move::speed(movedata)

df1 <- as.data.frame(do.call(cbind, timeLags))

df2 <- pivot_longer(data = df1, cols = colnames(df1), names_to = "Bear_ID", values_to = "timelags1")

hist(df2$timelags1)

##for the other parameter, need to make the new df and pivot it as well
df3 <- as.data.frame(do.call(cbind, distances))

df4 <- pivot_longer(data = df3, cols = colnames(df3), names_to = "Bear_ID", values_to = "distances")

hist(df4$distances)


#for speed

df5 <- as.data.frame(do.call(cbind, speeds))

df6 <- pivot_longer(data = df5, cols = colnames(df5), names_to = "Bear_ID", values_to = "speeds")

hist(df6$speeds)


################# Loop for bursting
library(maptools)
listbursts <- list()
for (i in unique(movedata@trackId)) {
  oneanimal <- movedata[[i]]
  
  DayNight <- rep("Day", n.locs(oneanimal)-1)
  DayNight[solarpos(oneanimal[-n.locs(oneanimal)], timestamps(oneanimal)[-n.locs(oneanimal)])[,2] < -6 & solarpos(oneanimal[-1], timestamps(oneanimal)[-1])[,2] < -6] <- "Night"
  
  oneaniburst <- burst(x=oneanimal, f=DayNight)
  
  plot(oneaniburst, type="l", col=c("red", "black"), asp=1)
  title(i)
  
  
  listbursts <- append(listbursts,oneaniburst)
}


plot(table(diff(dataframe1$datetime)))
