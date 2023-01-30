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

dataframe1<-distinct(dataframe1,Bear_ID , datetime, .keep_all= TRUE)

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


#make animations, need to be sure individual move in the same timeframe

#should filter for the time frames that have the most 
#individuals or tracks in consecutive time
library(gganimate)
myStackDF <- as.data.frame(movedata)
ggplot(data = myStackDF, aes(x = x, y = y, color = trackId)) + 
  geom_path(aes(x = x, y = y, color = trackId)) + 
  geom_point(aes(x = x, y = y, color = trackId),size = 1) + 
  theme_bw() + coord_cartesian()+
  transition_reveal(timestamps) +
  labs(title = 'Date: {frame_along}')


################# Loop for bursting
library(maptools)
listbursts <- list()

allburstsdf<-NULL

par(mfrow = c(3,2), 
    mar = c(2,2,2,2))


for (i in unique(movedata@trackId)) {
  oneanimal <- movedata[[i]]
  
  DayNight <- rep("Day", n.locs(oneanimal)-1)
  DayNight[solarpos(oneanimal[-n.locs(oneanimal)], timestamps(oneanimal)[-n.locs(oneanimal)])[,2] < -6 & solarpos(oneanimal[-1], timestamps(oneanimal)[-1])[,2] < -6] <- "Night"
  
  oneaniburst <- burst(x=oneanimal, f=DayNight)
  
  onedf <- as.data.frame(oneaniburst)
  onedf$Bear_ID <- i
  plot(oneaniburst, type="l", col=c("red", "black"), asp=1)
  legend("bottomleft",legend=c("day","night"), col=c("red", "black"), pch=19)
  title(i)

  plotBursts(oneaniburst,breaks=5, col=c("red", "black"), pch=19, add=F,main="Size of points: total time spent in burst segment", asp=1)
  legend("bottomleft",legend=c("day","night"), col=c("red", "black"), pch=19)
  
  allburstsdf <- rbind(allburstsdf, onedf)
  listbursts <- append(listbursts,oneaniburst)
}


plot(table(diff(dataframe1$datetime)))

allburstsdf %>% 
  filter(!is.na(burstId)) %>% 
  group_by(Bear_ID, burstId) %>% 
  summarise(
    TotDist = sum(distance),
    Tottime = sum(timeLag),
    MeanSpeed = mean(speed, na.rm=TRUE),
    MeanLag = mean(timeLag, na.rm=TRUE),
    MeanDist = mean(distance, na.rm=TRUE)
   )

