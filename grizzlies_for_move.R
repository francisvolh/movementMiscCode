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


#dataframe1[209:210,]

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

movedata$date <- as.Date(movedata$time)
  
head(movedata)


myStackDF <- as.data.frame(movedata)
head(myStackDF)

myStackDF %>% 
  group_by(trackId) %>% 
  summarise(
    MeanSpeed = mean(speed, na.rm=TRUE),
    minSpeed = min(speed, na.rm=TRUE),
    maxSpeed = max(speed, na.rm=TRUE),
    MeanLag = mean(timeLag, na.rm=TRUE),
    MeanDist = mean(distance, na.rm=TRUE),
    minDis= min(distance, na.rm=TRUE),
    maxDist = max(distance, na.rm=TRUE),
    TotDist = sum(distance, na.rm=TRUE),
    Tottime = sum(timeLag, na.rm=TRUE),
    Num.Days = length(unique(date)),
    Daily.Dis = TotDist/Num.Days
    #timeRangeb = range(time)[1],
    #timeRangee = range(time)[2]
  )

### look at speeds for individual bears
par(mfrow = c(3,2), 
    mar = c(2,2,2,2)) 

for (i in unique(myStackDF$trackId)){
  
  onebear <- myStackDF %>% 
    filter(trackId == i)
  hist(onebear$speed, main = i)
}


#make simple classification by speed threshold
#PLAY with this values a bit, either decide 3 or 4 classes
myStackDF <- myStackDF %>% 
    mutate(
        colorSpeed = case_when(
          speed >= 0 & speed < 0.01~ "red",
        speed > 0.01 & speed < 0.1  ~ "black",
        speed > 0.1 & speed < 0.3 ~ "blue",
        speed > 0.3 ~ "green"
          )
    
  )
#check out which are red
myStackDF %>% 
  filter(colorSpeed == "red")

#if using a basemap, crop it first, this Canada one is too big 
#canmap<-sf::st_read("gadm36_CAN_0.shp")

plot_listFULL <- list()

for (i in unique(myStackDF$trackId)) {
 onebear <- myStackDF %>% 
    filter(trackId == i)
  
p <- ggplot(data = onebear, aes(x=x , y=y)) +
  #geom_sf(data = canmap, aes())+ #map takes long to load
  #coord_sf(crs = 4326, xlim = range(onebear$x) , ylim = range(onebear$y)) + # do not do general limits
  geom_path(color = onebear$colorSpeed)+ # custom colours need to be listed as vectors, not as part of the df
  geom_point(color = onebear$colorSpeed)+# custom colours need to be listed as vectors, not as part of the df
  #guides(alpha = "none", color = "none", size = "none")+
  #ggtitle(i)+
  theme_bw()+
  theme(legend.position = "none")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle(i)
  print(p)
  plot_listFULL[[i]] <- p
  
}
cowplot::plot_grid(plotlist = plot_listFULL)

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

listbursts <- list() #create an empty list to throw in the individual bursts
allburstsdf<-NULL # create an empty dataframe to join all individuals burst now as dataframes
par(mfrow = c(3,2), 
    mar = c(2,2,2,2)) #set the plotting paramters for 4 plots in one panel, only works for base R plots
for (i in unique(movedata@trackId)) { #loop over the moveStack object using the trackIDs
  
  oneanimal <- movedata[[i]] #subset 1 animal from the moveStack
  
  #Steps to classify day night
  DayNight <- rep("Day", n.locs(oneanimal)-1) #create a label Day for that animal
  
  #overwrite whenever corresponds to Night
  DayNight[solarpos(oneanimal[-n.locs(oneanimal)], timestamps(oneanimal)[-n.locs(oneanimal)])[,2] < -6 & solarpos(oneanimal[-1], timestamps(oneanimal)[-1])[,2] < -6] <- "Night"
  
  #make an object with fragments/bursts corresponding to each break of day/night
  oneaniburst <- burst(x=oneanimal, f=DayNight)
  
  #make the bursted object into a Dataframe for future use
  onedf <- as.data.frame(oneaniburst)
  
  #create a column within the dataframe that contains the corresponding bear ID 
  onedf$Bear_ID <- i
  
  #plot the burst, Basic style 
  plot(oneaniburst, type="l", col=c("red", "black"), asp=1)
  legend("bottomleft",legend=c("day","night"), col=c("red", "black"), pch=19)
  title(i)

  #plot the burst with points with relative size of time spent on each fragment/burst 
  plotBursts(oneaniburst,breaks=5, col=c("red", "black"), pch=19, add=F,main="Size of points: total time spent in burst segment", asp=1)
  legend("bottomleft",legend=c("day","night"), col=c("red", "black"), pch=19)
  
  #now join all the DF versions of the individual burst with the next animal
  allburstsdf <- rbind(allburstsdf, onedf)
  
  #add the burst objects for each individual into a list, for future use, (maybe not needed)
  listbursts <- append(listbursts,oneaniburst)
}


#plot(table(diff(dataframe1$datetime)))

#make a summary for each animal and each category (day/night)

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

