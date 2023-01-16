library(moveVis)
library(move)
library(raster)

library(geosphere)

workdirect<-"C:/Users/Francis van Oordt/Downloads/shashi"
setwd(workdirect)

#after fixing issue with labels in Excel
a<-read.delim("Ananya_wet_table1.txt")
b<-read.csv("elep1.csv")

head(a)
colnames(a)<-c("date","time","Latitude","Longitude","Displaynam")
a$Date_Time <-paste(dummymoveVis$date, dummymoveVis$time)
a$Date_Time <-as.POSIXct(a$Date_Time, format="%m/%d/%Y %H:%M:%S")
a<-a[,c("Displaynam", "Longitude", "Latitude","Date_Time")]

head(b)
b<-b[,c("Displaynam", "Longitude", "Latitude","Date_Time")]

dummymoveVis<-rbind(a,b)



head(dummymoveVis)
summary(dummymoveVis)
class(dummymoveVis$Date_Time)

#convert timestamps into posixct
#dummymoveVis$Date_Time <-as.POSIXct(dummymoveVis$Date_Time, format="%Y-%m-%d %H:%M:%S")

#order data just in case, BEWARE IF THERE ARE MORE INDIVIDUALS
#movevis should take care of this anyway

dummymoveVis <- dummymoveVis[order(dummymoveVis$Date_Time),]

#nrow(dummymoveVis) #to rename rows for later when cleaning up bad points
#row.names(dummymoveVis)<-1:nrow(dummymoveVis)

#make the move object
dummymoveVis<-M
head(M)
m.dummymoveVis<-df2move(dummymoveVis,
                        proj = "+proj=longlat",
                        x = "Longitude", y = "Latitude", time = "Date_Time",  track_id = "id")

#check timestamps and sampling rates as they to SHOULD  be the same
unique(timestamps(m.dummymoveVis))
timeLag(m.dummymoveVis, unit = "mins")

#fixing the lag with interpolation for 4 min (240 secs)
m <- align_move(m.dummymoveVis, res = 60, unit = "mins")

#this uses my token for mapbox
#setting map style: penStreetMap 'watercolour' imagery with a transparency of 50%
frames <- frames_spatial(m, path_colours = as.factor(dummymoveVis$Displaynam),
                         map_service = "mapbox", map_type = "satellite",
                         map_token = "pk.eyJ1IjoiZnJhbmNpc3ZvbGgiLCJhIjoiY2t0dDVzMTBiMW41NjJycnI0bXpuMzV6cSJ9.0ikXBH2mCHPXXI9oTvlvkA", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()


#Have a look at the newly created frames list object and display a randomly selected frame 
#to get a first impression, how your animation will look like
length(frames) # number of frames
#frames[[300]] # display one of the frames
#frames[[150]]

#creating a gif of selected frames


animate_frames(frames, out_file = "moveVis_test_Jay1i.gif")


##To do a few extra calculations for speed and distance between points to find outliers
##check the data speed and distances
##
M<-dummymoveVis
#colnames(M)<-c("date","time","Latitude","Longitude","Displaynam","Date_time", "Date_Time")

M$timstp0 <- NA
M$tdif<-NA
M$dist <- NA
M$speed <- NA

for (i in unique(M$Displaynam)) {
  
  Rows <- which(M$Displaynam==i)
  
  M$timstp0[Rows] <- M$Date_Time[Rows] - M$Date_Time[Rows][1]
  M$tdif[Rows][-1] <- diff(M$Date_Time[Rows])
  M$dist[Rows][-1] <- distHaversine(M[Rows[-1],c("Longitude","Latitude")], M[Rows[-length(Rows)],c("Longitude","Latitude")])
}

M$speed<- M$dist/(M$tdif)

M$speed<- round(M$speed, digits=3)

hist(M$speed)
hist(M$tdif)
hist(M$dist)

#remove ALL crazy speed point
M<-M[which(M$speed<5),]


