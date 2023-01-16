library(sf)
library(mapview)
library(dplyr)
library(sp)
library(adehabitatHR)
library(tidyverse)
library(scales)


test <- read.csv(file.choose())
#see if data loaded well enough
head(test)
#confirm the previous if the numbers look okay 
summary(test)

#quick plot to see where things are off by plotting simple x and y locations 
plot(test$LONGITUDE, test$LATITUDE)

#clean up those strange locations in the southern hemisphere
test2 <- test %>% 
  dplyr::filter(LATITUDE > 0) %>%  #cleans everything that is less than 0 lat, so everything south of the equator
  dplyr::select(c(LONGITUDE, LATITUDE, Bear_ID)) #selects only 3 columns that we need

plot(test2$LONGITUDE, test2$LATITUDE) #replots with the clean data now


#Create an sf object to 
#just to see it in mapview, nicer interactive map
spf <- sf::st_as_sf(test2, 
                    coords = c("LONGITUDE","LATITUDE"),
                    crs = st_crs(4326))
#plot on mapview
mapview::mapview(spf)

# convert the sf to an sp object because adehabitatHR needs this for the MCP 
my.sp.point <- as(spf, "Spatial")

#run the MCP over all the  bear at the same time, the function understands the ids and will do for each bear
#note: it will only take 1 additional column of data besides lat and lon, and use as id for each individual
all.mcps <- mcp(my.sp.point)

#view the MCP object with all the polygons
all.mcps

#convert the MCP spatial object into a simple dataframe with the vaues of the mcp for other use (means, sd, etc?)
mcps.df <- as.data.frame(all.mcps)

#plots the MCPs quickly
plot(all.mcps)

#Colour each MCP by bear ID, simple plot with base R
plot(my.sp.point, col = as.factor(my.sp.point@data$Bear_ID), pch = 16)
plot(all.mcps, col = scales::alpha(1:5, 0.5), add = TRUE)


#now plots the MCPs with ggplot (instead of using base R, which doesn't easily give you axis for some reason)
ggplot() + 
  geom_sf(data= st_as_sf(all.mcps), aes(fill = id, alpha = 0.5)) +
  scale_fill_discrete(name = "Animal id")+
  geom_point(data = test2, aes(x=LONGITUDE , y=LATITUDE, colour = as.factor(Bear_ID)))+
  guides(alpha = "none", color = "none", size = "none")+
  theme_bw()




###to extract random points from each polygon
sample.mcps <- NULL #empty object to store points later
for (i in unique(all.mcps$id)) { #loop over each bear id in the mcp object
  
  print(i) #shows only in the console which bears ae being worked
  
  #sample the dataframe of 1 bear
  one.grizzly.df <- test2[which(test2$Bear_ID == i),] 
  
  sampleSize<-nrow(one.grizzly.df)
  
  
  xy.obs <- one.grizzly.df[sample(1:nrow(one.grizzly.df), sampleSize*3, replace = TRUE), c("LONGITUDE", "LATITUDE")]
  
  xy.obs$Used <- TRUE
  xy.obs$id <- i #give a label to those 2 points with the ith bear id
  one.grizzly.mcp <- all.mcps[which(all.mcps$id == i),] #subset the ith bear from the large mcp object
 
  xy.randomSp <- spsample(one.grizzly.mcp, n=sampleSize*3,"random") #select 2 random points from that 1 bear mcp
 
  xy.random <- as.data.frame(coordinates(xy.randomSp)) #convert the spatial object set of 2 points, into a dataframe
  
  xy.random$Used <- FALSE
  xy.random$id <- i #give a label to those 2 points with the ith bear id
  colnames(xy.random) <- c("LONGITUDE", "LATITUDE", "Used", "id")
  
  
  plot(xy.random$LONGITUDE, xy.random$LATITUDE, asp = 1, col = "darkblue", pch = 19, cex = 0.5)
  points(xy.obs$LONGITUDE, xy.obs$LATITUDE, pch = 19, col = "orange", cex = 0.5)
  
  plot(one.grizzly.mcp, add = TRUE) #make a simple plot of the mcp for the ith bear
  
  title(i)#give a title
  sample.mcps <- rbind(sample.mcps, xy.obs, xy.random) #join the sample points dataframe of each bear into a main dataframe
  
  
}

sample.mcps #revise the resulting dataframe
