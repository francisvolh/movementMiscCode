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
  guides(alpha = "none", color = FALSE, size = FALSE)+
  theme_bw()



###to extract random points from each polygon
#set number (not able to vary per bear yet)

sample.mcps <- NULL #empty object to store points later
for (i in unique(all.mcps$id)) { #loop over each bear id in the mcp object
  
  print(i) #shows only in the console which bears ae being worked
  
  one.grizzly.mcp <- all.mcps[which(all.mcps$id == i),] #subset the ith bear from the large mcp object
  one.grizzly.sample <- spsample(one.grizzly.mcp, n=2,"random") #select 2 random points from that 1 bear mcp
 
  pointsSamp <- as.data.frame(coordinates(one.grizzly.sample)) #convert the spatial object set of 2 points, into a dataframe
  
  pointsSamp$id <- i #give a label to those 2 points with the ith bear id

  plot(one.grizzly.mcp) #make a simple plot of the mcp for the ith bear
  plot(one.grizzly.sample, add=TRUE) #add the sampled points
  title(i)#give a title
  sample.mcps <- rbind(sample.mcps, pointsSamp) #join the sample points dataframe of each bear into a main dataframe
  
  
}

sample.mcps #revise the resulting dataframe
