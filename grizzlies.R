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
                    crs = sf::st_crs(4326))

spf<-st_transform(spf, crs = 3157)
#plot on mapview
mapview::mapview(spf)

# convert the sf to an sp object because adehabitatHR needs this for the MCP 
my.sp.point <- as(spf, "Spatial")

#run the MCP over all the  bear at the same time, the function understands the ids and will do for each bear
#note: it will only take 1 additional column of data besides lat and lon, and use as id for each individual
all.mcps <- mcp(my.sp.point, unout = "km2")

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
  geom_sf(data= sf::st_as_sf(all.mcps), aes(fill = id, alpha = 0.5)) +
  scale_fill_discrete(name = "Animal id")+
  coord_sf(crs = 4326)+
  geom_point(data = test2, aes(x=LONGITUDE , y=LATITUDE, colour = as.factor(Bear_ID)))+
  guides(alpha = "none", color = "none", size = "none")+
  theme_bw()

##Ploting individual MCP and points in a a grid
plot_list <- list()

for (i in unique(test2$Bear_ID)) {
  
  onemcp <- all.mcps[which(all.mcps$id == i),]
  onetrack <- test2[which(test2$Bear_ID == i),]
  
  p <-ggplot() + 
    geom_sf(data= sf::st_as_sf(onemcp), aes(fill = id, alpha = 0.5)) +
    scale_fill_discrete(name = "Animal id")+
    coord_sf(crs = 4326)+
    geom_point(data = onetrack, aes(x=LONGITUDE , y=LATITUDE, colour = as.factor(Bear_ID)))+
    guides(alpha = "none", color = "none", size = "none")+
    theme_bw()
  print(p)
  plot_list[[i]] <- p
  
}

cowplot::plot_grid(plotlist = plot_list)
###
aaa<-plot_list[1:3]
bbb<-plot_list[4:6]

cowplot::plot_grid(plotlist = bbb)


###to extract random points from each polygon, and plot mcp and kernels
plot_listFULL <- list()

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
 
  xy.randomSp <- spsample(one.grizzly.mcp, n=sampleSize*3,"random") #select random points from that 1 bear mcp
 
  xy.random <- as.data.frame(coordinates(xy.randomSp)) #convert the spatial object set of 2 points, into a dataframe
  
  xy.random$Used <- FALSE
  xy.random$id <- i #give a label to those 2 points with the ith bear id
  colnames(xy.random) <- c("LONGITUDE", "LATITUDE", "Used", "id")
  
  onespf <- sf::st_as_sf(one.grizzly.df, 
                      coords = c("LONGITUDE","LATITUDE"),
                      crs = sf::st_crs(4326))
  onespf<-st_transform(onespf, crs = 3157)
  
  
  # convert to sp object if needed
  one.sp.points <- as(onespf, "Spatial")
  
  onekernel<-kernelUD(one.sp.points)
  
  kernelTest50<-getverticeshr(onekernel, 50)
  kernelTest75<-getverticeshr(onekernel, 75)
  kernelTest95<-getverticeshr(onekernel, 95)
  
  #base plots
  #plot(xy.obs$LONGITUDE, xy.obs$LATITUDE, asp = 1, col = "darkblue", pch = 19, cex = 0.5)
  ####points(xy.random$LONGITUDE, xy.random$LATITUDE, pch = 19, col = "orange", cex = 0.5)
  #plot(one.grizzly.mcp, add = TRUE) #make a simple plot of the mcp for the ith bear
  
  #plot(kernelTest50, add = TRUE)
  #plot(kernelTest75, add = TRUE)
  #plot(kernelTest95, add = TRUE)
  #title(i)#give a title
  
  #trying to adjust the scales a bit
#  if (abs(kernelTest95@bbox[2,1] - kernelTest95@bbox[2,2]) > abs(kernelTest95@bbox[1,1] - kernelTest95@bbox[1,2])) {
#    latitudePlot <- range(kernelTest95@bbox[2,])
#    L <-mean(kernelTest95@bbox[1,])
#    longitudePlot <- c(L-abs(kernelTest95@bbox[2,1] - kernelTest95@bbox[2,2]), L+abs(kernelTest95@bbox[2,1] - kernelTest95@bbox[2,2]))
#  }else{
#    longitudePlot <- range(kernelTest95@bbox[1,])
    #L <-mean(kernelTest95@bbox[2,])
    #latitudePlot <- c(L-abs(kernelTest95@bbox[1,1] - kernelTest95@bbox[1,2]), L+abs(kernelTest95@bbox[1,1] - kernelTest95@bbox[1,2]))
#    latitudePlot <- range(kernelTest95@bbox[2,])
#    }

  #ggplots
  p <-ggplot() + 
    geom_sf(data= sf::st_as_sf(kernelTest50), aes( alpha = 0.5), fill = 'green'#, aes(fill = id, alpha = 0.5)
            )+
    geom_sf(data= sf::st_as_sf(kernelTest75), aes(alpha = 0.5), fill = 'green'#, aes(fill = id, alpha = 0.5)
    )+
    geom_sf(data= sf::st_as_sf(kernelTest95), aes(alpha = 0.5), fill = 'green'#, aes(fill = id, alpha = 0.5)
    )+
    geom_sf(data= sf::st_as_sf(one.grizzly.mcp), aes(alpha = 0.5), fill = "blue") +
    #scale_fill_discrete(name = "Animal id")+
    coord_sf(crs = 4326)+
    geom_point(data = xy.obs, aes(x=LONGITUDE , y=LATITUDE))+
    #coord_sf(crs = 4326, xlim = longitudePlot , ylim = latitudePlot) +
    guides(alpha = "none", color = "none", size = "none")+
    #ggtitle(i)+
    theme_bw()+
    theme(legend.position = "none")+
    xlab(NULL)+
    ylab(NULL)+

#trying to fix the scale to less decimals
   # scale_y_continuous(
    #  labels = scales::number_format(accuracy =0.1#,
                                     #decimal.mark = ','
     #                                ))+
    #
    #scale_x_continuous(
      #labels = scales::number_format(accuracy =0.1#,
    #                                 #decimal.mark = ','
     # ))+
    #scale_y_continuous(labels = percent_format(accuracy = 1)) +
    annotate("text",label=i, x=min(xy.obs$LONGITUDE), y=max(xy.obs$LATITUDE))
 print(p)
  
  sample.mcps <- rbind(sample.mcps, xy.obs, xy.random) #join the sample points dataframe of each bear into a main dataframe
  
  plot_listFULL[[i]] <- p
  
}

cowplot::plot_grid(plotlist = plot_listFULL)

###
aaa<-plot_listFULL[1:3]
bbb<-plot_listFULL[4:6]

cowplot::plot_grid(plotlist = bbb)

sample.mcps #revise the resulting dataframe

#plot mcps and kernels with base R


par(mfrow = c(3,2), #define number of row and columns for the plots
    mar = c(2,2,2,2)) #define margin sizes if needed
###to extract random points from each polygon, and plot mcp and kernels
plot_listFULL <- list()
df.areas <- NULL
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
  
  xy.randomSp <- spsample(one.grizzly.mcp, n=sampleSize*3,"random") #select random points from that 1 bear mcp
  
  xy.random <- as.data.frame(coordinates(xy.randomSp)) #convert the spatial object set of 2 points, into a dataframe
  
  xy.random$Used <- FALSE
  xy.random$id <- i #give a label to those 2 points with the ith bear id
  colnames(xy.random) <- c("LONGITUDE", "LATITUDE", "Used", "id")
  
  onespf <- sf::st_as_sf(one.grizzly.df, 
                         coords = c("LONGITUDE","LATITUDE"),
                         crs = sf::st_crs(4326))
  onespf<-st_transform(onespf, crs = 3157)
  
  
  # convert to sp object if needed
  one.sp.points <- as(onespf, "Spatial")
  
  onekernel<-kernelUD(one.sp.points)
  
  kernelTest50<-getverticeshr(onekernel, 50, unout = "km2")
  kernelTest75<-getverticeshr(onekernel, 75, unout = "km2")
  kernelTest95<-getverticeshr(onekernel, 95, unout = "km2")
  
  #base plots
  plot(xy.obs$LONGITUDE, xy.obs$LATITUDE, asp = 1, col = "darkblue", pch = 19, cex = 0.5)
  ####points(xy.random$LONGITUDE, xy.random$LATITUDE, pch = 19, col = "orange", cex = 0.5)
  plot(one.grizzly.mcp, add = TRUE) #make a simple plot of the mcp for the ith bear
  
  plot(kernelTest50, add = TRUE)
  plot(kernelTest75, add = TRUE)
  plot(kernelTest95, add = TRUE)
  title(i)#give a title
  
  one.df <- as.data.frame(one.grizzly.mcp)
  one.df$ud50area<-kernelTest50$area
  one.df$ud75area<-kernelTest75$area
  one.df$ud95area<-kernelTest95$area
  
  sample.mcps <- rbind(sample.mcps, xy.obs, xy.random) #join the sample points dataframe of each bear into a main dataframe
  
  df.areas <- rbind(df.areas, one.df)
  #plot_listFULL[[i]] <- p
  
}
df.areas
