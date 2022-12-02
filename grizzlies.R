library(sf)
library(mapview)
library(dplyr)
library(sp)
library(adehabitatHR)
library(tidyverse)


test <- read.csv(file.choose())

head(test)
summary(test)

plot(test$LONGITUDE, test$LATITUDE)

test2 <- test %>% 
  dplyr::filter(LATITUDE > 0) %>% 
  dplyr::select(c(LONGITUDE, LATITUDE, Bear_ID))

plot(test2$LONGITUDE, test2$LATITUDE)


#just to see it in mapview
spf <- sf::st_as_sf(test2, 
                    coords = c("LONGITUDE","LATITUDE"),
                    crs = st_crs(4326))

mapview::mapview(spf)

# convert to sp object if needed
my.sp.point <- as(spf, "Spatial")

all.mcps <- mcp(my.sp.point)
all.mcps

#dataframe with the vaues of the mcp
mcps.df <- as.data.frame(all.mcps)

plot(all.mcps)

#simple plot with base R
plot(my.sp.point, col = as.factor(my.sp.point@data$Bear_ID), pch = 16)
plot(all.mcps, col = scales::alpha(1:5, 0.5), add = TRUE)


#with ggplot
ggplot() + 
  geom_sf(data= st_as_sf(all.mcps), aes(fill = id, alpha = 0.5)) +
  scale_fill_discrete(name = "Animal id")+
  geom_point(data = test2, aes(x=LONGITUDE , y=LATITUDE, colour = as.factor(Bear_ID)))+
  guides(alpha = "none", color = FALSE, size = FALSE)+
  theme_bw()



###

plot(sampletest)



sample.mcps <- NULL
for (i in unique(all.mcps$id)) {
  print(i)
  
  one.grizzly.mcp <- all.mcps[which(all.mcps$id == i),]
  one.grizzly.sample <- spsample(one.grizzly.mcp, n=2,"random")

  
  pointsSamp <- as.data.frame(coordinates(one.grizzly.sample))
  
  pointsSamp$id <- i

  plot(one.grizzly.mcp)
  plot(one.grizzly.sample, add=TRUE)
  title(i)
  sample.mcps <- rbind(sample.mcps, pointsSamp)
  
  
}
