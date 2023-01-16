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
                    crs = sf::st_crs(4326))

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

### MOnth Labels
bit_acusticaNew %>%
  mutate(
    
    season2 = case_when(
      Month %in% c(11, 12, 1, 2, 3)  ~ "Winter",
      Month %in% c(4, 5)      ~ "Spring",
      Month %in% c(6, 7)      ~ "Summer",
      Month %in% c(8,9,10)    ~ "Fall"
    )
  )
###
summary1 <-grizzly %>% 
  group_by(BearID_Season) %>% 
  tally()

list <- summary1 %>% 
  filter(n < 6)

list <-list$BearID_Season

data <- grizzly %>% 
  filter( !BearID_Season %in% list)

#plot individual season mcps

ggplot() + 
  geom_sf(data= st_as_sf(all.mcps), aes(fill = id, alpha = 0.5)) +
  scale_fill_discrete(name = "Animal id")+
  geom_point(data = grizzly, aes(x=LONGITUDE , y=LATITUDE, colour = as.factor(BearID_Season)))+
  guides(alpha = "none", color = "none", size = "none")+
  theme(legend.position = "none")
  theme_bw()

############## extra graph for counting fixes per habitat
#file with season and habitat quality
sample_bears <- read.csv(file.choose())

head (sample_bears)
names(sample_bears)
library(tidyverse)

sample_bears$HabUnit2 <- NULL

head(sample_bears)

sample_1 <- sample_bears %>%
  select("X","OID_","COLLAR","NAME","Bear_ID","BEAR","SEX","DATE_","TIME_",
         "Year_","Month_","Day_","Hour_","LATITUDE","LONGITUDE","ESpring1",
         "Summer1","LSpring1",
         "Fall1"#, "ESpring2", "Summer2","LSpring2","Fall2","HabUnit2","ESpring3","Summer3","LSpring3","Fall3","SEASON"
  ) %>% 
  pivot_longer(names_to = "Season", cols = c("ESpring1", "LSpring1","Summer1", "Fall1"), values_to = "Class")


summary1 <- sample_1 %>% 
  filter(Class !=9999) %>% 
  group_by(Bear_ID, Season, Class) %>% 
  summarise(
    fixes = n()  )

mean_sd<-summary1 %>% 
  group_by(Season, Class) %>%
  
  summarise(
    Fixes_avg = mean(fixes),
    Fixes_sd = sd(fixes)
  )

ggplot(data= summary1, aes(x = Class, y = fixes, color = Class ))+
  geom_point(position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("#0072B2", "#00BFC4"))+
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.3), size = 1)+
  #labs(title = "Cholesterol concentration in Peruvian Boobies")+
  guides(color = "none")+
  #labs(x = NULL)+
  #scale_x_discrete(labels = NULL)+
  ylab("Mean Number of Fixes")+
  theme_bw()+
  facet_grid(Season~.)

ggplot(data= mean_sd, aes(x = Season, y = Fixes_avg, group = as.factor(Class)))+
  geom_bar(aes(fill= as.factor(Class)),  stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Fixes_avg-Fixes_sd, ymax=Fixes_avg+Fixes_sd), width=.2,
                position=position_dodge(.9))+ 
  scale_fill_brewer(palette="Paired") + 
  guides(fill=guide_legend(title="New Legend Title"))+ 
  theme_minimal()

####################################
bear.mcps <- list()
for (i in unique(my.sp.point$Bear_ID)) {
  print(i)
  
  x <- my.sp.point[which(my.sp.point$Bear_ID == i),]
  grizzly.mcp <-mcp(x)
  plot(grizzly.mcp)
  plot(x, add=TRUE)
    title(i)
    bear.mcps <- append(bear.mcps, grizzly.mcp)
  
  
}



grizzly.mcp <-mcp(my.sp.point)

grizzly.mcp


#simple plot, I am sure there are more complex ways to plot this
plot(grizzly.mcp)

####################################

str(spf)

ggplot2::ggplot(data = test2, aes(x = LONGITUDE, y =LATITUDE, colour = "red"))+
  geom_point()



grizzly$DATETIME <- as.POSIXct(paste(grizzly$DATE, grizzly$TIME), format="%Y-%m-%d %H:%M:%S")


# check to make sure there are only xx Bear_Id
grizzlybearid <- unique(grizzly$Bear_ID)
grizzlycollar <-unique(grizzly$COLLAR)
grizzlynames <-unique(grizzly$NAME)

str(grizzly)

# Many analyses require Easting-Northing locations rather than longitude-latitude. We can obtain projected (Easting-Northing) 
# points using the packages sp and sf. Plotting the projected tracks is a good sanity check; make sure they look reassuringly similar to the longitude/latitude plot.

# Project to UTM
llcoord <- st_as_sf(grizzly[, c("LONGITUDE", "LATITUDE")], coords = c("LONGITUDE", "LATITUDE"), 
                    crs = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- st_transform(llcoord, crs = CRS("+proj=utm +zone=10 +datum=WGS84"))

# Add Easting-Northing to data (in km)
grizzly[, c("x", "y")] <- st_coordinates(utmcoord)/1000

# Plot Northing vs Easting
ggplot(grizzly, aes(x, y, col = "Bear_ID")) + 
  geom_point(size = 0.5) + geom_path() +
  coord_equal()

grizzly.sp <- grizzly [, c("Bear_ID", "x", "y")]
coordinates(grizzly.sp) <-c("x", "y")


spf2<-st_sfc(spf, crs = 4326)
as(x, "Spatial")

spf2 <- sf::st_as_sf(spf, 
                    coords = c("LONGITUDE","LATITUDE"),
                    crs = st_crs(4326), 
                    Class = "Spatial")



grizzly.mcp

##############

#kernels

kernelTest<-kernelUD(my.sp.point)
image(kernelTest)

kernelTest50<-getverticeshr(kernelTest, 50)
kernelTest75<-getverticeshr(kernelTest, 75)
kernelTest95<-getverticeshr(kernelTest, 95)
kernelTest50$id

plot(kernelTest50)
plot(kernelTest75, add = TRUE)
plot(kernelTest95, add = TRUE)
