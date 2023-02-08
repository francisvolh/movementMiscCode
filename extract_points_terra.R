#read the points
gps.points <- read.csv(file.choose())

#read the raster
r <- terra::rast(file.choose())

#plot on mapview DOES NOT SUPPORT TERRA OBJECTS
#mapview::mapview(my.sp.point)+ mapview::mapview(w)

# convert the sf to an sp object because adehabitatHR needs this for the MCP 

my.sp.point <- terra::vect(gps.points, geom = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")

w <- terra::project(my.sp.point, r)

#extract the points and add them to the gps.points database
a <- terra::extract(r, w)
gps.points$Elevation <- a[,2]
head(gps.points)

#issues with the scale functions or the extraction of points
gps.points$aspect.sc <- scale(gps.points$aspect, center =TRUE,  scale = TRUE)
gps.points$elevation.sc <- scale(gps.points$Elevation, center = TRUE)
gps.points$for_road.sc <- scale(gps.points$Forest_Road     , center = TRUE)
gps.points$slope.sc <- scale(gps.points$Slope, center = TRUE)
summary(gps.points)

scaleddf<-scale(gps.points[ ,c("Slope","Elevation","Aspect","Forest_Road","aspect")], center = FALSE, scale =  FALSE)
summary(scaleddf)
#clean df a bit to model 
gps.points<-gps.points %>% 
 mutate(
  Used = as.factor(Used)) 
head(gps.points)
summary(gps.points$Used)
class(gps.points$Used)

gps.points<-gps.points%>% 
   mutate(
        PA = case_when(Used == "TRUE" ~ 1,
                       Used == "FALSE" ~ 0
    )  
  )

head(gps.points)
summary(gps.points)


#try so gLMs

modfull <- lme4::glmer(data=gps.points, Used ~ aspect.sc + elevation.sc + for_road.sc+ (1|id), family = binomial)

options(na.action = "na.fail")
dredge(modfull)

summary(modfull)
mod2 <- lme4::glmer(data=gps.points, Used ~ aspect + (1|id), family = binomial)
mod1 <- lme4::glmer(data=gps.points, Used ~  elev + (1|id), family = binomial)
summary(mod1)

mod3 <- lme4::glmer(data=gps.points, Used ~  for_road+ (1|id), family = binomial)
summary(mod3)
