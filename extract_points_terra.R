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
gps.points$aspect <- a[,2]
head(gps.points)


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


#try so gLMs

modfull <- lme4::glmer(data=gps.points, Used ~ aspect + elev + for_road+ (1|id), family = binomial)
mod2 <- lme4::glmer(data=gps.points, Used ~ aspect + (1|id), family = binomial)
mod1 <- lme4::glmer(data=gps.points, Used ~  elev + (1|id), family = binomial)
summary(mod1)

mod3 <- lme4::glmer(data=gps.points, Used ~  for_road+ (1|id), family = binomial)
summary(mod3)
