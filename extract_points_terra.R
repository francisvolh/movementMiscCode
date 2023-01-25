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
gps.points$elev <- terra::extract(r, w)

head(gps.points)
