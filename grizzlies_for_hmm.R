
# Plotting package
library(tidyverse)
theme_set(theme_bw())
# Movement modelling packages
library(momentuHMM)
#library(foieGras)
library(adehabitatLT)
# GIS packages
library(sf)
library(sp)
# Load functions for later
#source("utility_functions.R")

# Load data from Movebank URL
#URL <- paste0("https://www.datarepository.movebank.org/bitstream/handle/10255/",
#             "move.981/ThermochronTracking%20Elephants%20Kruger%202007.csv")

raw <- read.csv(file.choose())
head(raw)
raw$datetime <- paste0(raw$Year_, "-", raw$Month_, "-", raw$Day_," ", raw$Hour_)
ncol(raw)
# Keep relevant columns: ID, time, longitude, latitude, temperature
data_all <- raw[, c(5, 20, 15, 14, 16, 17, 18, 19)]
colnames(data_all) <- c("ID", "time", "lon", "lat", " Slope", "Elevation",   "Aspect", "Forest_Road")
data_all$time <- as.POSIXct(data_all$time, format = "%Y-%m-%d %H", tz = "GMT")
head(data_all)
# Just keep 2000 observations to save time with model fitting
#data <- data_all[c(which(data_all$ID == unique(data_all$ID)[5])[1:1000],
#                  which(data_all$ID == unique(data_all$ID)[6])[1:1000]),]
data<-data_all

data <- data[order(data$ID, data$time),]
data<-distinct(data, ID, time, .keep_all= TRUE)

nrow(data)

head(data)
data_all$Year <- format(data_all$time, format="%Y")
# Plot latitude vs longitude (Mercator projection)
ggplot(data, aes(lon, lat, col = ID)) +
  geom_point(size = 0.5) + geom_path() +
  coord_map("mercator")

# Longitude vs time
ggplot(data, aes(time, lon, col = ID)) +
  geom_point(size = 0.5) + 
  geom_path()+
  facet_wrap(~as.factor(ID), scales = "free_x")+
  theme(strip.placement = "outside") 

# Latitude vs time
ggplot(data, aes(time, lat, col = ID)) +
  geom_point(size = 0.5) + geom_path()

# Project to UTM
llcoord <- st_as_sf(data[, c("lon", "lat")], coords = c("lon", "lat"), 
                    crs = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- st_transform(llcoord, crs = CRS("+proj=utm +zone=10 +datum=WGS84"))

# Add Easting-Northing to data (in km)
data[, c("x", "y")] <- st_coordinates(utmcoord)/1000
names(data)
# Plot Northing vs Easting
ggplot(data, aes(x, y, col = ID)) + 
  geom_point(size = 0.5) + geom_path() +
  coord_equal()

# Table of time intervals in data
#move attempt
library(move)
data_move <- move::move(x=data$lon, 
                        y = data$lat, 
                        time=data$time, 
                        proj = sf::st_crs(4326)$proj4string, 
                        animal = data$ID)
?move::timeLag
data_move$timeLag <- unlist(lapply(timeLag(data_move, units="hours"),  c, NA))
?move::distance
data_move$distance <- unlist(lapply(distance(data_move), c, NA))
?move::speed
data_move$speed <- unlist(lapply(speed(data_move),c, NA ))

head(data_move)

plot(data_move$timeLag, data_move$distance)
plot(data_move$speed, data_move$distance)
plot(data_move$speed, data_move$timeLag)
summary(data_move$timeLag)

data %>% 
  group_by(ID) %>% 
  summarise(
    start = min(time),
    end = max(time)
  )

data_move_df <- as.data.frame(data_move)
names(data_move_df)
head(data_move_df)


B<-ggplot(data_move_df, aes(time, timeLag, col = trackId))+
  geom_line()+
  facet_grid(~as.factor(trackId), scales = "free_x", switch = 'x')+
  theme(strip.placement = "outside") 

A<-ggplot(data_move_df, aes(time, distance, col = trackId))+
  geom_line()+
  facet_grid(~as.factor(trackId), scales = "free_x", switch = 'x')+
  theme(strip.placement = "outside") 
C<-ggplot(data_move_df, aes(time, speed, col = trackId))+
  geom_line()+
  facet_grid(~as.factor(trackId), scales = "free_x", switch = 'x')+
  theme(strip.placement = "outside")

cowplot::plot_grid(A, B, C, ncol = 1, nrow = 3)


max(data_move_df$timeLag, na.rm=TRUE)

max(data_move_df$distance, na.rm=TRUE)
rownames(data_move_df)<-1:nrow(data_move_df)
data_move_df[which(data_move_df$timeLag>1000),]
data_move_df[490:499,]

plot(table(diff(data$time)), #xlim = c(0, 300), 
     xlab = "time interval (min)", ylab = "count")


plot(table(diff_by_ID(data_all$time, data_all$ID)), #xlim = c(0, 300), 
     xlab = "time interval (min)", ylab = "count")

data_inspect <-prepData(data, type = "UTM")




# Use function from utility_function.R to split data at gaps > 2 hours
data_split <- split_at_gap(data = data, max_gap = 2*60, shortest_track = 24*60)

ggplot(data_split, aes(x, y, col = ID)) + 
  geom_point(size = 0.5) + geom_path() +
  coord_equal()

#calculate number of points per NEW track
class(data_split)
names(data_split)
data_split %>% 
  group_by(ID) %>% 
  summarise(
    n = n()
  )

# Create adehabitat trajectory padded with NAs
data_ade <- setNA(ltraj = as.ltraj(xy = data_split[, c("x", "y")], 
                                   date = data_split$time, 
                                   id = data_split$ID), 
                  date.ref = data_split$time[1], 
                  dt = 60, tol = 5, units = "min")

# Transform back to dataframe
data_na <- ld(data_ade)[, c("id", "x", "y", "date")]
colnames(data_na) <- c("ID", "x", "y", "time")

# Add temperatures for non-missing locations
data_na$Elevation <- NA
data_na$Elevation[which(!is.na(data_na$x))] <- data_split$Elevation

data_na$Aspect <- NA
data_na$Aspect[which(!is.na(data_na$x))] <- data_split$Aspect

data_na$Forest_Road <- NA
data_na$Forest_Road[which(!is.na(data_na$x))] <- data_split$Forest_Road


head(data_na, 10)

# Prepare data for HMM (compute step lengths and turning angles)
data_hmm1 <-prepData(data_na, type = "UTM", covNames = c("Elevation", "Aspect", "Forest_Road"))

plot(data_hmm1)

head(data_hmm1, 10)


###############################################

names(data_hmm1)

ggplot(data_hmm1, aes(time, step, col = ID))+
  geom_line()+
  facet_grid(~as.factor(ID), scales = "free_x")+
  theme(strip.placement = "outside") 

summary1<-data_hmm1 %>% 
  group_by(ID) %>% 
  summarise(
    n = n(),
    MeanDist = mean(step, na.rm=TRUE),
    SDDist = sd(step, na.rm=TRUE),
    meanConc = mean(angle, na.rm=TRUE),
    timeRangeb = range(time)[1],
    timeRangee = range(time)[2]
  )
hist(summary1$n)
hist(summary1$MeanDist)
hist(summary1$meanConc)

min(summary1$n)

min(summary1$SDDist)
max(summary1$SDDist)
min(summary1$MeanDist)
max(summary1$MeanDist)

min(summary1$meanConc)

max(summary1$meanConc)

###############################################

# Observation distributions (step lengths and turning angles)
dist <- list(step = "gamma", angle = "vm")

# Initial parameters
# (step mean 1, step mean 2, step SD 1, step SD 2) and (angle concentration 1, angle concentration 2)
#adding a zero mass for step, as needed by the model
Par0_2s <- list(step = c(mean=c(0.05, 0.05), sd =c(0.2,0.2), c(0.001,0.5)), angle = c(0.1, 3))

# Fit a 2-state HMM
hmm1 <- fitHMM(data_hmm1, nbStates = 2, dist = dist, Par0 = Par0_2s)

# Print parameter estimates
hmm1

# Plot estimated distributions and state-coloured tracks
plot(hmm1, breaks = 25, ask = FALSE)



# Initial parameters for 3-state model
Par0_3s <- list(step = c(0.02, 0.1, 0.3, 0.02, 0.1, 0.3), 
                angle = c(0.01, 0.1, 3))

# Fit 3-state HMM
hmm2 <- fitHMM(data_hmm1, nbStates = 3, dist = dist, Par0 = Par0_3s)

hmm2

plot(hmm2, breaks = 25, ask = FALSE)

# Get most likely sequence of states (Viterbi algorithm)
head(viterbi(hmm2))

# Save most likely state sequences from 2-state and 3-state models
data_hmm1$state_2st <- factor(viterbi(hmm1))
data_hmm1$state_3st <- factor(viterbi(hmm2))

# Plot tracks, coloured by states
ggplot(data_hmm1, aes(x, y, col = state_2st, group = ID)) +
  geom_point(size = 0.5) + geom_path() +
  coord_equal()
ggplot(data_hmm1, aes(x, y, col = state_3st, group = ID)) +
  geom_point(size = 0.5) + geom_path() +
  coord_equal()

# Fit 2-state HMM with temperature covariate (linear or quadratic effect)
hmm3 <- fitHMM(data_hmm1, nbStates = 2, dist = dist, 
               Par0 = Par0_2s, formula = ~temp)
hmm4 <- fitHMM(data_hmm1, nbStates = 2, dist = dist, 
               Par0 = Par0_2s, formula = ~temp+I(temp^2))

# Compare models using AIC
AIC(hmm3, hmm4)

# Plot estimated distributions and transition probabilities as functions of temperature
plot(hmm3, plotTracks = FALSE, ask = FALSE, plotCI = TRUE)

# Plot stationary state probabilities as functions of temperature
plotStationary(hmm3, plotCI = TRUE)

# Plot pseudo-residuals for 2-state and 3-state models
plotPR(hmm1)
plotPR(hmm2)

# Change data to format expected by foieGras
data_foieGras <- data_split[,c("ID", "time", "lon", "lat")]
colnames(data_foieGras)[1:2] <- c("id", "date")
# Add column for location quality class (G for "GPS")
data_foieGras$lc <- "G"
# Change order of columns as expected by foieGras
data_foieGras <- data_foieGras[,c(1, 2, 5, 3, 4)] 

# Fit state-space model to predict regular locations on 0.5h grid
ssm <- fit_ssm(d = data_foieGras, time.step = 0.5)

# Data frame of regularised locations
pred <- grab(ssm, what = "predicted", as_sf = FALSE)
data_reg <- as.data.frame(pred[, 1:4])
colnames(data_reg)[1:2] <- c("ID", "time")

plot(ssm, ask = FALSE)

# Get step lengths and turning angles from regularised data
data_hmm2 <- prepData(data = data_reg, type = "LL", coordNames = c("lon", "lat"))

head(data_hmm2, 10)

# Fit 2-state HMM to regularised data
hmm4 <- fitHMM(data_hmm2, nbStates = 2, dist = dist, Par0 = Par0_2s)

plot(hmm4, ask = FALSE)

# Predict locations on 30-min grid using crawl (through momentuHMM wrapper)
crw_out <- crawlWrap(obsData = data_split, timeStep = "30 min", 
                     Time.name = "time", coord = c("x", "y"))
data_hmm3 <- prepData(data = crw_out)

# Fit 2-state HMM to regularised data
hmm5 <- fitHMM(data_hmm3, nbStates = 2, dist = dist, Par0 = Par0_2s)

# Fit HMM using multiple imputation
hmm6 <- MIfitHMM(miData = crw_out, nSims = 10, ncores = 3, nbStates = 2, 
                 dist = dist, Par0 = Par0_2s)