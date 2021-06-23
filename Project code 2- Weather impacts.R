#non-linear weather impacts on crop yields#
#impact on a single CSA farm

# load packages 
library(reshape2)
library(dplyr)
library(plyr)
library(tmaptools)
library(maps)
library(raster)
library(esmisc)
library(mapdata)
library(mapview)
library(sp)
library(splines)
library(splines2)
library(margins)
library(ggeffects)
library(margins)

setwd("C:/Users/S.VYAS/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather impacts")

# read data on yields and precipitation per farm
daten<-read.csv("daten.csv")


# summary statistics

summary(daten$PREC) # some summary statistics 
sd(daten$PREC, na.rm=T)      # standard deviation 

summary(daten$YIELD) # some summary statistics 
sd(daten$YIELD, na.rm=T)      # standard deviation 

hist(daten$PREC) # Histogram
hist(daten$YIELD) # Histogram

# for more summary graphics see https://bookdown.org/rdpeng/exdata/the-base-plotting-system-1.html


# demeaning or centering panel data- control time invariant farm charecterstics
# create new vars (newYIELD; newPREC)

demeaned<-transform(daten, 
                    newYIELD = YIELD-ave(YIELD, i),
                    newPREC  = PREC-ave(PREC, i)
)


# example plot of farm X3 ... i == X3 (farm 3 is super-champion, highest adaptation level)
# subset for i == "X3"

plot_sub<-subset(demeaned, i=="X3") 

par(mfrow=c(1,2)) # cuts the plot area in pieces. return to normal type in par(mfrow=c(1,1))

plot(plot_sub$t+2000,plot_sub$PREC, type="l", 
     main="Precipitation sum per year", 
     ylab="Precipitation sum [mm]",
     xlab="t = year")

plot(plot_sub$t+2000,plot_sub$newPREC, type="l", 
     main="Precipitation demeaned", 
     ylab="Deviation from long-term \n average Precipitation [mm]",
     xlab="t = year")
abline(h=0) # ref line

# estimate impact of different models on non-linearity
model1<-lm(YIELD~PREC, data=demeaned) # pooled model linear impact
summary(model1)

model2<-lm(newYIELD~newPREC, data=demeaned) # demeaned model linear impact
summary (model2)

model3<-lm(newYIELD~poly(newPREC,2), data=demeaned) # demeaned model quadratic impact
summary(model3)

cplot(model3,"newPREC", main ="Farm fixed effects (Demeaned) \n and quadratic impact")
abline(h=0,lty=2)
abline(v=0,lty=2)

#non parametric spline regression
model4<-lm(newYIELD~poly(newPREC,3), data=demeaned) # demeaned model cubic impact
model5<-lm(newYIELD~ns(newPREC,3), data=demeaned) # demeaned model  regression splines 3 knots
model6<-lm(newYIELD~ns(newPREC,4), data=demeaned) # demeaned model  regression splines 4 knots
model7<-lm(newYIELD~ns(newPREC,5), data=demeaned) # demeaned model  regression splines 5 knots

# plot knot locations
hist(demeaned$newPREC, breaks=100)
abline(v=c(attr(ns(demeaned$newPREC,3), "Boundary.knots")[1],attr(ns(demeaned$newPREC,3), "knots"),attr(ns(demeaned$newPREC,3), "Boundary.knots")[2]),lty=2, lwd=2, col="red")

hist(demeaned$newPREC, breaks=100)
abline(v=c(attr(ns(demeaned$newPREC,4), "Boundary.knots")[1],attr(ns(demeaned$newPREC,4), "knots"),attr(ns(demeaned$newPREC,4), "Boundary.knots")[2]),lty=2, lwd=2, col="red")

hist(demeaned$newPREC, breaks=100)
abline(v=c(attr(ns(demeaned$newPREC,5), "Boundary.knots")[1],attr(ns(demeaned$newPREC,5), "knots"),attr(ns(demeaned$newPREC,5), "Boundary.knots")[2]),lty=2, lwd=2, col="red")


# non-linear marginal effects
par(mfrow=c(2,4)) # cut to four cols/rows for comparison
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste("The impact of rainfall\n",
                             "during flowering\n",
                             "on winter wheat yield"), 
     cex = 1.6, col = "black")
cplot(model1,"PREC", main ="Pooled model")
cplot(model2,"newPREC", main ="Farm fixed effects (Demeaned)")
abline(h=0,lty=2)
cplot(model3,"newPREC", main ="Farm fixed effects (Demeaned) \n and quadratic impact")
abline(h=0,lty=2)
cplot(model4,"newPREC", main ="Farm fixed effects (Demeaned) \n and cubic impact")
abline(h=0,lty=2)
cplot(model5,"newPREC", main ="Farm fixed effects (Demeaned) \n and spline with 4 knots", xlim = c(-85,105))
abline(h=0,lty=2)
abline(v=c(attr(ns(demeaned$newPREC,3), "Boundary.knots")[1],attr(ns(demeaned$newPREC,3), "knots"),attr(ns(demeaned$newPREC,3), "Boundary.knots")[2]),lty=2, lwd=2)

cplot(model6,"newPREC", main ="Farm fixed effects (Demeaned) \n and spline with 5 knots", xlim = c(-85,105))
abline(h=0,lty=2)
abline(v=c(attr(ns(demeaned$newPREC,4), "Boundary.knots")[1],attr(ns(demeaned$newPREC,4), "knots"),attr(ns(demeaned$newPREC,4), "Boundary.knots")[2]),lty=2, lwd=2)

cplot(model7,"newPREC", main ="Farm fixed effects (Demeaned) \n and spline with 6 knots", xlim = c(-85,105))
abline(h=0,lty=2)
abline(v=c(attr(ns(demeaned$newPREC,5), "Boundary.knots")[1],attr(ns(demeaned$newPREC,5), "knots"),attr(ns(demeaned$newPREC,5), "Boundary.knots")[2]),lty=2, lwd=2)

#compare with non-csa farms


#-------------------------------------------------------------------------------

#note-following to be done
#apply same concept to geo-coded csv farms and weather data-identify weather thresholds
#modify following code from Dr. Ariel to own dataset
#no interpolation need, farm level point data
#match farms to nearest grid and extract data
#variables to be used- weather (IMD/CHIRPS), soil moisture?
#integrate CMIP6 to identify shifts in weather in future climate

#===============================================================================
# Description: Download PRISM from FTP servers
#===============================================================================

# This script file downloads data from the PRISM servers:
#   - monthly : tmin, tmax, tmean, ppt, over 1981-2020
#   - daily   : tmin, tmax, over 1981-2020, and ppt for August 2020
# These files take considerable amount of space on the hard drive, especially
# the daily temperature files (~120GB)

#===============================================================================
# 1). Preliminary ------
#===============================================================================

# Clean up workspace
rm(list=ls())
gc()

# Clean and load packages
wants <- c("rgdal","sp","raster","RColorBrewer","ncdf4","prism")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# Directories
dir <- list()
dir$root <- dirname(getwd())
dir$prism <- paste0(dir$root,"/data/PRISM")
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))


#===============================================================================
# 2). Import PRISM data from FTP server  ------
#===============================================================================


# Monthly data
# Variables: ppt, tmin, tmax, tmean
# ~80min and 8GB
system.time({
  lapply(c("tmin","tmax","tmean","ppt"), function(var) {
    
    # Set directory to write monthly files
    prism_set_dl_dir(paste0(dir$prism,"/monthly/",var))
    
    # Download files
    get_prism_monthlys(type = var, years = 2015:2020, mon = 1:12, keepZip=FALSE)
    
  })
})

# Daily data
# Variables: tmin, tmax
# ~20hours, 112GB (yes, that's a lot)
system.time({
  lapply(c("tmin","tmax"), function(var){
    
    print(var)
    
    # Set directory to write daily files
    prism_set_dl_dir(paste0(dir$prism,"/daily/",var))
    
    # Download files
    get_prism_dailys(type = var, minDate = "1981-01-01", maxDate = "2020-12-31", keepZip=FALSE)
  })
})

# Daily data
# Variables: ppt (only for august 2020)
# 70 s, 150 MB
system.time({
  lapply(c("ppt"), function(var){
    
    print(var)
    
    # Set directory to write daily files
    prism_set_dl_dir(paste0(dir$prism,"/daily/",var))
    
    # Download files
    get_prism_dailys(type = var, minDate = "2020-08-01", maxDate = "2020-08-31", keepZip=FALSE)
  })
})

#===============================================================================
# Description: This script files performs a series of tasks related to weather
# data. Here is a description of each section of the code:
# 1- Runs preliminary loading packages, directories and functions
# 2- Import gridded and station weather, shapefiles, land cover
# 3- Aggregate land cover to the PRISM grid
# 4- Create transformation "P" matrix for gridded data aggregation
# 5- Interpolate weather station data to counties
# 6- Interpolate weather station data to PRISM grid
# 7- Compute temperature exposure bins
# 8- Aggregate all gridded data to the county level
# 9- Create figures 1-7 in the chapter
#===============================================================================


#===============================================================================
# 1). Preliminary ------
#===============================================================================

# Clean up workspace
rm(list=ls())
gc()

# Clean and load packages
wants <- c("rgdal","sp","raster","RColorBrewer","ncdf4","prism","Matrix","zoo",
           "data.table","rworldmap","rgeos","spam","SpaDES","dplyr","sf","fasterize",
           "dplyr","plyr","maptools","readr","FNN")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# Directories
dir <- list()
dir$root <- dirname(getwd())
dir$map <- paste0(dir$root,"/data/gz_2010_us_050_00_20m")
dir$smap <- paste0(dir$root,"/data/cb_2018_us_state_20m")
dir$prism <- paste0(dir$root,"/data/PRISM")
dir$prism2 <- paste0(dir$root,"/data2/PRISM")
dir$stations <- paste0(dir$root,"/data/GHCN")
dir$data <- paste0(dir$root,"/data2/PRISM_co")
dir$bins <- paste0(dir$root,"/data2/PRISM/bins")
dir$landcover <- paste0(dir$root,"/data/NLCD_2016_Land_Cover_L48_20190424")
dir$figures <- paste0(dir$root,"/figures")
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

# Functions
source("functions.R")

#===============================================================================
# 2). Import data ------
#===============================================================================

# 1. PRISM land and ID masks ------

# Create land mask for PRISM raster
s <- paste0(dir$prism,"/daily/tmax/PRISM_tmax_stable_4kmD2_19810101_bil/PRISM_tmax_stable_4kmD2_19810101_bil.bil")
s <- raster(s)
mask <- s
mask[] <- 0
mask[!is.na(s[])] <- 1
plot(mask)

# Create a "grid cell ID" raster only for cells over land (NA otherwise)
id <- mask
id[] <- 1:length(id)
id[mask[]==0] <- NA
plot(id)
names(id) <- names(mask) <- "cellid"


# 2. Polygons of US counties -----

map <- readOGR(paste0(dir$map,"/gz_2010_us_050_00_20m.shp"),"gz_2010_us_050_00_20m")
map <- map[!(map@data$STATE %in% c("15","02","72")), ] # drop Hawaii, Alaska and Puerto Rico
map <- spTransform(map, CRS(projection(s))) # make sure map has same projection as raster
map@data$fips <- as.numeric(map@data$STATE)*10^3 + as.numeric(map@data$COUNTY) # county FIPS code
world <- getMap(resolution = "high") # map of the world
world <- spTransform(world, CRS(projection(map)))
us <- unionSpatialPolygons(map, IDs=rep(1,length(map)))

# 3. GHCND weather station data for 2020 -----

# Stations and their coordinates

# From readme.txt
#IV. FORMAT OF "ghcnd-stations.txt"
#- - - - - - - - - - - - - - - -
#  Variable   Columns   Type
#- - - - - - - - - - - - - - - - -
#ID            1-11   Character
#LATITUDE     13-20   Real
#LONGITUDE    22-30   Real
#ELEVATION    32-37   Real
#STATE        39-40   Character
#NAME         42-71   Character
#GSN FLAG     73-75   Character
#HCN/CRN FLAG 77-79   Character
#WMO ID       81-85   Character
pos <- fwf_positions(start=c(1,13,22,32,39,42,73,77,81),
                     end=c(11,20,30,37,40,71,75,79,85),
                     col_names = c("id","latitude","longitude","elevation","state","name","gsn flag","hcn/crn flag","wmo id"))
Stations <- read_fwf(file=paste0(dir$stations,"/ghcnd-stations.txt"), skip=0, col_positions = pos)
Stations <- as.data.frame(Stations)
Stations <- SpatialPointsDataFrame(Stations[,3:2], data=Stations)
proj4string(Stations) <- CRS(projection(map))
temp <- over(Stations, map)
stations <- Stations[!is.na(temp[,1]),] # Subset to stations falling over CONUS
rm(temp)

# Weather station data for 2020
Weather <- fread(paste0(dir$stations,"/2020.csv"))
Weather <- as.data.frame(Weather)
Weather <- Weather[Weather$V1 %in% stations@data$id, ] # Subset to CONUS
weather <- Weather[Weather$V2=="20200816",] # subset to one of hottest days of the year
weather <- weather[weather$V3=="TMAX",] # subset to tmax
weather$V4 <- weather$V4/10 # decimal point is missing


# 4. Land cover data ----

nlcd <- list.files(dir$landcover, recursive=T, full.names=T)
nlcd <- nlcd[extension(nlcd)==".img"]
nlcd <- raster(nlcd)


#===============================================================================.
# 3). Aggregate fine-scale 30m NLCD land cover data to PRISM grid ---------
#===============================================================================.


# Aggregation of land cover to grid and save to disk ----

# File name for weights
rname <- paste(dir$prism2,"/nlcd_prism_weights.RDS",sep="")

# Compute or read weights object from disk
if(file.exists(rname)) {
  
  rstack <- readRDS(rname)
  
} else { # Takes from 20min to 1 hour depending on your settings
  
  # Convert to polygon of grids and match land cover projection
  # This grid will be needed to compute "zonal" statistics of land cover
  # for each PRISM grid cell
  grid <- rasterToPolygons(id)
  grid <- spTransform(grid, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
  
  
  # Create tiles of polygons with PRISM grid cell ids
  # Takes ~3min
  system.time({
    
    # Raster ID list: split raster into 144 tiles
    ridlist <- splitRaster(id, nx=12 , ny=12) # break ID raster into tiles. (it starts as 144, but trims down to 107 tiles when NA are removed)
    
    # polygon ID list: convert these tiles of prism raster to sf polygons
    pidlist <- lapply(1:length(ridlist), function(i) { # i <- 1
      print(i)
      r <- ridlist[[i]]
      p <- rasterToPolygons(r, na.rm=FALSE)  # transform to polygons; (have to keep as na.rm false so it can loop and allow for tiles without values)
      p <- spTransform(p, CRS(projection(nlcd)))
      p <- st_as_sf(p) %>% filter(cellid!="NA") # remove NA polygons
      p
    }) # create polygon for these tiles
    
    # Remove empty tiles
    pidlist <- Filter(function(k) dim(k)[1]>0, pidlist)
    
  })
  
  # Loop over tiles (~1 hour serial, 20min with 4 cores)
  system.time({
    lout <- lapply(1:length(pidlist), function(t) { # serial
      #lout <- mclapply(1:length(pidlist), mc.cores=4, FUN=function(t) { # parallel
      
      print(paste("tile ",t,"/",length(pidlist),sep=""))
      
      # 1. Crop to tile ~ 12s
      system.time(d <- crop(nlcd,pidlist[[t]], snap="out"))
      
      # 2. rasterize tile of IDs ~ 1s
      system.time({
        idraster <- fasterize(pidlist[[t]], d, field="cellid")
        idraster <- crop(idraster, d)
      })
      
      # 3. Compute zonal stats using ddply ~ 30s
      system.time({
        dat <- data.frame(id=idraster[], value=d[])
        dat <- dat[!is.na(dat$id) & !is.na(dat$value),]
        out <- ddply(dat, "id", .progress="text", .fun=function(df) {
          count <- table(df$value)
          f <- rep(0,256)
          names(f) <- 0:255
          f[names(count)] <- round(count/sum(count)*100,3)
          f
        })
      })
      
      # 4. Export
      out
      
    }) # tile loop
  })
  
  # Merge & save df
  out <- do.call(rbind, lout)
  
  # Double check correct, sum to 100
  range(rowSums(out[,-1] ))
  
  # Store in raster
  r <- mask
  r[] <- NA
  rout <- lapply(1:255, function(i) {
    r[out$id] <- out[,paste(i)]
    r
  })
  rstack <- stack(rout)
  
  # Save to disk
  saveRDS(rstack, rname)
  
}

# Cultivated land ----

# Get cultivated land, pasture/hay and grasslands
# See classes here: https://www.mrlc.gov/data/legends/national-land-cover-database-2016-nlcd2016-legend
grass <- rstack[[71]]
past  <- rstack[[81]]
crop  <- rstack[[82]]
all <- (past+crop+grass)

#===============================================================================
# 4). Create transformation "P" matrix to aggregate raster to counties ------
#===============================================================================

# This section will be useful to generate data shown in Figure 7 of the chapter.

# 1. Extract cell IDs and cultivation weights for all counties ----

# Prepare extraction of cell IDs and cropland+pasture+grassland weights
# Takes ~ 90s
system.time({
  temp <- stack(id,all)
  info <- extract(temp, map, weights=TRUE, cellnumbers=TRUE)
  names(info) <- as.character(map@data$fips)
})

# Scale weights to 1 within each county
info <- lapply(names(info), function(i) {
  #print(i)
  df <- info[[paste(i)]]
  df <- as.data.frame(df)
  df$w <- df$layer/sum(df$layer,na.rm=T) # make them add to 1
  df <- df[!is.na(df$w),, drop=F] # drop=F, to make sure object stays as a matrix, even with 1 row
  df$fips.order <- match(i, names(info))
  df
})
names(info) <- as.character(map@data$fips)

# Double check
unique(sapply(info, function(x) sum(x$w))) # weights add to 1?
range(sapply(info, function(x) nrow(x))) # range of grid cell numbers inside counties

# Convert to dataframe that will go into our transformation "P" matrix
info <- do.call("rbind",info)
info <- info[,c("cellid","fips.order","w")]

# 2. Create projection "P" matrix to extract weather information ------

# We can use the info object to create weights to extract weather information
# for each country using matrix algebra. We need a transformation/projection
# matrix (named P) so that we can do the following:

# Say you have a
# A <- P %*% G

# where:
# G is a matrix with all the climate data, obtained as G <- g[], where g is a raster stack
# M has a dimension n x t, where n is the number of grid cells in the climate data, and t the number of time periods or raster layers
# A is a matrix with N rows (number of counties) and t columns (number of layers in g)
# P is our weight/projection matrix we need to construct
# P dimensions? n rows (number of grid cells) by N columns (number of countries)

# Let's start
g <- stack(s,s,s) # test with 3 raster layers
G <- g[]
dim(G) # should be 872505 (number of grid cell in PRISM) x 3 (number of layers)

# Create T matrix
P <- sparseMatrix(i=info$cellid,
                  j=info$fips.order,
                  x=info$w,
                  dims=c(ncell(g), nrow=length(unique(info$fips.order))))
colnames(P) <- as.character(map@data$fips)

# Double check that all columns sum to 1
unique(colSums(P))

# Test
A <- t(P) %*% G
dim(A)
head(A)

# Save projection matrix to disk
fname <- paste0(dir$prism2,"/p.RDS")
saveRDS(P, fname)




#===============================================================================
# 5). Interpolate weather stations to counties ------
#===============================================================================

# This section generates data that is shown in Figure 3 of the paper.

# The aggregation of weather station data follows a very similar pattern to
# the aggregation of gridded data. The whole point is to represent the source
# data as a matrix and to construct a sparse matrix that projects that matrix
# into the target dataset which will be interpolated. Each row of the projection
# matrix performs the local interpolation.


# Prepare data
stations2 <- stations[stations@data$id %in% weather[,1],] # stations with weather data
centroids <- SpatialPoints(coordinates(map))
proj4string(centroids) <- CRS(projection(map))
weather <- weather[match(stations2@data$id,weather[,1]),]
identical(weather[,1], stations2@data$id)

# Aggregation weights

# Stations within 0.5 degrees
delta1 <- 3 # distance around centroid of each county in degrees
nclose <- 1
dist1 <- nearest.dist(coordinates(stations2), coordinates(centroids), method = "greatcircle", delta=delta1)
dist1 <- 1/dist1
dist1 <- apply(dist1, 2, FUN=function(x) ifelse(rank(-x) %in% 1:nclose,x,0)) # get 5 closest distances
dist1 <- t(apply(dist1, 2, FUN=function(x) x/sum(x,na.rm=T))) # scale to 1
colnames(dist1) <- stations2@data$id
rownames(dist1) <- map@data$fips
P1 <- Matrix(dist1)

# 5 closest stations
delta2 <- 3 # distance around centroid of each county in degrees
nclose <- 5
dist2 <- nearest.dist(coordinates(stations2), coordinates(centroids), method = "greatcircle", delta=delta2)
dist2 <- 1/dist2
dist2 <- apply(dist2, 2, FUN=function(x) ifelse(rank(-x) %in% 1:nclose,x,0)) # get 5 closest distances
dist2 <- t(apply(dist2, 2, FUN=function(x) x/sum(x,na.rm=T))) # scale to 1
colnames(dist2) <- stations2@data$id
rownames(dist2) <- map@data$fips
P2 <- Matrix(dist2)

# Stations within 1 degree
delta3 <- 1 # distance around centroid of each county in degrees
dist3 <- nearest.dist(coordinates(stations2), coordinates(centroids), method = "greatcircle", delta=delta3)
dist3 <- 1/dist3
dist3 <- t(apply(dist3, 2, FUN=function(x) x/sum(x,na.rm=T))) # scale to 1
colnames(dist3) <- stations2@data$id
rownames(dist3) <- map@data$fips
P3 <- Matrix(dist3)


# Double check distance
if (F) {
  i <- 55
  plot(map, lwd=.1)
  plot(map[i,], add=T, col="black")
  plot(stations2[which(dist2[i,]>0),], add=T, pch=16, cex=.3, col="cyan")
  plot(stations2[which(dist1[i,]>0),], add=T, pch=16, cex=.3, col="blue")
  plot(stations2[which(dist3[i,]>0),], add=T, pch=16, cex=.3, col="red")
}

# Aggregate weather station to counties
# target    <- projection x source
weather.co1 <- P1 %*% cbind(weather$V4)
weather.co2 <- P2 %*% cbind(weather$V4)
weather.co3 <- P3 %*% cbind(weather$V4)

#===============================================================================
# 6). Interpolate weather stations to PRISM grid ------
#===============================================================================

# This section generates data that is shown in Figure 4 of the paper.

# Loop over weather variables
# Takes ~ 1 min
system.time({
  slist <- lapply(c("tmax","ppt"), function(var) {
    
    print("----------------")
    print(var)
    var2 <- ifelse(var=="tmax","TMAX","PRCP")
    
    # Import monthly PRISM data
    flist <- list.files(paste0(dir$prism,"/monthly/",var), full.names = T, recursive = T)
    flist <- flist[grepl("202008",flist)]
    flist <- flist[extension(flist)==".bil"]
    m <- raster(flist)
    
    # Import weather stations data
    weather <- Weather[grepl("202008",Weather$V2),] # subset to month
    weather <- weather[weather$V3==var2,] # subset to variable
    weather$V4 <- weather$V4/10 # decimal point is missing
    
    # Form a balanced panel of weather stations
    table(weather$V1) # not all stations have 31 observations
    hist(table(weather$V1), main=var) # distribution
    keep <- table(weather$V1)==max(table(weather$V1))
    keep <- names(keep)[keep]
    length(keep)  # number of stations in the panel
    stations.balanced <- stations[stations@data$id %in% keep, ]
    
    # Get PRISM raster grid centroids
    centroids <- coordinates(m)
    centroids <- centroids[mask[]==1,]
    coords.stations <- coordinates(stations.balanced)
    
    # Get 5 nearest stations to each PRISM gridcell
    k <- 5
    nn <- get.knnx(coords.stations, centroids, k=k)
    # nn[[1]][1,] # index of 5 nearest stations
    # plot(coords.stations, pch=16, cex=.5)
    # points(coords.stations[nn[[1]][1,],], col="red")
    
    # Compute the inverse distance weight for each one
    w <- 1/nn[[2]] # inverse distance
    w <- w/rowSums(w) # scaled to 1
    rowSums(w)
    
    # Create inverse distance weighting matrix
    dist <- sparseMatrix(i= rep(1:nrow(centroids),each=k), # row index - gridcells
                         j= c(t(nn[[1]])), # column index - stations
                         x= c(t(w)),
                         dims=c(nrow(centroids),nrow(coords.stations)))
    colnames(dist) <- stations.balanced@data$id
    
    # Loop over days of the month
    s <- lapply(unique(weather$V2), function(day) { # day <- 20200801
      
      print(day)
      
      # Subset weather to day
      d <- weather[weather$V2==day,]
      d <- d[match(colnames(dist),d$V1),]
      
      # Map to PRISM raster grids
      g <- dist %*% cbind(d$V4)
      g <- c(as.matrix(g))
      
      # Store in a raster
      r <- mask
      r[mask==1] <- g
      r[mask==0] <- NA
      plot(r, main=paste(var,"-",day))
      r
    })
    s <- stack(s)
    
    # Remove interpolated mean and add monthly PRISM mean
    if (var=="ppt") {
      stot <- sum(s)
      scale <- m/stot
      s <- s*scale
      test <- sum(s)
      fill <- which(is.na(test[]))
      fill <- fill[fill %in% which(mask[]==1)]
      s[][fill,] <- 0
      #plot(sum(s))
      #plot(m)
    } else {
      savg <- mean(s)
      s <- s - savg # Remove interpolated mean
      s <- s + m    # Add monthly PRISM mean
    }
    
    # Return
    names(s) <- unique(weather$V2)
    stack(s)
  })
})
names(slist) <- c("tmax","ppt")

#===============================================================================
# 7). Create monthly temperature exposure bins ------
#===============================================================================

# This section generates data that is shown in Figure 7 of the paper.

# Takes about 24 hours and 10GB for data for 1981-2020

# Create monthly list
flist <- list.files(paste0(dir$prism,"/daily"), full.names = T, recursive = T)
flist <- flist[extension(flist)==".bil"]
months <- rep(1981:2020, each=12)*100 + 1:12
missing0 <- mask[]==0 # # Create a logical vector to subset land
trez <- 15/60  # the time resolution of the interpolation
bins <- -10:50 # bins to that will be saved
binsbase <- seq(-100.5,100.5,1) # base bins over which computation are done

# Loop over months
system.time({
  lapply(months, function(mo) {
    
    print("--------------------------")
    timer <- system.time({
      # 1. Prepare data
      
      # Get files
      minfiles <- flist[grepl(mo,flist) & grepl("tmin",flist)]
      maxfiles <- flist[grepl(mo,flist) & grepl("tmax",flist)]
      ndays <- length(minfiles)
      print(paste(mo,"-",ndays,"days"))
      if (length(minfiles)!=length(maxfiles)) break
      # Add another day
      if (mo != max(months)) {
        mo2 <- months[match(mo, months)+1]
        minfiles <- c(minfiles, flist[grepl(mo2*100+1,flist) & grepl("tmin",flist)])
        maxfiles <- c(maxfiles, flist[grepl(mo2*100+1,flist) & grepl("tmax",flist)])
      } else {
        minfiles <- c(minfiles,minfiles[length(minfiles)])
        maxfiles <- c(maxfiles,maxfiles[length(maxfiles)])
      }
      
      # Read daily data to memory as matrices
      # Takes ~80s for 1 month worth of daily files
      system.time({
        tmin <- stack(minfiles)[]
        tmax <- stack(maxfiles)[]
      })
      
      
      # 2. interpolate matrix
      
      # Interpolate over rows that are over land
      # Takes ~ 60s
      system.time({
        m <- fun$sine.approx(tmin=tmin[!missing0,], tmax=tmax[!missing0,], trez=trez)
      })
      rm(tmin, tmax)
      gc()
      
      # Remove last 12 hours to get exactly ndays days
      remove <- (ncol(m)-((1/trez)*12)+1):ncol(m) # remove half day
      m <- m[,-remove]
      ncol(m)/4 # 744 hours for a month with 31 days with each column representing 15 mins
      
      # Quickly check the interpolation
      if (F) {
        plot(seq(1,32,length.out=ncol(m)),m[1,], type="l", ylab="Temperature", xlab="Day")
        points(1:31,tmin[!missing0,][1,c(1:31)], col="blue")
        points(1:31+.5,tmax[!missing0,][1,c(1:31)], col="red")
      }
      
      # 3. Compute bins
      
      # Split the matrix to facilitate bin computation
      index <- ceiling((1:nrow(m))/500)
      m_list <- split.data.frame(m,index)
      
      # Compute bins ~ 40s
      system.time({
        m2 <- lapply(1:length(unique(index)), function(j) {
          mat <- m_list[[j]]
          mat <- fun$exposuretobins(m=mat, bins=bins, binsbase=binsbase, trez=trez)
        })
        m2 <- do.call("rbind",m2)
      })
      
      # 4. Store bins in a raster file
      
      # Fill matrix
      mout  <- matrix(NA, nrow=length(missing0), ncol=length(bins))
      mout[!missing0,] <- m2
      
      # Create empty raster file for bins
      r <- mask
      r[] <- NA
      r <- stack(rep(list(r), length(bins)))
      names(r) <- gsub("-","m",paste("bin_",bins,sep=""))
      
      # Fill empty raster
      r[] <- mout
      names(r) <- paste0("bin",bins)
      
      # Check it out
      # plot(r[[20]])
      
      # Save to disk
      fname <- paste0(dir$bins,"/bins_",mo,".RDS")
      saveRDS(r, fname)
      
    })
    print(paste(round(timer[[3]]/60,1),"min"))
    
  })
})


#===============================================================================
# 8). Aggregate raster variables to the county level ------
#===============================================================================

# This section generates data that is used in the regression analysis in the chapter

# Loop over variables: ~ 35 mins
system.time({
  lapply(c("tmin","tmax","ppt","bins"), function(var) {
    
    print("---------------")
    print(var)
    
    # Read file list
    if (var=="bins") {
      flist <- list.files(paste0(dir$prism2,"/",var), full.names = T, recursive = T)
      flist <- flist[extension(flist)==".RDS"]
    } else {
      flist <- list.files(paste0(dir$prism,"/monthly/",var), full.names = T, recursive = T)
      flist <- flist[extension(flist)==".bil"]
    }
    
    # Loop over file list
    flist <- flist[order(flist)]
    d <- lapply(flist, function(f) { # f <- flist[1]
      
      #print(f)
      # Read data to a matrix
      if (var=="bins") r <- readRDS(f) else r <- raster(f)
      G <- r[]
      
      # Aggregate to county level
      A <- t(P) %*% G
      fips <- as.numeric(rownames(A))
      A <- as.matrix(A)
      if (var!="bins") colnames(A) <- var
      
      # Arrange for export
      ym <- rev(strsplit(f,"/")[[1]])[1]
      ym <- rev(strsplit(ym,"_")[[1]])[ifelse(var=="bins",1,2)]
      year <- as.numeric(substr(ym,1,4))
      month <- as.numeric(substr(ym,5,7))
      cat(c(year*100+month), sep="...")
      o <- data.frame(fips=fips, year=year, month=month, A)
      
    })
    
    # Combine aggregated data from all files
    d <- rbind.fill(d)
    d <- d[order(d$year,d$month,d$fips),]
    
    
    # Write to disk
    fname <- paste0(dir$data,"/prism_county_",var,"_",paste(range(d$year), collapse="-"),".csv")
    write.csv(d, fname, row.names = F)
    
  })
})

#===============================================================================
# 9). Visualizations ------
#===============================================================================

# Figure 1 -----
# Distribution of weather stations

fname <- paste0(dir$figures,"/fig1_map_stations.png")
png(fname, 1800, 2200, pointsize = 45)
par(mar=c(0,0,2,0), mfrow=c(2,1))
plot(world, lwd=1, border="grey70", col="grey95")
plot(Stations, pch=16, cex=.15, col="red3", add=T)
mtext("A", side=3, adj=.02, font=1, cex=2)
mtext("Global", side=3, font=3, cex=1.5)
plot(map, lwd=1, border="grey70", col="grey95")
plot(stations, pch=16, cex=.2, col="red3", add=T)
mtext("B", side=3, adj=.02, font=1, cex=2)
mtext("Contiguous US", side=3, font=3, cex=1.5)
dev.off()

# Figure 2 -----
# Plot raster and gridcell

# Prepare data and plotting objects
flist <- list.files(paste0(dir$prism,"/daily"), full.names = T, recursive = T)
flist <- flist[extension(flist)==".bil"]
s <- flist[grepl(20200816,flist) & grepl("tmax",flist)]
s   <- raster(s)
st <- map[map@data$STATE=="06",] # CA
s2 <- crop(s,st) # crop weather raster over CA
l <- crop(crop,st) # crop cropland raster over CA
inside <- rasterize(st, s2, field=1) # get raster cells inside the state
s2[is.na(inside[])] <- NA
l[is.na(inside[])] <- NA

# Settings for plot
breaks <- seq(5,55,5)
colors <- colorRampPalette(rev(brewer.pal(9,"Spectral")))(length(breaks)-1)
breaks2 <- seq(0,1,.1)
colors2 <- c(colorRampPalette(brewer.pal(9,"YlGn"))(length(breaks2)-1))
fname <- paste0(dir$figures,"/fig2_map_grid.png")

# Visualize
png(fname, width = 1000*2, height = 1000, pointsize = 35)
par(mar=c(2,2,2,2), oma=c(0,0,0,3), mfrow=c(1,2))
plot(s2, col=colors, axes=F, box=F, legend.width=2, legend.args=list(text='Maximum temperature (°C)', side=4, font=1, line=2.5, cex=1))
plot(st, lwd=1, add=T)
mtext("A", side=3, adj=-.1, font=1, cex=2)
plot(l/100, breaks=breaks2, col=colors2, axes=F, box=F, legend.width=2, legend.args=list(text='Fraction of cropland in PRISM gridcell', side=4, font=1, line=2.5, cex=1))
plot(st, lwd=1, add=T)
mtext("B", side=3, adj=-.1, font=1, cex=2)
dev.off()


# Figure 3 -----
# Weather station and interpolated values of Tmax for August 16 2020


# Plot settings
bgcol <- "grey30"
breaks <- seq(5,55,5)
bucket <- findInterval(weather$V4, brks)
colors <- colorRampPalette(rev(brewer.pal(9,"Spectral")))(length(breaks)-1)
o <- match(stations2@data$id,weather[,1])
colvec <- colors[bucket][o]
leg <- paste(brks[-length(brks)],brks[-1], sep="-")
colvec1 <- colors[findInterval(weather.co1, brks)]
colvec1[is.na( colvec1)] <- bgcol
colvec2 <- colors[findInterval(weather.co2, brks)]
colvec2[is.na( colvec2)] <- bgcol
colvec3 <- colors[findInterval(weather.co3, brks)]
colvec3[is.na( colvec3)] <- bgcol

# Plot
fname <- paste0(dir$figures,"/fig3_map_us_stations.png")
png(fname, width = 1800, height = 900*4, pointsize =  70)
par(mar=c(0,0,1,3), oma=c(0,0,1.5,0), xpd=T, mfrow=c(4,1))
# Stations
plot(map, lwd=.5, col=bgcol)
plot(stations2, pch=21, cex=.5, bg=colvec, add=T)
mtext("A", side=3, adj=.02, font=1, cex=2)
mtext("Weather stations", side=3, font=3, cex=1)
# Closest station
plot(map, lwd=.5, col=colvec1)
mtext("B", side=3, adj=.02, font=1, cex=2)
mtext("Nearest station to centroid", side=3, font=3, cex=1)
# 5 closest stations
plot(map, lwd=.5, col=colvec2)
mtext("C", side=3, adj=.02, font=1, cex=2)
mtext("Interpolation of 5 nearest stations", side=3, font=3, cex=1)
# Stations within 1 degree
plot(map, lwd=.5, col=colvec3)
mtext("D", side=3, adj=.02, font=1, cex=2)
mtext("Interpolation of stations within 1°", side=3, font=3, cex=1)
legend("bottomright", rev(c("n/a",leg)), pt.bg=rev(c(bgcol,colors)), pch=21, inset=c(-.1,0), title="Tmax (°C)", bty="n", cex=1, pt.cex=2)
dev.off()



# Figure 4 -----

# Import daily PRISM
flist <- list.files(paste0(dir$prism,"/daily/tmax"), full.names = T, recursive = T)
flist <- flist[extension(flist)==".bil"]
stmax <- flist[grepl(202008,flist) & grepl("tmax",flist)]
length(stmax)==31 # make sure this is 31, otherwise some daily files are missing
flist <- list.files(paste0(dir$prism,"/daily/ppt"), full.names = T, recursive = T)
flist <- flist[extension(flist)==".bil"]
sppt  <- flist[grepl(202008,flist) & grepl("ppt",flist)]
length(sppt)==31 # make sure this is 31, otherwise some daily files are missing
stmax <- stack(stmax)
sppt <- stack(sppt)
diff.tmax <- slist$tmax[["X20200816"]]-stmax[[16]]
diff.ppt <- slist$ppt[["X20200816"]]-sppt[[16]]

# Colors
breaks <- seq(5,55,5)
colors <- colorRampPalette(rev(brewer.pal(9,"Spectral")))(length(breaks)-1)
breaks2 <- c(seq(-14,-2,4),seq(2,14,4))
colors2 <- colorRampPalette((brewer.pal(7,"RdBu")))(length(breaks2)-1)
breaks3 <- seq(0,275,25)
breaks3 <- c(0,1,breaks3[-1])
colors3 <- c("white",colorRampPalette(c("aliceblue","blue","magenta"))(length(breaks3)-2))
breaks4 <- seq(-275,275,50)
colors4 <- colorRampPalette((brewer.pal(9,"RdBu")))(length(breaks4)-1)

# Plot
figname <- paste0(dir$figures,"/fig4_interpol_prism.png")
png(figname,2400,900, pointsize=30)
par(mfrow=c(2,3), oma=c(0,7,2,2), mar=c(0,1,1,1))

# TMAX
plot(slist$tmax[["X20200816"]], breaks=breaks, col=colors, axes=F, box=F, legend.width=1.5)
plot(us, add=T, lwd=1)
mtext("Daily interpolation", side=3, las=1, font=3, line=0)
mtext("TMAX (°C)", side=2, las=2, line=1, at=42, font=3)
mtext("A", side=3, adj=.02, font=1, cex=2)

plot(stmax[[16]], breaks=breaks, col=colors, axes=F, box=F, legend.width=1.5)
plot(us, add=T, lwd=1)
mtext("Daily PRISM", side=3, font=3, line=0)
mtext("B", side=3, adj=.02, font=1, cex=2)

plot(diff.tmax,  breaks=breaks2, col=colors2, axes=F, box=F, legend.width=1.5)
plot(us, add=T, lwd=1)
mtext("Difference", side=3, font=3, line=0)
mtext("C", side=3, adj=.02, font=1, cex=2)

# PPT
plot(slist$ppt[["X20200816"]], breaks=breaks3, col=colors3, axes=F, box=F, legend.width=1.5)
plot(us, add=T, lwd=1)
mtext("PPT (mm)", side=2, las=2, line=1, at=42, font=3)
mtext("D", side=3, adj=.02, font=1, cex=2)

plot(sppt[[16]], breaks=breaks3, col=colors3, axes=F, box=F, legend.width=1.5)
plot(us, add=T, lwd=1)
mtext("E", side=3, adj=.02, font=1, cex=2)

plot(diff.ppt, breaks=breaks4, col=colors4, axes=F, box=F, legend.width=1.5)
plot(us, add=T, lwd=1)
mtext("F", side=3, adj=.02, font=1, cex=2)

dev.off()

# Figure 5 -----
# Land cover fraction of cropland+pasture+grassland over each PRISM gridcell

# Settings
cuts <- seq(0,1,.1)
pal <- c(colorRampPalette(brewer.pal(9,"YlGn"))(length(cuts)-1))

# Plot
fname <- paste0(dir$figures,"/fig5_map_landcover_prism.png")
png(fname, width = 2000, height = 1000, pointsize = 30)
par(mar=c(0,0,0,0), xpd=T)
plot(all/max(all[], na.rm=T), axes=F, box=F, breaks=cuts, col=pal, legend.width=1.5, legend.args = list(text = 'Fraction of cropland, pasture and grasslands in PRISM gridcell', side = 4, font = 1, line = 2.5, cex = 1))
plot(map, add=T, lwd=.75)
dev.off()




# Figure 6 -----
# Show a sequence of daily Tmax and Tmin over a month as well as the interpolated
# temperatures based on a double sine curve.

# Select time period and get files
mo <- 202008
minfiles <- flist[grepl(mo,flist) & grepl("tmin",flist)] # flist list defined in section 6). of the code
maxfiles <- flist[grepl(mo,flist) & grepl("tmax",flist)]

# Read daily data to memory as matrices
tmin <- stack(minfiles)[]
tmax <- stack(maxfiles)[]
missing0 <- mask[]==0 # # Create a logical vector to subset land
trez <- 15/60  # the time resolution of the interpolation
bins <- -10:50 # bins to that will be saved
binsbase <- seq(-100.5,100.5,1) # base bins over which computation are done

# Interpolate over rows that are over land
system.time(m <- fun$sine.approx(tmin=tmin[!missing0,], tmax=tmax[!missing0,], trez=trez))

# Compute bins ~ 40-60s
index <- ceiling((1:nrow(m))/500) # Split the matrix to facilitate bin computation
m_list <- split.data.frame(m,index)
system.time({
  m2 <- lapply(1:length(unique(index)), function(j) {
    mat <- m_list[[j]]
    mat <- fun$exposuretobins(m=mat, bins=bins, binsbase=binsbase, trez=trez)
  })
  m2 <- do.call("rbind",m2)
})

# Settings
set.seed(8675309)
i <- sample(1:nrow(m),1)
cols <- c("red1","royalblue","grey50","green3")
ylim=c(0,35)
binrange <- c(floor(min(tmin[!missing0,][i,c(1:31)])): floor(max(tmax[!missing0,][i,c(1:31)])))

# Plot
if (T) {
  png(paste0(dir$figures,"/fig6_temperature_bins.png"),1400,800, pointsize=30)
  par(mar=c(3.5,3.5,1,9))
  plot(seq(1,31.5,length.out=ncol(m)),m[i,], type="p", ylab="", xlab="", axes=F, pch=16, cex=.3, ylim=ylim, col=cols[3])
  abline(h=bins, lty=2, lwd=.5)
  points(1:31,tmin[!missing0,][i,c(1:31)], bg=cols[2], pch=21, cex=.8)
  points(1:31+.5,tmax[!missing0,][i,c(1:31)], bg=cols[1], pch=21, cex=.8)
  # Outer
  axis(1, at=1:32, labels=NA, tck=-0.015)
  axis(1, at=c(1,5,10,15,20,25,31))
  axis(2, las=2)
  #axis(4, labels=F, tck=0.025)
  mtext("Temperature time distribution", col=cols[4], side=4, line=6.5, at=mean(range(ylim+c(10,0))))
  mtext("(31 days = 744 hours total)", col=cols[4], side=4, line=7.5, at=mean(range(ylim+c(10,0))), font=3)
  mtext("Day of the month", side=1, line=2)
  mtext("Temperature (°C)", side=2, line=2.5)
  box()
  legend("bottom", c("Daily tmax","Daily tmin","Sine interpolation (15 mins)","1°C bin intervals"),
         col=c(1,1,cols[3],1), pt.bg=c(cols[c(1:2)],NA,NA),
         lwd=c(NA,NA,NA,1), lty=c(NA, NA, NA, 2), pch=c(21,21,16, NA), pt.cex=c(1,1,.8,NA),
         inset=.02, ncol=2, cex=1, x.intersp=0)
  # Histogram
  par(xpd=T)
  lapply(binrange, function(x) rect(xleft = 33.25, xright = 33.25+m2[i,paste(x)]/7, ybottom = x, ytop = x+1, col=cols[4]))
  par(xpd=F)
  dev.off()
}


# Figure 7 -----
# Exposure above 30C for the month of August 2020

# Prepare data
#r <- readRDS(paste0(dir$bins,"/bins_198107.RDS"))
r <- readRDS(paste0(dir$bins,"/bins_202008.RDS"))
above30C <- sum(r[[which(bins>=30)]])
breakpoints <- seq(0,900,50)
colors <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(length(breakpoints)-1)

# Plot map
png(paste0(dir$figures,"/fig7_map_above30C.png"),2000,1000, pointsize=30)
par(mar=c(0,0,0,0))
plot(above30C,breaks=breakpoints,col=colors, box=F, axes=F, legend.width=1.5, legend.args=list(text='Exposure >30°C (hours)', side=4, font=2, line=2.5, cex=1))
plot(map, add=T, lwd=.5)
dev.off()

#===============================================================================
# Description: Estimate non-linear effects of temperature exposure on US corn
# yields based on step functions, natural cubic splines and Chebyshev polynomials.
# The script file creates figures 8-10 in the chapter.
#===============================================================================

#===============================================================================
# 1). Preliminary ------
#===============================================================================

# Clean up workspace
rm(list=ls())

# Clean and load packages
wants <- c("zoo","raster","sp","rgdal","RColorBrewer","splines","plyr","lfe","Matrix")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# Directories
dir <- list()
dir$root <- dirname(getwd())
dir$yields <- paste0(dir$root,"/data/yields")
dir$prism <- paste0(dir$root,"/data2/PRISM_co")
dir$figures <- paste0(dir$root,"/figures")
dir$map <- paste0(dir$root,"/data/gz_2010_us_050_00_20m")
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))


#===============================================================================
# 2). Import and manipulate data ------
#===============================================================================

# Map of US counties
map <- readOGR(paste0(dir$map,"/gz_2010_us_050_00_20m.shp"),"gz_2010_us_050_00_20m")
map <- map[!(map@data$STATE %in% c("15","02","72")), ] # drop Hawaii, Alaska and Puerto Rico
map <- spTransform(map, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) # nice projection
map@data$fips <- as.numeric(map@data$STATE)*10^3 + as.numeric(map@data$COUNTY)
eastfips <- map@data$fips[ coordinates(map)[,1] > -100 ]
#plot(map, col=ifelse(map@data$fips %in% eastfips, "green3","grey90"), lwd=.5, main="CONUS counties east of 100th meridian west")

# Corn yield data
flist <- list.files(dir$yields, full.names=T)
yields <- lapply(flist, function(f) {
  d <- read.csv(f)
  d$statefips <- d$State.ANSI
  d$fips <-  d$statefips*10^3 +  d$County.ANSI
  d$dist <-  d$statefips*10^3 +  d$Ag.District.Code
  d$yield <- d$Value
  d$year <- d$Year
  d <- d[,c("statefips","dist","fips","year","yield")]
  d
})
yields <- do.call("rbind",yields)
yields <- yields[order(yields$fips, yields$year),]

# County-level PRISM data
flist <- list.files(dir$prism, full.names = T, recursive = T)
weather <- lapply(flist, function(f) as.data.frame(fread(f)))
names(weather) <- sapply(strsplit(flist,"_"), function(x) rev(x)[2])
identical(weather[[1]][,1:3],weather[[2]][,1:3])
identical(weather[[1]][,1:3],weather[[3]][,1:3])
identical(weather[[1]][,1:3],weather[[4]][,1:3])
weather <- data.frame(weather$ppt, tmax=weather$tmax[,-c(1:3)], tmin=weather$tmin[,-c(1:3)], weather$bins[,-c(1:3)])
names(weather) <- gsub("[.]","-",names(weather))
weather$tmean <- (weather$tmax + weather$tmin)/2
weather <- weather[,c("fips", "year", "month","ppt","tmax","tmin","tmean",names(weather)[grepl("bin",names(weather))])]

#===============================================================================
# 4). Estimation of non-linear effects ------
#===============================================================================

# 0. Prepare data -----

# Settings
season <- 4:9 # April-September
tails  <- 0.01 # minimum density of extreme bins (top/bottom coding)

# Data aggregation
w <- weather[weather$month %in% season, ]
w <- as.data.table(w)
w1 <- w[, lapply(.SD, sum), by=.(fips, year), .SDcols=c("ppt",names(w)[grepl("bin",names(w))])]
w2 <- w[, lapply(.SD, mean), by=.(fips, year), .SDcols=c("tmax","tmin","tmean")]
w <- merge(w2,w1)
rm(w1,w2)
w <- as.data.frame(w)

# Regression data
regdata <- merge(yields,w, by=c("fips","year"))
regdata <- regdata[regdata$fips %in% eastfips, ] # easter counties only

# Drop yield equal to zero (suspicious)
sum(regdata$yield==0) # 212 cases
regdata[which(regdata$yield==0),1:8]
regdata <-   regdata[regdata$yield>0,]

# 1. Step functions (+ Fig 8) ------

# Parameters
bins <- 0:38 # bins
dfs <- c(1,3,7) # width of the step functions

# Prepare data, run regressions, and export marginal effects
out1 <- lapply(dfs, function(df) {
  
  print(df)
  
  # Prepare data
  steps <- seq(min(bins),max(bins),df)
  idx <- rep(1:length(steps),each=df)
  idx <- idx[1:length(bins)]
  # If only one small step function at the end, aggregate with the previous step
  if (table(idx)[max(idx)]==1) idx[length(idx)] <- idx[length(idx)-1]
  B <- sparseMatrix(i=1:length(bins), j=idx, x=1)
  bindata <- regdata[,grepl("bin",names(regdata))]/24 # in days
  center <- bindata[,paste0("bin",bins[-c(1,length(bins))])]
  left   <- bindata[,1:(match(paste0("bin",bins[1]), names(bindata))-1)]
  right  <- bindata[,(match(paste0("bin",bins[length(bins)]), names(bindata))+1):ncol(bindata)]
  bindata <- cbind(rowSums(left),center,rowSums(right))
  names(bindata) <- paste0("bin",bins)
  bdata <- as.matrix(bindata) %*% B
  bdata <- as.matrix(bdata)
  colnames(bdata) <- paste0("step",unique(idx))
  rdata <- regdata[,c("statefips","dist","fips","year","yield","ppt","tmin","tmax","tmean")]
  rdata <- cbind(rdata,bdata)
  dens <- colMeans(bindata)
  names(dens) <- bins
  dens <- dens/sum(dens)
  
  # Formula
  f <- paste("log(yield) ~",paste(colnames(bdata), collapse=" + "), "+ ppt + I(ppt^2) + year + I(year^2)")
  f <- paste(f,"| fips | 0 | statefips + year")
  f <- as.formula(f)
  
  # Run model
  reg <- felm(f, rdata)
  summary(reg)
  
  # Get marginal effects
  me <- B %*% cbind(coef(reg)[1:length(unique(idx))])
  me <- as.matrix(me)
  
  # Exposure-center marginal effects
  me <- me - sum(me*(dens/sum(dens)))
  
  # Get confidence bands
  se <- B %*% vcov(reg)[1:length(unique(idx)),1:length(unique(idx))] %*% t(B)
  se <- sqrt(diag(se))
  
  # Quick view
  if (F) {
    plot(bins, me, type="s")
    lines(bins, me+1.96*se, type="s")
    lines(bins, me-1.96*se, type="s")
  }
  
  # Export
  list(df=df, me=me, se=se, dens=dens, steps=steps, idx=idx)
  
})
names(out1) <- dfs

# Figure 8
if (T) {
  # Header
  fname <- paste0(dir$figures,"/fig8_step.png")
  png(fname, width = 2200, height = 800, pointsize = 40)
  par(mar=c(1,2,2,2), oma=c(3,3,2,0), mfcol=c(1,3), lwd=2, cex.axis=1.2)
  
  # Loop over degrees of freedom
  lapply(dfs, function(df) {
    
    print(df)
    
    # Parameters
    colors2 <- c("darkblue", adjustcolor("steelblue", alpha.f = .4))
    lwd <- 3
    bot <- out1[[paste(df)]]$me - 1.96 * out1[[paste(df)]]$se
    top <- out1[[paste(df)]]$me + 1.96 * out1[[paste(df)]]$se
    bot2 <- out1[[paste(df)]]$me - 2.58 * out1[[paste(df)]]$se
    top2 <- out1[[paste(df)]]$me + 2.58 * out1[[paste(df)]]$se
    ylim2 <- c(-.15,.05)
    dens <- out1[[paste(df)]]$dens
    # Remove repeated values
    top <- top[!duplicated(top)]
    top2 <- top2[!duplicated(top2)]
    bot <- bot[!duplicated(bot)]
    bot2 <- bot2[!duplicated(bot2)]
    bins2 <- c(out1[[paste(df)]]$steps, max(bins))
    
    # Panel
    plot(bins, out1[[paste(df)]]$me, type="s", axes=F, xlab="", ylab="", col=colors2[1], ylim=ylim2, lwd=lwd+1)
    for(i in 1:(length(bins2)-1)) {
      polygon(x=c(bins2[c(i,i+1)],rev(bins2[c(i,i+1)])), y=c(top2[c(i,i)],rev(bot2[c(i,i)])), col=colors2[2], border=NA)
      polygon(x=c(bins2[c(i,i+1)],rev(bins2[c(i,i+1)])), y=c(top [c(i,i)],rev(bot[c(i,i)])), col=colors2[2], border=NA)
    }
    abline(h=0, lty=2)
    axis(1)
    axis(2, las=2)
    box()
    if (df==dfs[1]) mtext("Response function", side=2, cex=1, font=3, line=3.5)
    mtext("Temperature bin (°C)", side=1, font=1, cex=1, line=2.5)
    mtext(paste0(df,"°C step"), side=3, font=3)
    # Add distributions
    scale <- max(dens) * 10
    for (i in 1:length(dens)) rect(xleft=bins[i]-.5, xright = bins[i]+.5, ybottom = min(ylim2), ytop = min(ylim2) + dens[i]*scale, col="green3", border="green4")
    mtext("Growing-season distribution", side=1, font=1, cex=.8, line=-5, col="green4")
    
  })
  mtext("Step function", side=3, outer=T, cex=1.2, font=2)
  dev.off()
}

# 2. Natural cubic spline (+ Fig 9)------

# Parameters
bins <- 0:38 # bins
dfs <- c(3,7,12) # degrees of freedom

# Prepare data, run regressions, and export marginal effects
out2 <- lapply(dfs, function(df) {
  
  print(df)
  
  # Prepare data
  B <- ns(bins,df, intercept = T)
  bindata <- regdata[,grepl("bin",names(regdata))]/24 # in days
  center <- bindata[,paste0("bin",bins[-c(1,length(bins))])]
  left   <- bindata[,1:(match(paste0("bin",bins[1]), names(bindata))-1)]
  right  <- bindata[,(match(paste0("bin",bins[length(bins)]), names(bindata))+1):ncol(bindata)]
  bindata <- cbind(rowSums(left),center,rowSums(right))
  names(bindata) <- paste0("bin",bins)
  bdata <- as.matrix(bindata) %*% B
  colnames(bdata) <- paste0("ns",1:df)
  rdata <- regdata[,c("statefips","dist","fips","year","yield","ppt","tmin","tmax","tmean")]
  rdata <- cbind(rdata,bdata)
  dens <- colMeans(bindata)
  names(dens) <- bins
  dens <- dens/sum(dens)
  
  # Formula
  f <- paste("log(yield) ~",paste(colnames(bdata), collapse=" + "), "+ ppt + I(ppt^2) + year + I(year^2)")
  f <- paste(f,"| fips | 0 | statefips + year")
  f <- as.formula(f)
  
  # Run model
  reg <- felm(f, rdata)
  summary(reg)
  
  # Get marginal effects
  me <- B %*% cbind(coef(reg)[1:df])
  
  # Exposure-center marginal effects
  me <- me - sum(me*(dens/sum(dens)))
  
  # Get confidence bands
  se <- B %*% vcov(reg)[1:df,1:df] %*% t(B)
  se <- sqrt(diag(se))
  
  # Quick view
  #plot(bins, me, type="l")
  #lines(bins, me+1.96*se)
  #lines(bins, me-1.96*se)
  
  # Export
  list(df=df, me=me, se=se, dens=dens)
  
})
names(out2) <- dfs

# The end