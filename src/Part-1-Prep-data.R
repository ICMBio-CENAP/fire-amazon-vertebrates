# Effect of forest fires on Amazon vertebrates
# Elildo Carvalho Jr @ ICMBio/CENAP
# 2020-09-09


## ----Install these packages if you have not already---------------------------
install.packages(c("TeachingDemos","lubridate","unmarked","ggplot2","dplyr","chron","vegan", "activity", "ggmap"), repos="https://cloud.r-project.org")

## ----Load libraries------------------------
#library(TeachingDemos)
#library(lubridate)
#library(unmarked)
#library(ggplot2)
#library(dplyr)
#library(chron)
#library(vegan)
#library(activity)
#library(ggmap)
library(here)

## ----Source this file--------
#source("camera trap analysis code-WILDID-09-20-17.R")

## ----Load data -------
data <- read.csv(here("data", "Wild_ID_RBG_ROADS_2015to2017_processed.csv"))


## ---- Estimate photgraphic capture rates per CT per species-------

photo.rates <- function(x, y) { # , z) {           # x = dataframe, y=species, z=Sampling.Event
  # first create Sampling.Unit.Name
  x$Sampling.Unit.Name <- x$Camera.Trap.Name
  cams <- unique(x$Sampling.Unit.Name)
  cams <- sort(cams)
  rows <- length(cams)
  effort <- rep(NA, rows)
  n.photos <- rep(NA, rows)
  RAI <- rep(NA, rows)
  newdata <- data.frame(cams,effort, n.photos,RAI)
  
  # fill object
  df.sp <- subset(x, bin == y)
  
  #for all cameras, determine the open and close date
  start.dates<-tapply(as.character(x$Start.Date),x$Sampling.Unit.Name,unique)
  nms<-names(start.dates)
  start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  
  end.dates<-tapply(as.character(x$End.Date),x$Sampling.Unit.Name,unique)
  end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  # calculate effort, number of (independent) photos and RAI
  newdata$effort <- as.numeric(end.dates-start.dates)
  for(i in 1:nrow(newdata))  # counter
  {
    newdata$n.photos[i] <- nrow(subset(df.sp, Sampling.Unit.Name==newdata[i,1]))
    newdata$RAI <- (newdata$n.photos/newdata$effort)*100
  }
  assign("newdata", newdata, envir = globalenv())
  print(newdata)
}

# improve following lines, check false NAs, put everything within the above function etc...

# Dasyprocta prymnolopha
icmbio(data.2015, "Dasyprocta prymnolopha")
daspry.2015 <- newdata
icmbio(data.2016, "Dasyprocta prymnolopha")
daspry.2016 <- newdata
daspry <- data.frame(daspry.2015$cams, daspry.2015$RAI, daspry.2016$RAI)
colnames(daspry) <- c("Camera.Trap.Name", "photo.rate.2015", "photo.rate.2016")
daspry$dif <- daspry$photo.rate.2015-daspry$photo.rate.2016
daspry
hist(daspry$dif, xlab="Difference in photographic capture rate, 2015-2016")




#------------------------------------------------------------------------------

#--------------------------------------------------------------------------
# Plot difference RAIs (BACI) against burned area

# read covars file
covars <- read.csv(here("data", "baci_covars.csv"))


# make a subset of covars to daspryand then match the order
covars <- subset(covars, Camera.Trap.Name %in% unique(daspry$Camera.Trap.Name))
covars$Camera.Trap.Name <- factor(covars$Camera.Trap.Name)
covars <- covars[match(covars$Camera.Trap.Name, daspry$Camera.Trap.Name),]

hist(covars$prop.buf.500m.burned, xlab="Proportion of buffer burned", main="", col="grey")

# add last column of covars to daspry
daspry$prop.buf.500m.burned <- covars$prop.buf.500m.burned
daspry$fire <- covars$fire.2016
daspry$forest <- covars$mapbiom.forest.pixels
# add last column of covars to tapter
tapter$prop.buf.500m.burned <- covars$prop.buf.500m.burned
tapter$forest <- covars$mapbiom.forest.pixels
# add last column of covars to certho
certho$prop.buf.500m.burned <- covars$prop.buf.500m.burned
certho$forest <- covars$mapbiom.forest.pixels
# add last column of covars to leopar
leopar$prop.buf.500m.burned <- covars$prop.buf.500m.burned
leopar$forest <- covars$mapbiom.forest.pixels
# add last column of covars to pautub
pautub$prop.buf.500m.burned <- covars$prop.buf.500m.burned
pautub$fire <- covars$fire.2016
pautub$forest <- covars$mapbiom.forest.pixels

# glm daspry
mod1 <- with(daspry, glm(dif~forest+prop.buf.500m.burned))
summary(mod1)
