# Effect of forest fires on Amazon vertebrates
# Elildo Carvalho Jr @ ICMBio/CENAP
# 2020-09-09


## ----Load libraries------------------------
#library(TeachingDemos)
library(lubridate)
#library(unmarked)
#library(ggplot2)
library(dplyr)
#library(chron)
#library(vegan)
#library(activity)
#library(ggmap)
library(here)

## ----Source this file--------
source(here("bin", "ahumada_codes.R"))

## ----Load data -------
data <- read.csv(here("data", "Wild_ID_RBG_ROADS_2015to2017_processed.csv"))

# minor adjustments
data$Camera.Trap.Name <- as.factor(data$Camera.Trap.Name)
data$td.photo <- as.POSIXct(data$td.photo)
data$Start.Date <- as.Date(data$Start.Date)
data$End.Date <- as.Date(data$End.Date)
data$Photo.Date <- as.Date(data$Photo.Date)

## ----Filter independent records-------
# Group by events that are 60 minutes apart (and from different species)
data <- f.separate.events(data, 60)
# filter retaining only independent records (distinctive combinations of bin and grp)
data <- distinct(data, bin, grp, .keep_all = TRUE)


# separate years
data2015 <- subset(data, Sampling.Event == 2015)
data2016 <- subset(data, Sampling.Event == 2016)
data2017 <- subset(data, Sampling.Event == 2017)

## ---- Estimate photgraphic capture rates per CT per species-------

photo.rates <- function(data, species) { # , z) {           # x = dataframe, y=species, z=Sampling.Event
  # first create Sampling.Unit.Name
  data$Sampling.Unit.Name <- data$Camera.Trap.Name
  #cams <- unique(data$Sampling.Unit.Name)
  cams <- levels(data$Sampling.Unit.Name)
  cams <- sort(cams)
  rows <- length(cams)
  effort <- rep(NA, rows)
  n.photos <- rep(NA, rows)
  RAI <- rep(NA, rows)
  newdata <- data.frame(cams,effort, n.photos,RAI)
  
  # fill object
  df.sp <- subset(data, bin == species)
  
  #for all cameras, determine the open and close date
  start.dates <- tapply(as.character(data$Start.Date), data$Sampling.Unit.Name, unique)
  nms <- names(start.dates)
  start.dates <- ymd(start.dates)
  names(start.dates) <- nms
  
  end.dates <- tapply(as.character(data$End.Date), data$Sampling.Unit.Name, unique)
  end.dates <- ymd(end.dates)
  names(end.dates) <- nms
  
  # calculate effort, number of (independent) photos and RAI
  newdata$effort <- as.numeric(end.dates-start.dates)
  for(i in 1:nrow(newdata))  # counter
  {
    newdata$n.photos[i] <- nrow(subset(df.sp, Sampling.Unit.Name==newdata[i,1]))
    newdata$RAI <- (newdata$n.photos/newdata$effort)*100
  }
  assign("newdata", newdata, envir = globalenv())
  #print(newdata)
  return(newdata)
}


# Test function with Dasyprocta prymnolopha
photo.rates(data2015, "Dasyprocta prymnolopha")
agouti2015 <- newdata

photo.rates(data2016, "Dasyprocta prymnolopha")
agouti2016 <- newdata

photo.rates(data2017, "Dasyprocta prymnolopha")
agouti2017 <- newdata

# bind RAIs from different years
agouti <- cbind(agouti2015[,c(1,4)], agouti2016[,c(4)], agouti2017[,c(4)])
names(agouti) <- c("site", "RAI.15", "RAI.16", "RAI.17")

# difference
agouti$diff15to16 <- agouti$RAI.15-agouti$RAI.16


#---- Covariates ----

# read covars file
covars <- read.csv(here("data", "baci_covars.csv"))

# unify coordinates for all sites-years in the dataset
covars$site <- covars$Camera.Trap.Name
agouti <- merge(agouti[,c(1,5)], covars[,c(8,7)], by="site", all.x=T, all.y=F)
agouti <- agouti[complete.cases(agouti),]

burned <- subset(agouti, burned=="y")
burned <- burned$diff15to16

unburned <- subset(agouti, burned=="n")
unburned <- unburned$diff15to16

# now use burned and unburned as inputs in Bayesian inference

#-------------------------------------------------------

# Test function with Psophia obscura
photo.rates(data2015, "Psophia obscura")
psophia2015 <- newdata

photo.rates(data2016, "Psophia obscura")
psophia2016 <- newdata

photo.rates(data2017, "Psophia obscura")
psophia2017 <- newdata

# bind RAIs from different years
psophia <- cbind(psophia2015[,c(1,4)], psophia2016[,c(4)], psophia2017[,c(4)])
names(psophia) <- c("site", "RAI.15", "RAI.16", "RAI.17")

# difference
psophia$diff15to16 <- psophia$RAI.15-psophia$RAI.16


#---- Covariates ----

# read covars file
covars <- read.csv(here("data", "baci_covars.csv"))

# unify coordinates for all sites-years in the dataset
covars$site <- covars$Camera.Trap.Name
psophia <- merge(psophia[,c(1,5)], covars[,c(8,7)], by="site", all.x=T, all.y=F)
psophia <- psophia[complete.cases(psophia),]

burned <- subset(psophia, burned=="y")
burned <- burned$diff15to16

unburned <- subset(psophia, burned=="n")
unburned <- unburned$diff15to16



#--------------------------------------------------------------------------------










#------------------------------------------------------------------------------

#--------------------------------------------------------------------------
# Plot difference RAIs (BACI) against burned area



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
