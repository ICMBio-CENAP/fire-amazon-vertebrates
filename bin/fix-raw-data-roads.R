# Read and fix raw data

##----- 1 - Load libraries-----
library(stringr)
library(here)
library(tidyverse)
library(dplyr)


## ----Source this file--------
source(here("bin", "ahumada_codes.R"))
#source(here("bin", "camera trap analysis functions-10-06-18.R"))
source(here("bin", "time-lag.R"))
source(here("bin", "check-coords.R"))


##----- 2 - Read and fix raw data-----
roads <- f.readin.fix.data(here("data", "Wild_ID_RBG_ROADS_2015to2017.csv"))
colnames(roads)[10] <- "Photo.Time"

##----- 2015 -----
roads2015 <- subset(roads, Sampling.Event == 2015)
roads2015$Camera.Trap.Name <- as.factor(roads2015$Camera.Trap.Name)
levels(roads2015$Camera.Trap.Name)


# check date errors
min(roads2015$Camera.Start.Date); max(roads2015$Camera.End.Date)
min(roads2015$Photo.Date); max(roads2015$Photo.Date)
sort(unique(roads2015$Photo.Date))
roads2015 <- subset(roads2015, Photo.Date <= "2015-12-31")

# check the time lag between start date and 1st photo, last photo and end date
# if lag is too large there is still something wrong in dates, fix by redefining start and end dates 
time.lag(roads2015) # check

# wrong starts, fix using day of last photo
roads2015[roads2015$Camera.Trap.Name=="CT-RBG-E-06",]$Camera.Start.Date <- min(subset(roads2015, Camera.Trap.Name=="CT-RBG-E-06")$Photo.Date)-1
# wrong ends, fix using day of last photo
roads2015[roads2015$Camera.Trap.Name=="CT-RBG-E-08",]$Camera.End.Date <- max(subset(roads2015, Camera.Trap.Name=="CT-RBG-E-08")$Photo.Date)+1
roads2015[roads2015$Camera.Trap.Name=="CT-RBG-E-13",]$Camera.End.Date <- max(subset(roads2015, Camera.Trap.Name=="CT-RBG-E-13")$Photo.Date)+1
roads2015[roads2015$Camera.Trap.Name=="CT-RBG-E-17",]$Camera.End.Date <- max(subset(roads2015, Camera.Trap.Name=="CT-RBG-E-17")$Photo.Date)+1
roads2015[roads2015$Camera.Trap.Name=="CT-RBG-E-19",]$Camera.End.Date <- max(subset(roads2015, Camera.Trap.Name=="CT-RBG-E-19")$Photo.Date)+1
roads2015[roads2015$Camera.Trap.Name=="CT-RBG-E-35",]$Camera.End.Date <- max(subset(roads2015, Camera.Trap.Name=="CT-RBG-E-35")$Photo.Date)+1

time.lag(roads2015) # check


##----- 2016 -----
roads2016 <- subset(roads, Sampling.Event == 2016)
roads2016$Camera.Trap.Name <- as.factor(roads2016$Camera.Trap.Name)
levels(roads2016$Camera.Trap.Name)


# check date errors
min(roads2016$Camera.Start.Date); max(roads2016$Camera.End.Date)
min(roads2016$Photo.Date); max(roads2016$Photo.Date)
sort(unique(roads2016$Photo.Date))
roads2016 <- subset(roads2016, Photo.Date <= "2016-06-30")


# check the time lag between start date and 1st photo, last photo and end date
# if lag is too large there is still something wrong in dates, fix by redefining start and end dates 
time.lag(roads2016) # check

# wrong starts, fix using day of last photo
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-21",]$Camera.Start.Date <- min(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-21")$Photo.Date)-1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-24",]$Camera.Start.Date <- min(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-24")$Photo.Date)-1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-34",]$Camera.Start.Date <- min(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-34")$Photo.Date)-1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-35",]$Camera.Start.Date <- min(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-35")$Photo.Date)-1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-36",]$Camera.Start.Date <- min(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-36")$Photo.Date)-1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-37",]$Camera.Start.Date <- min(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-37")$Photo.Date)-1
# wrong ends, fix using day of last photo
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-12",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-12")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-13",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-13")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-15",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-15")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-20",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-20")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-21",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-21")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-24",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-24")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-34",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-34")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-35",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-35")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-36",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-36")$Photo.Date)+1
roads2016[roads2016$Camera.Trap.Name=="CT-RBG-E-37",]$Camera.End.Date <- max(subset(roads2016, Camera.Trap.Name=="CT-RBG-E-37")$Photo.Date)+1
time.lag(roads2016) # check


##----- 2017 -----

roads2017 <- subset(roads, Sampling.Event == 2017)
roads2017$Camera.Trap.Name <- as.factor(roads2017$Camera.Trap.Name)
levels(roads2017$Camera.Trap.Name) # camera-trap names differ from all other datasets. Find out why and fix


# check date errors
min(roads2017$Camera.Start.Date); max(roads2017$Camera.End.Date)
min(roads2017$Photo.Date); max(roads2017$Photo.Date)
sort(unique(roads2017$Photo.Date))

# check the time lag between start date and 1st photo, last photo and end date
# if lag is too large there is still something wrong in dates, fix by redefining start and end dates 
time.lag(roads2017) # check



##----- 3 - join all years in a single dataframe-----

# check dimensions and colnames
dim(roads2015); dim(roads2016); dim(roads2017)
names(roads2015); names(roads2016); names(roads2017)


roads <- rbind(roads2015, roads2016, roads2017)
roads$Project.Name <- "Gurupi"
roads$Organization.Name <- "ICMBio/CENAP"
roads$Camera.Trap.Name <- factor(roads$Camera.Trap.Name)
levels(roads$Camera.Trap.Name)
roads <- roads[,c(1:3,5:33,4)] # reorder columns (but cam.ID in the last position)

# fix coordinates
coords <- distinct(roads, Camera.Trap.Name, Latitude, Longitude)
coords <- coords[order(coords$Camera.Trap.Name),]

# CT-RBG-E-39 lacks coordinates!!! Fix it or remove the camera
roads <- subset(roads, Camera.Trap.Name != "CT-RBG-E-39")

# there are duplicated camera traps with same name but differing coordinates
# based on quick check I assume that differences for same camera are sligth and I can select anyone of them
# so I will use the distinct function
coords <- distinct(coords, Camera.Trap.Name, .keep_all= TRUE)
coords$Camera.Trap.Name <- factor(coords$Camera.Trap.Name)

# five decimal places are equivalent to approximately 1.1 m precision
# so we can round
coords$Latitude <- round(as.numeric(coords$Latitude), 6)
coords$Longitude <- round(as.numeric(coords$Longitude), 6)


# unify coordinates for all sites-years in the dataset
df1 <- coords
df2 <- roads
lat.long <- c("Latitude", "Longitude")
df2[lat.long] <- lapply(lat.long, function(x) df1[[x]][match(df2$Camera.Trap.Name, df1$Camera.Trap.Name)])

distinct(df2, Camera.Trap.Name, Latitude, Longitude)

# check if lat long are OK
check.coord(df2)


# fix species names (raw data version)
fix.raw.data.species.names <- function(data)  {
  data$bin <- factor(data$bin)
  #data<-data[!(data$bin==" "),]
  levels(data$bin)[levels(data$bin)=="Aramides cajanea"] <- "Aramides cajaneus" # rename by name
  levels(data$bin)[levels(data$bin)=="Caprimulgidae sp"] <- "Caprimulgus unknown"
  levels(data$bin)[levels(data$bin)=="Chelonoidis sp"] <- "Chelonoidis unknown"
  levels(data$bin)[levels(data$bin)=="Crypturellus sp"] <- "Crypturellus unknown"
  levels(data$bin)[levels(data$bin)=="Dasypus novencinctus"] <- "Dasypus novemcinctus"
  levels(data$bin)[levels(data$bin)=="Mazama gouazoubira"] <- "Mazama nemorivaga"
  levels(data$bin)[levels(data$bin)=="Mazama sp"] <- "Mazama unknown"
  levels(data$bin)[levels(data$bin)=="Odontophorus sp"] <- "Odontophorus gujanensis"
  levels(data$bin)[levels(data$bin)=="Penelope sp"] <- "Penelope unknown"
  levels(data$bin)[levels(data$bin)=="staff"] <- "Homo sapiens"
  levels(data$bin)[levels(data$bin)=="Tinamus sp"] <- "Tinamus unknown"
  levels(data$bin)[levels(data$bin)=="Tupinambis sp"] <- "Tupinambis unknown"
  #data<-data[!(data$bin==" "),] # remove empty " " rows
  data$bin <- factor(data$bin)
  assign("dataTemp", data, envir=.GlobalEnv)
}

fix.raw.data.species.names(df2)
df3 <- dataTemp

# fix Genus and Species (previous function changed only the "bin" column)
df3$Genus <- gsub( " .*$", "", df3$bin) # extract Genus from bin
df3$Species <- sub("^\\S+\\s+", '', df3$bin) # extract Species from bin

# small changes so we can use f.readin.fixing.data
#df3$bin <- NULL
#df3$Start.Date <- NULL
#df3$End.Date <- NULL
#df3$td.photo <- NULL
#colnames(df3)[9] <- "Photo.time" # probably will have to reverse it after using f.readin.fixing.data
#df3$Photo.Time <- df3$Photo.time

# save as csv for use in Part-1
write.csv(df3, here("data", "Wild_ID_RBG_ROADS_2015to2017_processed.csv"), row.names=FALSE)

