# this explores DBSCAN based clustering for the austin pothole dataset

# ---- Setup and Cleaning ----
# setwd 
setwd("/Users/heidihurst/Documents/UK/ucl-gis/t2/stdm/coursework/")

# libraries/imports
library(dbscan)
library(lubridate)
library(scales)
library(leaflet)

# read in file form austin subset
austin <- read.csv("Pothole Austin 2017.csv")

# convert date range to numeric?
# parse dates
austin$Status.Change.Date.cleaned <- mdy_hms(as.character(austin$Status.Change.Date))
austin$Created.Date.cleaned <- mdy_hms(as.character(austin$Created.Date))
austin$Last.Update.Date.cleaned <- mdy_hms(as.character(austin$Last.Update.Date))
austin$Close.Date.cleaned <- mdy_hms(as.character(austin$Close.Date))

# duration = close date - create date
austin$duration <- ymd_hms(austin$Close.Date.cleaned) - ymd_hms(austin$Created.Date.cleaned)
# if no duration
# count no duration
sum(length(which(is.na(austin$duration))))
sum(length(austin$duration))
sum(length(which(is.na(austin$Created.Date))))
sum(length(which(is.na(austin$Close.Date))))
sum(length(which(is.na(austin$Created.Date.cleaned))))
sum(length(which(is.na(austin$Close.Date.cleaned))))

# find why there's no duration for those - did they just parse weird? look at examples
austin_notopen = austin[which(is.na(austin$Created.Date.cleaned)),]
austin_open = austin[-which(is.na(austin$Created.Date.cleaned)),]

# weird parsing seems to be because (god knows why) some are ymdhms and some are ymdhm
# update only for the ones with na - this seems to work for almost all (except 5) - those don't have start
austin$Created.Date.cleaned <- ifelse(is.na(mdy_hms(as.character(austin$Created.Date))),
                                      mdy_hm(as.character(austin$Created.Date)),
                                      mdy_hms(as.character(austin$Created.Date)))
                                      
# similar processing for close date - 190 not processed because they were transfered, open, duplicate, in progress, etc...
austin$Close.Date.cleaned <- ifelse(is.na(mdy_hms(as.character(austin$Close.Date))),
                                      mdy_hm(as.character(austin$Close.Date)),
                                      mdy_hms(as.character(austin$Close.Date)))
                                            
# create duration - has 190 na's
austin$duration <- austin$Close.Date.cleaned - austin$Created.Date.cleaned
# remove nas
austin_duration <- austin[-which(is.na(austin$duration)),]
# look at some basic stats
min(austin_duration$duration)
max(austin_duration$duration)
mean(austin_duration$duration)
median(austin_duration$duration)
# what's up with the zeros? - there's only one, so idt we need to worry about it
austin_duration[austin_duration$duration == 0,]

# lets convert other variables to what we need
# method received - how many types?
unique(austin_duration$Method.Received) # 4 types - open311, phone, spot311 interface, web
# convert factor to integer
austin_duration$Method.received <- as.numeric(austin_duration$Method.Received)

# scale rows we're interested in to 0,1 to analyze
austin_duration$Method.received <- rescale(austin_duration$Method.received)
austin_duration$duration <- rescale(austin_duration$duration)
austin_duration$State.Plane.X.Coordinate <- rescale(austin_duration$State.Plane.X.Coordinate)
austin_duration$State.Plane.Y.Coordinate <- rescale(austin_duration$State.Plane.Y.Coordinate)
austin_duration$Created.Date.cleaned <- rescale(austin_duration$Created.Date.cleaned)

# ---- DBSCAN ----
# introduce into DBSCAN
austin_scan <- data.matrix(subset(austin_duration, select = c("Method.received", "duration", 
                                                  "State.Plane.X.Coordinate",
                                                  "State.Plane.Y.Coordinate", 
                                                  "Created.Date.cleaned")))
# convert all components to numeric (from factors)
austin_db <- dbscan(austin_scan, eps=0.25, minPts = 10)
# add cluster back into original data
austin_duration$db <- austin_db$cluster
# get number of clusters
db_nclust <- length(unique(austin_duration$db))

# plot output
factpal <- colorFactor(topo.colors(db_nclust), 
                       austin_duration$db)

m <- leaflet(austin_duration) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude.Coordinate, ~Latitude.Coordinate, color=~factpal(db), stroke=TRUE,fillOpacity = 0.8,
                   popup = paste("Cluster: ", austin_duration$db,"<br>",
                                 "Status: ", austin_duration$SR.Status,"<br>",
                                 "Duration: ", austin_duration$duration,"<br>",
                                 "Case Title: ", austin_duration$CASE_TITLE))
m


par(mfrow=c(ceiling(db_nclust/3),3))
for (i in(unique(austin_duration$db))){
  # plot state plane coordinates
  plot(austin_duration[austin_duration$db == i,]$State.Plane.X.Coordinate, 
       austin_duration[austin_duration$db == i,]$State.Plane.Y.Coordinate,
       xlab = "x", ylab = 'y', main = paste("Cluster",i, sep = ' '),
       xlim = c(min(austin_duration$State.Plane.X.Coordinate), 
                max(austin_duration$State.Plane.X.Coordinate)),
       ylim = c(min(austin_duration$State.Plane.Y.Coordinate), 
                max(austin_duration$State.Plane.Y.Coordinate)))
  
  # add title
}

# questions to consider:
# what columns should be included? 
# how many is the min points?
# what hsould eps be set to?
# what does each cluster mean?

# run for a variety of options and then plot, visualize

# ---- K Means ----
# set maximum number of clusters
k.max <- 50 # started with 10 - no discernable kink before 50 - maybe somewhere around 10?
wss <- sapply(1:k.max, function(k){kmeans(austin_scan, k,
                                          nstart=50,iter.max = 15 )$tot.withinss})
plot(wss, type="l", xlab="k", ylab="WSS", main="Elbow Plot: K Means, Austin, 5 Var") 
# elbow plot doesn't seem to show a clear kink, but rather an exponential decay

# try 10 clusters and plot
km_nclust <- 10
austin_km <- kmeans(austin_scan, km_nclust, nstart=50, iter.max = 15)
austin_duration$km <- austin_km$cluster



# plottttt
factpal <- colorFactor(topo.colors(length(unique(austin_duration$km))), 
                       austin_duration$km)

m <- leaflet(austin_duration) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude.Coordinate, ~Latitude.Coordinate, color=~factpal(km), stroke=TRUE,fillOpacity = 0.8,
                   popup = paste("Cluster: ", austin_duration$db,"<br>",
                                 "Status: ", austin_duration$SR.Status,"<br>",
                                 "Duration: ", austin_duration$duration,"<br>",
                                 "Case Title: ", austin_duration$CASE_TITLE))
m

# try subplots for each cluster
par(mfrow=c(ceiling(km_nclust/3),3))
# for loop to create each one
for (i in(unique(austin_duration$km))){
  # plot state plane coordinates
  plot(austin_duration[austin_duration$km == i,]$State.Plane.X.Coordinate, 
       austin_duration[austin_duration$km == i,]$State.Plane.Y.Coordinate,
       # add labels
       xlab = "x", ylab = 'y', main = paste("Cluster",i, sep = ' '),
       # add cluster statistics
       sub = paste("Method: ",round(austin_km$centers[i,"Method.received"],3),
                   "Duration: ",round(austin_km$centers[i,"duration"],3),
                   "\nCreated: ",round(austin_km$centers[i,"Created.Date.cleaned"],3),
                   "X: ",round(austin_km$centers[i,"State.Plane.X.Coordinate"],3),
                   "Y: ",round(austin_km$centers[i,"State.Plane.Y.Coordinate"],3)),
       # ensure they're all scaled appropriate
       xlim = c(min(austin_duration$State.Plane.X.Coordinate), 
                max(austin_duration$State.Plane.X.Coordinate)),
       ylim = c(min(austin_duration$State.Plane.Y.Coordinate), 
                max(austin_duration$State.Plane.Y.Coordinate)))
}

# get statistics for each cluster
# how do clusters help us predict?

# ---- Exploratory Data Analysis ----

# line plots for open date by time - need to re-bin
# re-do with meaningful axis
par(mfrow=c(1,1))
hist(austin$Created.Date.cleaned)
hist(austin$duration)

