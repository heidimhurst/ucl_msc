# this explores DBSCAN based clustering for the austin pothole dataset

# ---- Setup and Cleaning ----
# setwd 
setwd("/Users/heidihurst/Documents/UK/ucl-gis/t2/stdm/coursework/")

# libraries/imports
library(dbscan) # for running DBSCAN analysis
library(lubridate) # for date parsing
library(scales) # for scaling dataset
library(leaflet) # for visualizing interactive maps
library(tictoc) # for benchmarking

# ---- ALL AUSTIN DATA SETUP ----
# the updated download seems to be working just fine - will work from that moving forward
aus_all <- read.csv("Austin_311_Public_Data.csv")
aus_all$Created.Date.cleaned <- mdy_hms(aus_all$Created.Date)
aus_all$Close.Date.cleaned <- mdy_hms(aus_all$Close.Date)
aus_all$duration <- aus_all$Close.Date.cleaned - aus_all$Created.Date.cleaned

# convert methods to numbers
aus_all$Method.numeric <- sapply(as.character(aus_all$Method.Received), switch, 
                                 "Spot311 Interface" = 1, "Phone" = 2, "Web" = 3, 4,
                                 USE.NAMES = F)

# normalize fields of interest
aus_all$Method.numeric.rescale <- rescale(aus_all$Method.numeric)
aus_all$duration.rescale <- rescale(as.numeric(aus_all$duration))
aus_all$State.Plane.X.Coordinate.rescale <- rescale(aus_all$State.Plane.X.Coordinate)
aus_all$State.Plane.Y.Coordinate.rescale <- rescale(aus_all$State.Plane.Y.Coordinate)
aus_all$Created.Date.cleaned.rescale <- rescale(aus_all$Created.Date.cleaned)

# export
write.csv(aus_all, "Austin_2017_ds.csv")

hist(week(aus_all$Created.Date.cleaned), breaks = 52)

# select just potholes
aus_pot <- aus_all[which(apply(aus_all, 1, 
                               function(r) any(grepl('pothole', r, ignore.case = TRUE)))),]

# normalize fields of interest
aus_pot$Method.numeric.rescale.pot <- rescale(aus_pot$Method.numeric)
aus_pot$duration.rescale.pot <- rescale(as.numeric(aus_pot$duration))
aus_pot$State.Plane.X.Coordinate.rescale.pot <- rescale(aus_pot$State.Plane.X.Coordinate)
aus_pot$State.Plane.Y.Coordinate.rescale.pot <- rescale(aus_pot$State.Plane.Y.Coordinate)
aus_pot$Created.Date.cleaned.rescale.pot <- rescale(aus_pot$Created.Date.cleaned)

# export
write.csv(aus_pot, "Austin_pot_2017_ds.csv")

# select duplicates
aus_pot_dup <- aus_all[which(apply(aus_pot, 1, 
                               function(r) any(grepl('duplicate', r, ignore.case = TRUE)))),]

# ---- Austin Data Import ----
# re-import(future use)
aus_all <- read.csv("Austin_2017_ds.csv")
aus_pot <- read.csv("Austin_pot_2017_ds.csv")

# ---- Basic Dataviz ----
all_color <- 'darkblue'
pot_color <- 'magenta'

# PLOT THIS - bar chart - methods
# sort and ensure order is the same for both
aus_methods <- sort(table(aus_all$Method.Received), decreasing = TRUE)
methods_order <- rownames(aus_methods)
aus_pot_methods <- data.frame(table(aus_pot$Method.Received))
aus_pot_methods <- aus_pot_methods[match(methods_order, aus_pot_methods$Var1),]
# ensure we have enough space for labels by changing margins
par(mar=c(9, 4.1, 4.1, 2.1))
# force it to not be in scientific notation
options(scipen = 999)
# plot bars overlaid - change limits to fit labels etc
xx <- barplot(aus_methods/nrow(aus_all), col = all_color, las = 2, 
              ylim = c(-1, 1), yaxt = "n")
barplot(-aus_pot_methods$Freq/nrow(aus_pot), 
        col= pot_color, add = T, las = 2, ylim = c(-0.1, 1), yaxt = "n")
# add numbers to top
text(x = xx, y = data.frame(aus_methods)$Freq/nrow(aus_all), 
     label = data.frame(aus_methods)$Freq, 
     pos = 3, cex = 0.8, col = all_color)
text(x = xx, y = -aus_pot_methods$Freq/nrow(aus_pot), label = aus_pot_methods$Freq, 
     pos = 1, cex = 0.8, col = pot_color)
# add custom axis labels
axis(2, at = c(-1,-0.5,0,0.5,1), labels = c('100%', '50%', '0%', '50%', '100%'))
# add legend, title information
legend("topright",
       legend = c("All Other Complaints", "Pothole Complaints"),
       fill = c(all_color, pot_color),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Austin 2017 - Submission Method")



# setup margins, etc
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
# plot as bar chart for consistency
aus_create_wk <- data.frame(table(week(aus_all$Created.Date.cleaned)))
aus_pot_create_wk <- data.frame(table(week(aus_pot$Created.Date.cleaned)))

xx <- barplot(aus_create_wk$Freq/nrow(aus_all), 
              col = all_color, las = 2, ylim = c(-.05, .05), yaxt = "n")
barplot(-aus_pot_create_wk$Freq/nrow(aus_pot), 
        col= pot_color, add = T, las = 2, ylim = c(-.05, .05),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.04,-0.02,0,0.02,0.04), labels = c('4%', '2%', '0%', '2%', '4%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("All Other Complaints", "Pothole Complaints"),
       fill = c(all_color, pot_color),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Austin 2017 - Open Date")


# Closed by week
# setup margins, etc
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
# plot as bar chart for consistency
aus_close_wk <- data.frame(table(week(aus_all$Close.Date.cleaned)))
aus_pot_close_wk <- data.frame(table(week(aus_pot$Close.Date.cleaned)))

xx <- barplot(aus_close_wk$Freq/nrow(aus_all), 
              col = all_color, las = 2, ylim = c(-.05, .05), yaxt = "n")
barplot(-aus_pot_close_wk$Freq/nrow(aus_pot), 
        col= pot_color, add = T, las = 2, ylim = c(-.05, .05),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.04,-0.02,0,0.02,0.04), labels = c('4%', '2%', '0%', '2%', '4%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("All Other Complaints", "Pothole Complaints"),
       fill = c(all_color, pot_color),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Austin 2017 - Close Date")

# OPEN/CLOSE PLOTTING
# ALL
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
xx <- barplot(aus_create_wk$Freq/nrow(aus_all), 
              col = all_color, las = 2, ylim = c(-.05, .05), yaxt = "n")
barplot(-aus_close_wk$Freq/nrow(aus_all), 
        col= adjustcolor(all_color, 0.6), add = T, las = 2, ylim = c(-.05, .05),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.04,-0.02,0,0.02,0.04), labels = c('4%', '2%', '0%', '2%', '4%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("Open Date", "Close Date"),
       fill = c(all_color, adjustcolor(all_color, 0.6)),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Austin 2017 - All Complaints")

# POTHOLES
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
xx <- barplot(aus_pot_create_wk$Freq/nrow(aus_pot), 
              col = pot_color, las = 2, ylim = c(-.05, .05), yaxt = "n")
barplot(-aus_pot_close_wk$Freq/nrow(aus_pot), 
        col= adjustcolor(pot_color, 0.4), add = T, las = 2, ylim = c(-.05, .05),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.04,-0.02,0,0.02,0.04), labels = c('4%', '2%', '0%', '2%', '4%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("Open Date", "Close Date"),
       fill = c(pot_color, adjustcolor(pot_color, 0.6)),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Austin 2017 - Pothole Complaints")


# # plot hist of duration - use log to be able to see better
# hist(log(as.numeric(aus_all$duration)), col=all_color,
#      main = "Austin 2017 Complaint Duration", xlab = "log of duration")
# hist(log(as.numeric(aus_pot$duration)), col=pot_color, add = T)
# 
# # bin durations
# aus_all$Days.Taken <- as.numeric(aus_all$duration/86400)
# aus_all$Custom.Taken <- cut(aus_all$Days.Taken, 
#                                  breaks = 
#                                    c(-1, 1, 7, 14, 30,  91, 183, 365, 448), 
#                                  labels = c("1 Day", "1 Week",	"Fortnight",	"1 Month",	"3 Months",	"6 Months",	"1 Year",	"More than a Year")
# )
# # create table and plot that mofo
# aus_duration <- table(aus_all$Custom.Taken)



# ---- Clustering Setup ----
# don't need to do this - just need to used the normalized columns
# convert factor to integer
aus_all$Method.numeric <- sapply(as.character(aus_all$Method.Received), switch, 
                                 "Spot311 Interface" = 1, "Phone" = 2, "Web" = 3, "Open311" = 4, 
                                 "E-Mail" = 5, "Field Request" = 6, "CSR - Follow On SR" = 7, "Other" = 8,
                                 USE.NAMES = F)

# scale rows we're interested in to 0,1 to analyze
aus_all$Method.numeric.rescale <- rescale(aus_all$Method.numeric)
aus_all$duration.rescale <- rescale(as.numeric(aus_all$duration))
aus_all$State.Plane.X.Coordinate.rescale <- rescale(aus_all$State.Plane.X.Coordinate)
aus_all$State.Plane.Y.Coordinate.rescale <- rescale(aus_all$State.Plane.Y.Coordinate)
aus_all$Created.Date.cleaned.rescale <- rescale(as.numeric(aus_all$Created.Date.cleaned))

# ---- DBSCAN ---- 
# TODO - how to tune parameters for this? 
# https://github.com/alitouka/spark_dbscan/wiki/Choosing-parameters-of-DBSCAN-algorithm
# introduce into DBSCAN
aus_all_scan <- data.matrix(subset(aus_all, select = c("Method.numeric.rescale", 
                                                       "duration.rescale", 
                                                       "State.Plane.X.Coordinate.rescale",
                                                       "State.Plane.Y.Coordinate.rescale", 
                                                       "Created.Date.cleaned.rescale")))
aus_all_s <- aus_all_scan[which(complete.cases(aus_all_scan)),]

# this doesn't work with NAs, so find which rows have NA and remove
aus_all_nona <- which(complete.cases(aus_all_scan)) # keep a record of which are which
# aus_all_scan <- data.matrix(as.numeric(complete.cases(aus_all_scan)))
# convert all components to numeric (from factors)
tic("dbscan")
aus_all_db <- dbscan(aus_all_s, eps=0.5, minPts = 10)
toc()

aus_all[aus_all_nona, "db"] <- aus_all_db$cluster
aus_all2 <- aus_all[aus_all_nona,]

# visualize
db_nclust <- length(unique(aus_all2$db))
factpal <- colorFactor(topo.colors(db_nclust),aus_all2$db)
m <- leaflet(aus_all2) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude.Coordinate, ~Latitude.Coordinate, color=~factpal(db), stroke=TRUE,fillOpacity = 0.8,
                   popup = paste("Cluster: ", aus_all2$db,"<br>",
                                 "Status: ", aus_all2$SR.Status,"<br>",
                                 "Duration: ", aus_all2$duration))
m

par(mfrow=c(ceiling(db_nclust/3),3))
for (i in(unique(austin_all_duration$db))){
  # plot state plane coordinates
  plot(austin_all_duration[austin_all_duration$db == i,]$State.Plane.X.Coordinate, 
       austin_all_duration[austin_all_duration$db == i,]$State.Plane.Y.Coordinate,
       main = paste("Cluster",i, "-", 
                    table(austin_all_duration$db)[toString(i)],
                    "points", sep = ' '),
       xlab = "x", ylab = 'y',
       xlim = c(min(austin_all_duration$State.Plane.X.Coordinate), 
                max(austin_all_duration$State.Plane.X.Coordinate)),
       ylim = c(min(austin_all_duration$State.Plane.Y.Coordinate), 
                max(austin_all_duration$State.Plane.Y.Coordinate)))
  
  # add title
}



# test for a bunch of things to see how many clusters there are based on the params
eps <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 5) # eps steps to test
minPts <- c(10,100,1000) # minimum points to test
nclust <- c() # create matrix for number of clusters
aus_all_db <- matrix(, nrow = length(aus_all_s), ncol = length(eps)*minPts) # to store dbs, so we don't have to re-reun
tic.clearlog()
for (j in minPts){
  for (i in 1:length(eps)){
    # clear log, so we can get time counts
    tic.clearlog()
    db <- dbscan(aus_all_s, eps=0.5, minPts = j)
    nclust[i] <- length(unique(db$clusters))
    aus_all_db[,i] <- db$cluster
    toc(log = TRUE, quiet = FALSE)
  }
}
# get timer outputs
log.txt <- tic.log(format = TRUE)
unlist(log.txt)


# add cluster back into original data
aus_all[aus_all_nona, "db"] <- aus_all_db

# plot output
factpal <- colorFactor(topo.colors(db_nclust), 
                       aus_all_duration$db)

m <- leaflet(austin_all_duration) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude.Coordinate, ~Latitude.Coordinate, color=~factpal(db), stroke=TRUE,fillOpacity = 0.8,
                   popup = paste("Cluster: ", austin_all_duration$db,"<br>",
                                 "Status: ", austin_all_duration$SR.Status,"<br>",
                                 "Duration: ", austin_all_duration$duration,"<br>",
                                 "Case Title: ", austin_all_duration$CASE_TITLE))
m

par(mfrow=c(ceiling(db_nclust/3),3))
for (i in(unique(austin_all_duration$db))){
  # plot state plane coordinates
  plot(austin_all_duration[austin_all_duration$db == i,]$State.Plane.X.Coordinate, 
       austin_all_duration[austin_all_duration$db == i,]$State.Plane.Y.Coordinate,
       main = paste("Cluster",i, "-", 
                    table(austin_all_duration$db)[toString(i)],
                    "points", sep = ' '),
       xlab = "x", ylab = 'y',
       xlim = c(min(austin_all_duration$State.Plane.X.Coordinate), 
                max(austin_all_duration$State.Plane.X.Coordinate)),
       ylim = c(min(austin_all_duration$State.Plane.Y.Coordinate), 
                max(austin_all_duration$State.Plane.Y.Coordinate)))
  
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
tic.clearlog()
wss <- sapply(1:k.max, function(k){ tic("kmeans")
                                    kmeans(aus_all_s, centers = k,
                                          nstart= 50,iter.max = 15)$tot.withinss
                                    toc(log=TRUE, quiet=FALSE)})
plot(wss, type="l", xlab="k", ylab="WSS", main="Elbow Plot: K Means, austin_all, 5 Var") 
# elbow plot doesn't seem to show a clear kink, but rather an exponential decay

# try 10 clusters and plot
km_nclust <- 8
austin_all_km <- kmeans(austin_all_scan, km_nclust, nstart=50, iter.max = 15)
austin_all_duration$km <- austin_all_km$cluster



# plottttt
factpal <- colorFactor(topo.colors(length(unique(austin_all_duration$km))), 
                       austin_all_duration$km)

m <- leaflet(austin_all_duration) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude.Coordinate, ~Latitude.Coordinate, color=~factpal(km), stroke=TRUE,fillOpacity = 0.8,
                   popup = paste("Cluster: ", austin_all_duration$db,"<br>",
                                 "Status: ", austin_all_duration$SR.Status,"<br>",
                                 "Duration: ", austin_all_duration$duration,"<br>",
                                 "Case Title: ", austin_all_duration$CASE_TITLE))
m

# try subplots for each cluster
par(mfrow=c(ceiling(km_nclust/3),3))
# for loop to create each one
for (i in(unique(austin_duration$km))){
  # plot state plane coordinates
  plot(austin_duration[austin_duration$km == i,]$State.Plane.X.Coordinate, 
       austin_duration[austin_duration$km == i,]$State.Plane.Y.Coordinate,
       # add labels
       xlab = "x", ylab = 'y', 
       main = paste("Cluster",i, "-", 
                    table(austin_duration$km)[i],
                    "points", sep = ' '),
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

# ---- BOSTON DATA SETUP----
## read in all
# bos <- read.csv("Boston_311.csv")
# # convert date
# bos$open_dt_cleaned <- ymd_hms(bos$open_dt)
# boston <- bos[year(bos$open_dt_cleaned) == 2017,]
# boston$closed_dt_cleaned <- ymd_hms(boston$closed_dt)
# boston$duration <- boston$closed_dt_cleaned - boston$open_dt_cleaned
# 
# # save this off so I don't have to do it again
# write.csv(boston, "Boston_2017.csv")

# read it back in 
boston <- read.csv("Boston_2017.csv")

# create source numeric
boston$Source.numeric <- sapply(as.character(boston$Source), switch, 
                                "Citizens Connect App" = 1, "Constituent Call" = 2, 
                                "City Worker App" = 3, 4,
                                USE.NAMES = F)

# normalize fields of interest
boston$Source.numeric.rescale <- rescale(boston$Source.numeric)
boston$open_dt_cleaned.rescale <- rescale(as.numeric(boston$open_dt_cleaned))
boston$Latitude.rescale <- rescale(boston$Latitude)
boston$Longitude.rescale <- rescale(boston$Longitude)
boston$duration.rescale <- rescale(boston$duration)

# export
write.csv(boston, "Boston_2017_ds.csv")

# find potholes
bos_pot <- boston[which(apply(boston, 1, 
                               function(r) any(grepl('pothole', r, ignore.case = TRUE)))),]
# identify all duplicates
bos_pot_nodup <- boston[which(apply(bos_pot, 1, 
                              function(r) any(grepl('Duplicate', r, ignore.case = TRUE)))),]

# normalize fields of interest
bos_pot$Source.numeric.rescale.pot <- rescale(bos_pot$Source.numeric)
bos_pot$open_dt_cleaned.rescale.pot <- rescale(as.numeric(bos_pot$open_dt_cleaned))
bos_pot$Latitude.rescale.pot <- rescale(bos_pot$Latitude)
bos_pot$Longitude.rescale.pot <- rescale(bos_pot$Longitude)
bos_pot$duration.rescale.pot <- rescale(bos_pot$duration)

# export
write.csv(bos_pot, "Boston_pot_2017_ds.csv")

# ---- boston visualizations ----
all_color <- 'darkblue'
pot_color <- 'magenta'

# PLOT THIS - bar chart - methods
# sort and ensure order is the same for both
bos_methods <- sort(table(boston$Source), decreasing = TRUE)
methods_order <- rownames(bos_methods)
bos_pot_methods <- data.frame(table(bos_pot$Source))
bos_pot_methods <- bos_pot_methods[match(methods_order, bos_pot_methods$Var1),]
# ensure we have enough space for labels by changing margins
par(mar=c(10, 4.1, 4.1, 2.1))
# force it to not be in scientific notation
options(scipen = 999)
# plot bars overlaid - change limits to fit labels etc
xx <- barplot(bos_methods/nrow(boston), col = all_color, las = 2, 
              ylim = c(-0.5, 0.5), yaxt = "n")
barplot(-bos_pot_methods$Freq/nrow(bos_pot), 
        col= pot_color, add = T, las = 2, yaxt = "n")
# add numbers to top
text(x = xx, y = data.frame(bos_methods)$Freq/nrow(boston), 
     label = data.frame(bos_methods)$Freq, 
     pos = 3, cex = 0.8, col = all_color)
text(x = xx, y = -bos_pot_methods$Freq/nrow(bos_pot), label = bos_pot_methods$Freq, 
     pos = 1, cex = 0.8, col = pot_color)
# add custom axis labels
axis(2, at = c(-0.5,-0.25, 0,0.25,0.5), labels = c('50%', '25%', '0%', '25%', '50%'))
# add legend, title information
legend("topright",
       legend = c("All Other Complaints", "Pothole Complaints"),
       fill = c(all_color, pot_color),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Boston 2017 - Submission Method")



# setup margins, etc
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
# plot as bar chart for consistency
bos_create_wk <- data.frame(table(week(boston$open_dt_cleaned)))
bos_pot_create_wk <- data.frame(table(week(bos_pot$open_dt_cleaned)))

xx <- barplot(bos_create_wk$Freq/nrow(boston), 
              col = all_color, las = 2, ylim = c(-.1, .1), yaxt = "n")
barplot(-bos_pot_create_wk$Freq/nrow(bos_pot), 
        col= pot_color, add = T, las = 2, ylim = c(-.1, .1),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.1,-0.05,0,0.05,0.1), labels = c('10%', '5%', '0%', '5%', '10%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("All Other Complaints", "Pothole Complaints"),
       fill = c(all_color, pot_color),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Boston 2017 - Open Date")


# Closed by week
# setup margins, etc
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
# plot as bar chart for consistency
bos_close_wk <- data.frame(table(week(boston$closed_dt_cleaned)))
bos_pot_close_wk <- data.frame(table(week(bos_pot$closed_dt_cleaned)))

xx <- barplot(bos_close_wk$Freq/nrow(boston), 
              col = all_color, las = 2, ylim = c(-.1, .05), yaxt = "n")
barplot(-bos_pot_close_wk$Freq/nrow(bos_pot), 
        col= pot_color, add = T, las = 2, ylim = c(-.1, .05),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.1,-0.05,0,0.05), labels = c('10%', '5%', '0%', '5%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("All Other Complaints", "Pothole Complaints"),
       fill = c(all_color, pot_color),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Boston 2017 - Close Date")

# OPEN/CLOSE PLOTTING
# ALL
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
xx <- barplot(bos_create_wk$Freq/nrow(boston), 
              col = all_color, las = 2, ylim = c(-.05, .05), yaxt = "n")
barplot(-bos_close_wk$Freq/nrow(boston), 
        col= adjustcolor(all_color, 0.6), add = T, las = 2, ylim = c(-.05, .05),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.04,-0.02,0,0.02,0.04), labels = c('4%', '2%', '0%', '2%', '4%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("Open Date", "Close Date"),
       fill = c(all_color, adjustcolor(all_color, 0.6)),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Boston 2017 - All Complaints")

# POTHOLES
par(mfrow=c(1,1))
par(mar=c(6, 4.1, 4.1, 2.1))
xx <- barplot(bos_pot_create_wk$Freq/nrow(bos_pot), 
              col = pot_color, las = 2, ylim = c(-.1, .1), yaxt = "n")
barplot(-bos_pot_close_wk$Freq/nrow(bos_pot), 
        col= adjustcolor(pot_color, 0.4), add = T, las = 2, ylim = c(-.1, .1),
        yaxt = "n")
# add month markers
axis(2, at = c(-0.1,-0.05,0,0.05,0.1), labels = c('10%', '5%', '0%', '5%', '10%'))
axis(1, at = 1.2 * (1 + seq(1,49, length.out = 12)), las = 2,
     labels = c('January', 'February', 'March', 'April', 'May', 'June','July', 
                'August','September','October', 'November', 'December'))
# add legend, title information
legend("topright",
       legend = c("Open Date", "Close Date"),
       fill = c(pot_color, adjustcolor(pot_color, 0.6)),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Boston 2017 - Pothole Complaints")

# ---- Duration Plot Comparison ---
# Austin Vs Boston Duration
aus_all$Days.Taken <- as.numeric(aus_all$duration/86400)
aus_all$Custom.Taken <- cut(aus_all$Days.Taken, 
                                  breaks = 
                                    c(-1, 1, 7, 14, 30,  91, 183, 365, 448), 
                                  labels = c("1 Day", "1 Week",	"Fortnight",	"1 Month",	"3 Months",	"6 Months",	"1 Year",	">1 Year")
)
boston$Days.Taken <- as.numeric(boston$duration/86400)
boston$Custom.Taken <- cut(boston$Days.Taken, 
                            breaks = 
                              c(-1, 1, 7, 14, 30,  91, 183, 365, 448), 
                            labels = c("1 Day", "1 Week",	"Fortnight",	"1 Month",	"3 Months",	"6 Months",	"1 Year",	">1 Year")
)
 # create table and plot that mofo
aus_duration <- data.frame(table(aus_all$Custom.Taken))
bos_duration <- data.frame(table(boston$Custom.Taken))
# plot
xx <- barplot(aus_duration$Freq/nrow(aus_all), names.arg = aus_duration$Var1,
              col = all_color, las = 2, ylim = c(-.7, .6), yaxt = "n")
barplot(-bos_duration$Freq/nrow(boston), 
        col= adjustcolor(all_color, 0.6), add = T, las = 2, ylim = c(-.7, .6),
        yaxt = "n")
# fix axis labels
axis(2, at = c(-0.5,-0.25,0,0.25,0.5), labels = c('50%', '25%', '0%', '25%', '50%'))
# add numbers
text(x = xx, y = aus_duration$Freq/nrow(aus_all), 
     label = data.frame(aus_methods)$Freq, 
     pos = 3, cex = 0.8, col = all_color)
text(x = xx, y = -bos_duration$Freq/nrow(boston), label = bos_duration$Freq, 
     pos = 1, cex = 0.8, col = adjustcolor(all_color, 0.6))

# add legend, title information
legend("topright",
       legend = c("Austin", "Boston"),
       fill = c(all_color, adjustcolor(all_color, 0.6)),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("All Complaint Duration")

# POTHOLE complaint duration
aus_pot$Days.Taken <- as.numeric(aus_pot$duration/86400)
aus_pot$Custom.Taken <- cut(aus_pot$Days.Taken, 
                            breaks = 
                              c(-1, 1, 7, 14, 30,  91, 183, 365, 448), 
                            labels = c("1 Day", "1 Week", "Fortnight",  "1 Month",  "3 Months", "6 Months", "1 Year", ">1 Year")
)
bos_pot$Days.Taken <- as.numeric(bos_pot$duration/86400)
bos_pot$Custom.Taken <- cut(bos_pot$Days.Taken, 
                            breaks = 
                              c(-1, 1, 7, 14, 30,  91, 183, 365, 448), 
                            labels = c("1 Day", "1 Week",  "Fortnight",  "1 Month",  "3 Months", "6 Months", "1 Year", ">1 Year")
)
# create table and plot that mofo
aus_pot_duration <- data.frame(table(aus_pot$Custom.Taken))
bos_pot_duration <- data.frame(table(bos_pot$Custom.Taken))
# plot
xx <- barplot(aus_pot_duration$Freq/nrow(aus_pot), names.arg = aus_pot_duration$Var1,
              col = pot_color, las = 2, ylim = c(-.8, .8), yaxt = "n")
barplot(-bos_pot_duration$Freq/nrow(bos_pot), 
        col= adjustcolor(pot_color, 0.4), add = T, las = 2, ylim = c(-.8, .8),
        yaxt = "n")
# fix axis labels
axis(2, at = c(-0.5,0,0.5), 
     labels = c('50%','0%','50%'))
# add numbers
text(x = xx, y = aus_pot_duration$Freq/nrow(aus_pot), 
     label = data.frame(aus_methods)$Freq, 
     pos = 3, cex = 0.8, col = pot_color)
text(x = xx, y = -bos_pot_duration$Freq/nrow(bos_pot), label = bos_pot_duration$Freq, 
     pos = 1, cex = 0.8, col = adjustcolor(pot_color, 0.4))

# add legend, title information
legend("topright",
       legend = c("Austin", "Boston"),
       fill = c(pot_color, adjustcolor(pot_color, 0.4)),
       bty = 'n', pt.cex = 1, cex = 0.75)
title("Pothole Complaint Duration")



# ---- SCRATCH ----
austin_all <- read.csv("Austin- all 311 2017.csv")
# weird parsing seems to be because (god knows why) some are ymdhms and some are ymdhm
# update only for the ones with na - this seems to work for almost all (except 5) - those don't have start
# austin_all$Created.Date.cleaned <- ifelse(is.na(mdy_hms(as.character(austin_all$Created.Date))),
#                                          mdy_hm(as.character(austin_all$Created.Date)),
#                                          mdy_hms(as.character(austin_all$Created.Date)))

# austin_all$Created.Date.char <- as.character(austin_all$Created.Date)
# austin_all$Created.Date.cleaned <- ymd(as.character(austin_all$Created.Date))
# ymd seems to parse some of them
# some seem to be empty - this is na

# take out all empties - 5999
austin_all <- austin_all[-which(austin_all$Created.Date == ""),]
# try and parse nona records as ymd - parses 40000k records, etc
# austin_all_nona$Created.Date.cleaned <- ymd(austin_all_nona$Created.Date)
# why didn't the other ones parse?
# austin_all_nona_notopen <- austin_all_nona[which(is.na(austin_all_nona$Created.Date.cleaned)),"Created.Date"]
# dates are either YMD or mdy_HMS - need to parse accordingly

# problem with date processing - some have time and some dont.  
# to avoid, convert all to DATE, not DATE-TIME
austin_all$Created.Date.cleaned <- ifelse(is.na(mdy_hms(as.character(austin_all$Created.Date))),
                                          ymd(as.character(austin_all$Created.Date)),
                                          as_date(mdy_hms(as.character(austin_all$Created.Date))))

# extract created month for bar plot
austin_all$Created.Month <- month(austin_all$Created.Date.cleaned)
year(austin_all$Created.Date.cleaned)

date_levs <- levels(austin_all$Created.Date)
nplace = 100
hist(austin_all$Created.Date.cleaned, breaks = 12, 
     xlim = c(nplace * floor(min(austin_all$Created.Date.cleaned)/nplace), 
              nplace * ceiling(max(austin_all$Created.Date.cleaned))/nplace))

# can we plot this using gg plot?
library(ggplot2)


# similar processing for close date - 190 not processed because they were transfered, open, duplicate, in progress, etc...
austin_all$Close.Date.cleaned <- ifelse(is.na(mdy_hms(as.character(austin_all$Close.Date))),
                                        as_date(dmy_hm(as.character(austin_all$Close.Date))),
                                        as_date(dmy_hms(as.character(austin_all$Close.Date))))

austin_all$duration <- austin_all$Close.Date.cleaned - austin_all$Created.Date.cleaned
# WTF - why does duration give negative values?! are dates DMY or MDY?!

austin_all[897, c("Created.Date", "Close.Date")] 
austin_all[344, c("Created.Date", "Close.Date")] 
# how many weren't processed?
length(which(is.na(austin_all$Close.Date.cleaned))) # 20012 don't have close dates
length(which((austin_all$Close.Date == ""))) # 20012 are empty cells ("")
# this indicates all have been parsed correctly

# don't need the lines below - used for debugging
# find all the ones NA where close date isn't empty cell to figure out what's up
# austin_na_sleuth <- austin_all_nona[is.na(austin_all_nona$Close.Date.cleaned),] 
# austin_all_notopen = austin[which(is.na(austin_all$Created.Date.cleaned)),]
# austin_all_open = austin[-which(is.na(austin_all$Created.Date.cleaned)),]

# create duration - has 20012 na's
austin_all$duration <- austin_all$Close.Date.cleaned - austin_all$Created.Date.cleaned
# remove nas
austin_all_duration <- austin_all[-which(is.na(austin_all$duration)),]
# look at some basic stats
min(austin_all_duration$duration)
max(austin_all_duration$duration)
mean(austin_all_duration$duration)
median(austin_all_duration$duration)

# lets convert other variables to what we need
# method received - how many types?
unique(austin_all_duration$Method.Received) # 4 types - open311, phone, spot311 interface, web
table(austin_all_duration$Method.Received)

# read in file form austin subset
austin_pothole <- read.csv("Pothole Austin 2017.csv")
austin_all <- read.csv("Austin- all 311 2017.csv")

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

# ---- SCRATCH - DBSCAN ----
db_run <- function(df_311, columns, plot_311 = TRUE, eps = 0.25, minPts = 10, db = "db"){
  # introduce into DBSCAN
  matrix_311 <- data.matrix(subset(df_311, select = columns))
  # convert all components to numeric (from factors)
  db_311 <- dbscan(matrix_311, eps=eps, minPts = minPts)
  # add cluster back into original data
  df_311$db <- db_311$cluster
  
  if (plot_311 == TRUE) {
    # get number of clusters
    db_nclust <- length(unique(df_311$db))
    
    # plot output
    factpal <- colorFactor(topo.colors(db_nclust), 
                           df_311$db)
    
    m <- leaflet(austin_duration) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude.Coordinate, ~Latitude.Coordinate, color=~factpal(db), stroke=TRUE,fillOpacity = 0.8,
                       popup = paste("Cluster: ", df_311$db,"<br>",
                                     "Status: ", df_311$SR.Status,"<br>",
                                     "Duration: ", df_311$duration,"<br>",
                                     "Case Title: ", df_311$CASE_TITLE))
    m
    
    par(mfrow=c(ceiling(db_nclust/3),3))
    for (i in(unique(df_311$db))){
      # plot state plane coordinates
      plot(df_311[df_311$db == i,]$State.Plane.X.Coordinate, 
           df_311[df_311$db == i,]$State.Plane.Y.Coordinate,
           main = paste("Cluster",i, "-", 
                        table(df_311$db)[toString(i)],
                        "points", sep = ' '),
           xlab = "x", ylab = 'y',
           xlim = c(min(df_311$State.Plane.X.Coordinate), 
                    max(df_311$State.Plane.X.Coordinate)),
           ylim = c(min(df_311$State.Plane.Y.Coordinate), 
                    max(df_311$State.Plane.Y.Coordinate)))
      
      # add title
    }
  }
}

# process austin data for use
# convert method received to to number
aus_all$Method.numeric <- sapply(as.character(aus_all$Method.Received), switch, 
       "Spot311 Interface" = 1, "Phone" = 2, "Web" = 3, "Open311" = 4, 
       "E-Mail" = 5, "Field Request" = 6, "CSR - Follow On SR" = 7, "Other" = 8,
       USE.NAMES = F)


# export for everyone's use
write.csv(aus_all, "Austin_2017_ds.csv")
# remove nas
aus_all_nona <- aus_all
db_run(aus_all, columns = c("Method.numeric", "duration", 
                          "State.Plane.X.Coordinate",
                          "State.Plane.Y.Coordinate", 
                          "Created.Date.cleaned"))



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
       main = paste("Cluster",i, "-", 
                    table(austin_duration$db)[toString(i)],
                    "points", sep = ' '),
       xlab = "x", ylab = 'y',
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

# ---- SCRATCH - K Means ----
# set maximum number of clusters
k.max <- 50 # started with 10 - no discernable kink before 50 - maybe somewhere around 10?
wss <- sapply(1:k.max, function(k){kmeans(austin_scan, k,
                                          nstart=50,iter.max = 15 )$tot.withinss})
plot(wss, type="l", xlab="k", ylab="WSS", main="Elbow Plot: K Means, Austin, 5 Var") 
# elbow plot doesn't seem to show a clear kink, but rather an exponential decay

# try 10 clusters and plot
km_nclust <- 8
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
       xlab = "x", ylab = 'y', 
       main = paste("Cluster",i, "-", 
                    table(austin_duration$km)[i],
                    "points", sep = ' '),
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

# ---- SCRATCH - Exploratory Data Analysis ----

# line plots for open date by time - need to re-bin
# re-do with meaningful axis
par(mfrow=c(1,1))
hist(austin$Created.Date.cleaned)
hist(austin$duration)