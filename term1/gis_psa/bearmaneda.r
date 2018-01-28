# exploratory data analysis 

# library imports
library(rgdal) # for importing shapefiles, converting CRS
library(tmap) # for plotting
library(lubridate) # for extracting month
library(ggplot2) # for plotting pretty overlays
library(ggmap) # for better ggplot plotting
library(spgwr) # for geospatially weighted regression
library(dplyr) # for mutating dataset into bins
library(Hmisc) # for cutting bins
library(grid) # for viewport ()

# visualization tool - add alpha value to a colour (from https://magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/)
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# visualization settings - reduce margin size to 0
par(mar=c(0,0,0.5,0))

# set working directory
setwd("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment2-bearman/data")

# add polygons, for visualization & aggregation
borough <- readOGR("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data/boundaries/", "London_Borough_Excluding_MHW")
lsoa <- readOGR("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data/boundaries/", "LSOA_2011_London_gen_MHW")
msoa <- readOGR("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data/boundaries/", "MSOA_2011_London_gen_MHW")
ward <- readOGR("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data/boundaries/", "London_Ward")
  
# read in house price dataset
houses <- read.csv("file:///Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment2-bearman/data/london-house-prices-pp-2016.txt")

# find outlier & plot
outie <- read.csv("file:///Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment2-bearman/data/london-house-prices-pp-2016.txt")
outie$Post <- gsub(" ","",as.character(outie$Postcode))
outie <- merge(outie, pcode, by.x="Post", by.y = "Postcode")
out <- outie[outie$Nothings == min(as.numeric(outie$Nothings)),]
plot(outie$Eastings, outie$Nothings, xlab="Eastings (m)", ylab="Nothings (m)", pch = 20, col = add.alpha("blue", 0.2))
points(out$Eastings, out$Nothings, pch = 19, col = "red", add = T)
legend(505000,135000,c("Data Point","Outlier"), pch = c(20,19), col = c(add.alpha("blue", 0.6),"red"), pt.cex = 0.8, bty="n")
title("Data Set Outlier")
dev.print(pdf, "outlier.pdf")


# exploration
head(houses)
summary(houses)

## Date Information
# parse out month using lubridate for all observations, decimals approximate
houses$Month <- month(houses$Date) + (day(houses$Date)-1)/31
# distance from brexit (months, rounded)
houses$Bdist <- houses$Month - 6.7419355

## Join Postcode Information
# import postcode information
pcode <- read.csv("file:///Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment2-bearman/data/london-postcode-bng-lookup.txt")
# clean postcodes prior to join
houses$Post <- gsub(" ","",as.character(houses$Postcode))
pcode$Postcode <- gsub(" ","",pcode$Postcode)
# join!
houses_sp <- merge(houses, pcode, by.x="Post",by.y="Postcode")
# remove one outlier (clearly outside limits)
outie <- houses_sp[houses_sp$Nothings == min(houses_sp$Nothings),]
houses_sp <- houses_sp[!houses_sp$Nothings == min(houses_sp$Nothings),]
# convert point object to spatial object (from Practical 14)
#setup variables for british national gird
bng <- "+init=epsg:27700" #BNG, British National Grid
#create hosue prices as spatial data
coords <- cbind(Eastings = houses_sp$Eastings, Northings = houses_sp$Nothings)
houses.pts <- SpatialPointsDataFrame(coords,  houses_sp, proj4string = CRS(bng))
# plot to confirm
plot(houses.pts, pch = '.', col = 'pink')
# add borough boundaries
plot(borough, add = TRUE)
title("Completeness of Housing Price Data")

# color points by dist from brexit date
rbPal <- colorRampPalette(c('red','black','blue'))
houses.pts$Bdist.color <- rbPal(10)[as.numeric(cut(houses.pts$Bdist,breaks = 10))]
plot(houses.pts, pch = '.', col = houses.pts$Bdist.color)
plot(borough, add = TRUE)

# histograms of time of year houses are sold
hist(houses.pts$Month)

# extract values in a particular borough
# ensure CRS are the same - if out of order set one to the other
proj4string(houses.pts) = proj4string(borough)

# extract over a given borough
z <- houses.pts[!is.na(over(houses.pts, geometry(borough[1,]))),]
plot(borough[1,])
plot(z, pch = '.', col = z$Bdist.color, add = TRUE)


# subplots for given time periods
par(mfrow = c(2,2))
ylim = c(156000, 201000)
xlim = c(504000, 559000)
# > 2mo before brexit
plot(houses.pts[houses.pts$Bdist < -2,], pch = '.', col = add.alpha("#0055ff", 0.4), xlim = xlim, ylim=ylim)
plot(borough, add = TRUE)

# < 2mo before breixit
plot(houses.pts[houses.pts$Bdist > -2 & houses.pts$Bdist < 0,], pch = '.', col = add.alpha("#76d5e8", 0.4))
plot(borough, add = TRUE)

# < 2 mo after brexit
plot(houses.pts[houses.pts$Bdist < 2 & houses.pts$Bdist > 0,], pch = '.', col = add.alpha("#f7808c", 0.4))
plot(borough, add = TRUE)

# > 2 mo after brexit
plot(houses.pts[houses.pts$Bdist > 2,], pch = '.', col = add.alpha("#f20e25", 0.4))
plot(borough, add = TRUE)


## Add Data to Borough
# set time boundaries as matrix
b <- -4:3 # lower bound
t <- -3:4 # upper bound
n <- c("b4", "b3", "b2", "b1", "p1", "p2", "p3", "p4") # epoch name
epoch <- rbind(b, t, n)

## Aggregate Statistics by Ward
# create new df to add these too, with join code
wdf <- data.frame(ward$GSS_CODE)
labels <- c("GSS_CODE")
for (i in 1:8){
  # get subset of houses for time epoch
  wp2 <- houses.pts[houses.pts$Bdist >= (-5 + i) & houses.pts$Bdist < (-4 + i), ]
  # initialize empty arrays
  npts <- c()
  nmedian <- c()
  nmin <- c()
  nmax <- c()
  nmean <- c()
  # iterate through wards
  for (j in 1:nrow(ward)){
    houses <- wp2[ward[j,],] # get subset of houses for each ward
    npts[j] = nrow(houses)/as.numeric(ward[j,]$HECTARES) * 10 #scale for size - per 100,000m^2
    nmedian[j] <- median(houses$Price)
    nmin[j] <- min(houses$Price)
    nmax[j] <- max(houses$Price)
    nmean[j] <- mean(houses$Price)
  }
  # bind all created columns to data frame
  wdf <- cbind(wdf, npts, nmedian, nmin, nmax, nmean)
  # add column name to labels
  labels <- c(labels, paste0(n[i],"_number"))
  labels <- c(labels, paste0(n[i],"_median"))
  labels <- c(labels, paste0(n[i],"_min"))
  labels <- c(labels, paste0(n[i],"_max"))
  labels <- c(labels, paste0(n[i],"_mean"))
}
# add column names
colnames(wdf) <- labels
# join wdf to wards
ward <- merge(ward, wdf, by.x = "GSS_CODE", by.y = "GSS_CODE")

# plot with tmap
tm_shape(ward) + tm_fill(col = "b1_number")

## Aggregate Statistics by Borough:
bdf <- data.frame(borough$GSS_CODE)
labels <- c("GSS_CODE")
for (i in 1:8){
  # get subset of houses for time epoch
  wp2 <- houses.pts[houses.pts$Bdist >= (-5 + i) & houses.pts$Bdist < (-4 + i), ]
  # initialize empty arrays
  npts <- c()
  nmedian <- c()
  nmin <- c()
  nmax <- c()
  nmean <- c()
  # iterate through boroughs
  for (j in 1:nrow(borough)){
    houses <- wp2[borough[j,],] # get subset of houses for each borough
    npts[j] = nrow(houses)/as.numeric(borough[j,]$HECTARES) * 10 #scale for size - per 100,000m^2
    nmedian[j] <- median(houses$Price)
    nmin[j] <- min(houses$Price)
    nmax[j] <- max(houses$Price)
    nmean[j] <- mean(houses$Price)
  }
  # bind all created columns to data frame
  bdf <- cbind(bdf, npts, nmedian, nmin, nmax, nmean)
  # add column name to labels
  labels <- c(labels, paste0(n[i],"_number"))
  labels <- c(labels, paste0(n[i],"_median"))
  labels <- c(labels, paste0(n[i],"_min"))
  labels <- c(labels, paste0(n[i],"_max"))
  labels <- c(labels, paste0(n[i],"_mean"))
}
# add column names
colnames(bdf) <- labels
# join bdf to boroughs
borough <- merge(borough, bdf, by.x = "GSS_CODE", by.y = "GSS_CODE")
# plot with tmap
tm_shape(borough) + tm_fill(col = "b3_number")

## Linear Regression
# predict post-brexit numbers (3 months later) with pre-brexit 3 months
# number of sales (borough)
lmodel <- lm(borough$p4_number ~ borough$b1_number + borough$b2_number + borough$b3_number)
summary(lmodel)
borough.resid <- resid(lmodel)
borough$nresid <- borough.resid
tmap_mode("plot")
tm_shape(borough) + tm_fill(col = "nresid", palette = "BrBG", colorNA = c.na) + niceties("Pre- vs Post-Brexit Sales\nTotal Number Residual")
dev.print(pdf, "nresid_borough.pdf")

# median prices (borough)
lmodel.med <- lm(borough$p4_median ~ borough$b1_median + borough$b2_median + borough$b3_median)
summary(lmodel.med)
borough.medresid <- resid(lmodel.med)
borough.medresid <- c(borough.medresid[1:24], NA, borough.medresid[25:32])
borough$medresid <- borough.medresid
tmap_mode("plot")
tm_shape(borough) + tm_fill(col = "medresid", palette = "BrBG", colorNA = c.na) + niceties("Pre- vs Post-Brexit Sales\nMedian Value Residual")
dev.print(pdf, "medresid_borough.pdf")

# number of sales (ward) (should we include HECTARES? no, doesn't improve by much)
wlmodel <- lm(ward$p4_number ~ ward$b1_number + ward$b2_number + ward$b3_number)
summary(wlmodel)
ward.resid <- resid(wlmodel)
ward$nresid <- ward.resid
tmap_mode("plot")
tm_shape(ward) + tm_fill(col = "nresid", palette = "BrBG", colorNA = c.na) + niceties("Pre- vs Post-Brexit Sales\nTotal Number Residual")
dev.print(pdf, "nresid_ward.pdf")

# median prices (ward)
wlmodel.med <- lm(ward$p4_median ~ ward$b1_median + ward$b2_median + ward$b3_median)
summary(wlmodel.med)
ward.medresid <- resid(wlmodel.med)
fresid <- c()
j = 1
namedian <- is.na(ward$p4_median)
for (i in 1:nrow(ward)){
  if (!namedian[i]){
    fresid <- c(fresid, ward.medresid[j])
    j <- j + 1
  } else {
    fresid <- c(fresid, NA)
  }
}
ward$medresid <- fresid
tmap_mode("plot")
tm_shape(ward) + tm_fill(col = "medresid", palette = "BrBG", colorNA = c.na) + niceties("Pre- vs Post-Brexit Sales\nMedian Value Residual")
dev.print(pdf, "medresid_ward.pdf")

## GWR
# borough#
GWRbandwidth <- gwr.sel(borough$p4_number ~ borough$b1_number + borough$b2_number + borough$b3_number, data=borough, adapt=T)
gwr.model = gwr(borough$p4_number ~ borough$b1_number + borough$b2_number + borough$b3_number, data=borough, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
gwr.model
# borough median
GWRbandwidthm <- gwr.sel(borough$p4_median ~ borough$b1_median + borough$b2_median + borough$b3_median, data=borough, adapt=T)
gwr.modelm = gwr(borough$p4_median ~ borough$b1_median + borough$b2_median + borough$b3_median, data=borough, adapt=GWRbandwidthm, hatmatrix=TRUE, se.fit=TRUE) 
gwr.modelm

# ward#
GWRbandwidth.w <- gwr.sel(ward$p4_number ~ ward$b1_number + ward$b2_number + ward$b3_number, data=ward, adapt=T)
gwr.model.w = gwr(ward$p4_number ~ ward$b1_number + ward$b2_number + ward$b3_number, data=ward, adapt=GWRbandwidth.w, hatmatrix=TRUE, se.fit=TRUE) 
gwr.model.w
# ward median
GWRbandwidthm.w <- gwr.sel(ward$p4_median ~ ward$b1_median + ward$b2_median + ward$b3_median, data=ward, adapt=T)
gwr.modelm.w = gwr(ward$p4_median ~ ward$b1_median + ward$b2_median + ward$b3_median, data=ward, adapt=GWRbandwidthm.w, hatmatrix=TRUE, se.fit=TRUE) 
gwr.modelm.w

# from the output, we can see that the r^2 value increased only trivially
# not worth doing GWR since prediction value is so high

## Bivariate Choropleth
# inspiration drawn from:
#    http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/ 
#    http://rpubs.com/apsteinmetz/prek
# precursors
bins <- 3
bigtext = 1
smalltext = 0.5
text_sf = 1
c <- c("#e8e8e8", "#ace4e4", "#5ac8c8", "#dfb0d6", "#a5add3", "#5698b9", 
       "#be64ac", "#8c62aa", "#3b4994") # color array, from Josh Stevens
c.na <- "white" # NA color
# function for creating legend color squares
leg <- function(color, df = map_df){
  legend <- tm_shape(df) + 
    tm_layout(bg.color = color) +
    tm_fill(col = "GSS_CODE", alpha = 0, title = "test") + 
    tm_legend(show = FALSE)
  
  return(legend)
}
# auto-generate label 
generate_label <- function(df = map_df){
  for (i in 0:8){
    # set location of square
    x = 0.1 + 0.05*(i %% 3)
    y = 0.1 + 0.05*floor(i/3)
    # create & color square
    vpi = viewport(x=x, y=y, width= .06, height=0.1)
    tmi = leg(c[i+1], df)
    # add square to image
    print(tmi, vp=vpi)
  }
}

# add bin to borough
bdf$p4_nscaled <- bdf$p4_number/borough$HECTARES
bdf$b1_nscaled <- bdf$b1_number/borough$HECTARES
test <- bdf
test <- mutate(test, preBin = cut2(p4_nscaled, g = bins, levels.mean = TRUE))
test <- mutate(test, postBin = cut2(b1_nscaled, g = bins, levels.mean = TRUE))
# create new mapping dataframe
map_df <- test
# bin creation
levels(map_df$preBin) <- bins:1
levels(map_df$postBin) <- bins:1
# create compound bin designators
map_df <- mutate(map_df, bin = paste(preBin, '-', postBin, sep=''))
map_df <- transmute(map_df, GSS_CODE = GSS_CODE, bin = bin)
borough$bin <- map_df$bin

# add bin to ward
wdf$p4_nscaled <- wdf$p4_number/ward$HECTARES
wdf$b1_nscaled <- wdf$b1_number/ward$HECTARES
test <- wdf
test <- mutate(test, preBin = cut2(p4_nscaled, g = bins, levels.mean = TRUE))
test <- mutate(test, postBin = cut2(b1_nscaled, g = bins, levels.mean = TRUE))
# create new mapping dataframe
map_df <- test
# bin creation
levels(map_df$preBin) <- bins:1
levels(map_df$postBin) <- bins:1
# create compound bin designators
map_df <- mutate(map_df, bin = paste(preBin, '-', postBin, sep=''))
map_df <- transmute(map_df, GSS_CODE = GSS_CODE, bin = bin)
ward$bin <- map_df$bin

# plot for either scale
plotBrexit <- function(spatialdf = borough){
  # jank fix to distinguish bw borough and ward cases (color issues)
  if (nrow(spatialdf) < 50){
    b <- c[-7][-3]
  } else {
    b <- c
  }
  brexit <- tm_shape(spatialdf) + 
    tm_fill(col = "bin", palette = b, colorNA = c.na) +
    tm_legend(show = FALSE) +
    tm_credits("Pre-Brexit vs Post-Brexit\nNumber of Sales", position = c("right","top"), 
               just = c("right"), align = c("right"), size = text_sf * bigtext) + 
    tm_credits("pre-brexit", align = c("center"), just = c("center"),
               position = c(0.1, 0.02), size = text_sf * smalltext) +
    tm_credits("p\no\ns\nt", position = c(0.02, 0.09), just = c("center"), size = text_sf * smalltext) +
    tm_credits("low", position = c(0.02, 0.02), just = c("center"), size = text_sf * smalltext, col = "grey") + 
    tm_credits("high", position = c(0.18, 0.02), just = c("center"), size = text_sf * smalltext, col = "grey") +
    tm_credits("high", position = c(0.02, 0.23), just = c("center"), size = text_sf * smalltext, col = "grey") +
    tm_compass(position = c(0.9, 0.07), color.dark = "grey") + 
    tm_scale_bar(width = 0.15, position = c("right","BOTTOM"), color.dark = "grey")
  
  return(brexit)
}

# plot bivariate ward
tmap_mode("plot")
plotBrexit(ward)
generate_label(borough)
dev.print(pdf, "brexit_ward.pdf")

# plot bivariate borough
tmap_mode("plot")
plotBrexit(borough)
generate_label(borough)
dev.print(pdf, "brexit_borough.pdf")

# plot median residuals - borough
tm_medresid <- tm_shape(borough) + 
  tm_fill(col = "medresid", colorNA = c.na) +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_credits("Pre- vs Post-Brexit Sales\nMedian Value Residual", position = c("right","top"), 
             just = c("right"), align = c("right"), size = text_sf * bigtext) + 
  tm_compass(position = c(0.9, 0.07), color.dark = "grey") + 
  tm_scale_bar(width = 0.15, position = c("right","BOTTOM"), color.dark = "grey")
tmap_mode("plot")
tm_medresid
dev.print(pdf, "medresid_borough.pdf")

niceties <- function(name = ""){
  nice <- tm_legend(legend.position = c("left", "bottom")) +
    tm_credits(name, position = c("right","top"), 
               just = c("right"), align = c("right"), size = text_sf * bigtext) + 
    tm_compass(position = c(0.9, 0.07), color.dark = "grey") + 
    tm_scale_bar(width = 0.15, position = c("right","BOTTOM"), color.dark = "grey")
  
  return(nice)
}
