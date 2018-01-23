# Principles of Spatial Analysis Coursework

## Imports
# library imports
library("sp")
library("rgdal")
library("tmap")
library(classInt)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library("ggpubr")
library(Hmisc)
library(dplyr)
library(grid)

## Setup & Data Read-In
# set working directory to folder with data
setwd("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data")

# import data on size perception
size <- read.csv("whataboutsize.csv", stringsAsFactors=FALSE)
satisfaction <- read.csv("whataboutsatisfaction.csv", stringsAsFactors=FALSE)

# import spatial data
area <- readOGR("/boundaries/", "London_Borough_Excluding_MHW")

# join size data to area data
whatabout <- merge(size, satisfaction, by.x = "Authorith_code", by.y = "loc_code")

# import borough data/information
prof <- read.csv("london-borough-profiles2.csv", header=TRUE)

# merge borough data with self image data
profsize <- merge(prof, whatabout, by.x = "Code", by.y = "Authorith_code")

# merge atlas data with existing shapefiles
shapeprof <- merge(area, profsize, by.x = "GSS_CODE", by.y = "Code")

## Exploratory Data Analysis
# function to plot values for exploration only
plotVar <- function(var, title = "", n = 5, colorscheme = "PuBuGn", method = "pretty", add = FALSE){
  breaks <- classIntervals(var, n = n, style = method)
  my_colours <- brewer.pal(n, colorscheme)
  plot(shapeprof, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)], 
       axes = FALSE, border = rgb(0.8,0.8,0.8,0), add = add) 
  legend(x = 499761.2, y = 169345.2, legend = leglabs(breaks$brks), fill = my_colours, 
         bty = "n", cex = 0.7)
  title(title)
}

# exploratory plotting of single variables
plotVar(shapeprof$girl_toofat, "Girl - Too Fat", 7, colorscheme = "Purples")
plotVar(shapeprof$boy_toofat, "Boy - Too Fat", 7, colorscheme = "Blues")
plotVar(shapeprof$boy_toothin, "Boy - Too Thin", 7, colorscheme = "Greens")

## Correllation Analysis
# Setup Data Frame
obcor <- data.frame(as.numeric(profsize$girl_toofat),
                    as.numeric(profsize$boy_toofat),
                    as.numeric(profsize$girl_toothin),
                    as.numeric(profsize$boy_toothin),
                    as.numeric(profsize$Childhood_Obesity_Prevalance_..._2015.16),
                    as.numeric(profsize$X._of_population_from_BAME_groups_.2016.))

colnames(obcor) <- c("girltf", "boytf", "girltt", "boytt", "obesity", "bame")
cor(obcor)

# fat perception & obesity (NEG) (significant)
ggscatter(obcor, x = "girltf", y = "obesity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Fat`", ylab = "Childhood Obesity Rates")

# fat perception (boys) & obesity (not significant)
ggscatter(obcor, x = "boytt", y = "obesity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Boys Self-Describing as `Too Thin`", ylab = "Childhood Obesity Rates")

# fat perception & BAME
ggscatter(obcor, x = "girltf", y = "bame", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Fat`", ylab = "%BAME")

# thin perception & BAME (significant)
ggscatter(obcor, x = "girltt", y = "bame", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Thin`", ylab = "%BAME")

# thin & fat perception (significant)
ggscatter(obcor, x = "girltf", y = "girltt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Fat`", ylab = "% of Girls Self-Describing as `Too Thin`")


## Linear regressions, for plotting residuals
# regression - fat perception and childhood obesity
model <- lm(shapeprof$girl_toofat ~ as.numeric(shapeprof$Childhood_Obesity_Prevalance_..._2015.16))
summary(model)
# explore through plots
plot(model)
# add to dataframe and then plot
resid <- resid(model)
# no data for city of london
resid[33] <- NA
shapeprof$resid_tf_ob <- abs(resid)

# visualize
resid_tf_ob <- tm_shape(shapeprof) + 
  tm_fill(col = "resid_tf_ob", colorNA = c.na) + 
  tm_legend(position = c("left", "bottom"), title = " ") +
  tm_credits("Absolute Residuals:\nObesity & Fat Perception", position = c("right","top"), 
             just = c("right"), align = c("right"), size = text_sf * bigtext) + 
  tm_compass(position = c(0.9, 0.07), color.dark = "grey") + 
  tm_scale_bar(width = 0.15, position = c("right","BOTTOM"), color.dark = "grey")
tmap_mode("plot")
resid_tf_ob

# save
dev.print(pdf, "resid_tf_ob.pdf")

# thin perception and diversity
model2 <- lm(shapeprof$girl_toothin ~ as.numeric(shapeprof$X._of_population_from_BAME_groups_.2016.))
summary(model2)
plot(model2)
resid2 <- resid(model2)
resid2[33] <- NA
shapeprof$resid_tt_bame <- abs(resid2)

# visualize
resid_tt_bame <- tm_shape(shapeprof) + 
  tm_fill(col = "resid_tt_bame", colorNA = c.na) + 
  tm_legend(position = c("left", "bottom"), title = " ") +
  tm_credits("Absolute Residuals:\nBAME & Thin Perception", position = c("right","top"), 
             just = c("right"), align = c("right"), size = text_sf * bigtext) + 
  tm_compass(position = c(0.9, 0.07), color.dark = "grey") + 
  tm_scale_bar(width = 0.15, position = c("right","BOTTOM"), color.dark = "grey")
tmap_mode("plot")
resid_tt_bame

# save
dev.print(pdf, "resid_tt_bame.pdf")

## Bivariate plot of obesity & fat perception
# inspiration drawn from:
#    http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/ 
#    http://rpubs.com/apsteinmetz/prek

# create new column for bin data
bins <- 3
test <- profsize
test <- mutate(test, fatBin = cut2(girl_toofat, g = bins, levels.mean = TRUE))
test <- mutate(test, obesityBin = cut2(as.numeric(Childhood_Obesity_Prevalance_..._2015.16), g = bins, levels.mean = TRUE))
# create new mapping dataframe
map_df <- test
# bin creation
levels(map_df$fatBin) <- bins:1
levels(map_df$obesityBin) <- bins:1
# create compound bin designators
map_df <- mutate(map_df, bin = paste(fatBin, '-', obesityBin, sep=''))
map_df <- transmute(map_df, Code = Code, name = Area_name, bin = bin)
# color array, from Josh Stevens
c <- c("#e8e8e8", "#ace4e4", "#5ac8c8", "#dfb0d6", "#a5add3", "#5698b9", 
       "#be64ac", "#8c62aa", "#3b4994")
# NA color
c.na <- "white"

# remove unnecessary data
map_df <- transmute(map_df, Code = Code, bin = bin)
# merge back in spatial data
map_df <- merge(area, map_df, by.x = "GSS_CODE", by.y = "Code")

bigtext = 1
smalltext = 0.5
text_sf = 1
# plot map with legend
obesity <- tm_shape(map_df) + 
  tm_fill(col = "bin", palette = c[-2], colorNA = c.na) +
  tm_legend(show = FALSE) +
  tm_credits("Fat Perception and\nChildhood Obesity", position = c("right","top"), 
             just = c("right"), align = c("right"), size = text_sf * bigtext) + 
  tm_credits("fat perception", align = c("center"), just = c("center"),
             position = c(0.1, 0.02), size = text_sf * smalltext) +
  tm_credits("o\nb\ne\ns\ni\nt\ny", position = c(0.02, 0.07), just = c("center"), size = text_sf * smalltext) +
  tm_credits("low", position = c(0.02, 0.02), just = c("center"), size = text_sf * smalltext, col = "grey") + 
  tm_credits("high", position = c(0.18, 0.02), just = c("center"), size = text_sf * smalltext, col = "grey") +
  tm_credits("high", position = c(0.02, 0.26), just = c("center"), size = text_sf * smalltext, col = "grey") +
  tm_compass(position = c(0.9, 0.07), color.dark = "grey") + 
  tm_scale_bar(width = 0.15, position = c("right","BOTTOM"), color.dark = "grey")
tmap_mode("plot")
obesity

# function for creating legend color squares
leg <- function(color, df = map_df){
  legend <- tm_shape(map_df) + 
    tm_layout(bg.color = color) +
    tm_fill(col = "NONLD_AREA", alpha = 0, title = "test") + 
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

# plot 9 insets within map for legend
tmap_mode("plot")
obesity
generate_label()

# save as PDF
dev.print(pdf, "fatperception_obesity.pdf")


## Bivariate Plot of Thin Perception and BAME
# create bins
test2 <- profsize
test2 <- mutate(test2, thinBin = cut2(girl_toothin, g = bins, levels.mean = TRUE))
test2 <- mutate(test2, bameBin = cut2(as.numeric(X._of_population_from_BAME_groups_.2016.), g = bins, levels.mean = TRUE))
# create new mapping dataframe
map_df2 <- test2
# bin creation
levels(map_df2$thinBin) <- bins:1
levels(map_df2$bameBin) <- bins:1
# create compound bin designators
map_df2 <- mutate(map_df2, bin = paste(thinBin, '-', bameBin, sep=''))
# map_df2 <- transmute(map_df2, Code = Code, bin = bin)

# remove unnecessary data
map_df2 <- transmute(map_df2, Code = Code, name = Area_name, bin = bin)
# merge back in spatial data
map_df2 <- merge(area, map_df2, by.x = "GSS_CODE", by.y = "Code")

# plot map with legend
bame <- tm_shape(map_df2) + 
  tm_fill(col = "bin", palette = c[-3], colorNA = c.na) +
  tm_legend(show = FALSE) +
  tm_credits("Thin Perception and\nBAME Populations", position = c("right","top"), 
             just = c("right"), align = c("right"), size = text_sf * bigtext) + 
  tm_credits("thin perception", align = c("center"), just = c("center"),
             position = c(0.1, 0.02), size = text_sf * smalltext) +
  tm_credits("B\nA\nM\nE", position = c(0.02, 0.09), just = c("center"), size = text_sf * smalltext) +
  tm_credits("low", position = c(0.02, 0.02), just = c("center"), size = text_sf * smalltext, col = "grey") + 
  tm_credits("high", position = c(0.18, 0.02), just = c("center"), size = text_sf * smalltext, col = "grey") +
  tm_credits("high", position = c(0.02, 0.23), just = c("center"), size = text_sf * smalltext, col = "grey") +
  tm_compass(position = c(0.9, 0.07), color.dark = "grey") + 
  tm_scale_bar(width = 0.15, position = c("right","BOTTOM"), color.dark = "grey")

# plot
tmap_mode("plot")
bame
generate_label(map_df2)

# save as PDF
dev.print(pdf, "thinperception_bame.pdf")
