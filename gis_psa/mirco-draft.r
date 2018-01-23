# Heidi Hurst
# Principles of Spatial Analysis Coursework

# library imports
library("sp")
library("rgdal")
library("tmap")
library(classInt)
library(RColorBrewer)
library(maptools)
library(ggplot2)

# set working directory
setwd("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data")

# import data on size perception
size <- read.csv("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data/whataboutsize.csv", stringsAsFactors=FALSE)
satisfaction <- read.csv("whataboutsatisfaction.csv", stringsAsFactors=FALSE)

# import spatial data
area <- readOGR("/Users/heidihurst/Documents/UK/ucl-gis/t1/principles-spatial-analysis/assignment1-mirco/data/boundaries/", "London_Borough_Excluding_MHW")

# plot to confirm
plot(area)

# join size data to area data
whatabout <- merge(size, satisfaction, by.x = "Authorith_code", by.y = "loc_code")
boroughsize <- merge(area, whatabout, by.x = "GSS_CODE", by.y = "Authorith_code")

# function to plot values
plotVar <- function(var, title = "", n = 5, colorscheme = "PuBuGn", method = "pretty", add = FALSE){
  breaks <- classIntervals(var, n = n, style = method)
  my_colours <- brewer.pal(n, colorscheme)
  plot(boroughsize, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)], 
       axes = FALSE, border = rgb(0.8,0.8,0.8,0), add = add) 
  legend(x = 499761.2, y = 169345.2, legend = leglabs(breaks$brks), fill = my_colours, 
         bty = "n", cex = 0.7)
  title(title)
}

plotVar(boroughsize$girl_toofat, "Girl - Too Fat", 7, colorscheme = "Purples")
plotVar(boroughsize$boy_toofat, "Boy - Too Fat", 7, colorscheme = "Blues")
plotVar(boroughsize$boy_toothin, "Boy - Too Thin", 7, colorscheme = "Greens")

# explore correlations
cordata <- data.frame(boroughsize$girl_toothin, boroughsize$boy_toothin, boroughsize$girl_toofat, boroughsize$boy_toofat, boroughsize$girl_aboutright, boroughsize$boy_aboutright)
cor(cordata, use = "complete.obs")

# import borough data/information
prof <- read.csv("london-borough-profiles2.csv", header=TRUE)

# merge borough data with self image data
profsize <- merge(prof, whatabout, by.x = "Code", by.y = "Authorith_code")

# merge atlas data with existing shapefiles
shapeprof <- merge(boroughsize, prof, by.x = "GSS_CODE", by.y = "Code")

# explore correlations between stuff...
par(mfrow = c(2,2))
plotVar(shapeprof$girl_toofat, "Girl- Too Fat", 6, colorscheme = "Purples", method = "quantile")
plotVar(as.numeric(shapeprof$Median_House_Price._2015), "median house price", 6, colorscheme = "Purples", method = "quantile")
plotVar(as.numeric(shapeprof$Childhood_Obesity_Prevalance_..._2015.16))

# more correlations - exploratory
cordata2 <- data.frame(shapeprof$girl_toofat,
                       shapeprof$girl_toothin,
                       as.numeric(shapeprof$Childhood_Obesity_Prevalance_..._2015.16), 
                       as.numeric(shapeprof$Gross_Annual_Pay._.2016.), 
                       as.numeric(shapeprof$X._of_adults_who_cycle_at_least_once_per_month._2014.15), 
                       as.numeric(shapeprof$Employment_rate_..._.2015.),
                       as.numeric(shapeprof$Teenage_conception_rate_.2014.), 
                       as.numeric(shapeprof$Median_House_Price._2015),
                       as.numeric(shapeprof$X._of_population_from_BAME_groups_.2016.),
                       as.numeric(shapeprof$Female_employment_rate_.2015.),
                       as.numeric(shapeprof$Gross_Annual_Pay_._Female_.2016.),
                       as.numeric(shapeprof$Proportion_of_working_age_people_with_no_qualifications_..._2015),
                       as.numeric(shapeprof$Proportion_of_working_age_with_degree_or_equivalent_and_above_..._2015),
                       as.numeric(shapeprof$People_aged_17._with_diabetes_...),
                       as.numeric(shapeprof$Life_satisfaction_score_2011.14_.out_of_10.),
                       as.numeric(shapeprof$Happiness_score_2011.14_.out_of_10.),
                       as.numeric(shapeprof$Worthwhileness_score_2011.14_.out_of_10.),
                       as.numeric(spacemh$Generalised.anxiety.disorder.Estimated.cases))
# rename for compactness
colnames(cordata2) <- c("girltoofat", "girltoothin", "Obesity", "GrossPay", "MonthlyCycle", 
                        "Employment", "conception", "House$", "BAME", "fememploy",
                        "fempay", "noqual", "degqual", "diabetes", "life", 
                        "happiness", "worthwhile", "anxiety")
# take full correllation matrix and explore
cor(cordata2, use = "complete.obs")

# more info from youth survey?

# more mental health info?
mentalhealth <- read.csv("mental-health-common-problems-borough.csv")
# try and merge (by name - codes don't match)
spacemh <- merge(shapeprof, mentalhealth, by.x = "Area_name", by.y = "Area")

# correllation between satisfaction? plot boys & girls
par(mfrow = c(2,2))
plotVar(as.numeric(boroughsize$girl_toofat), "girl tf", 5, "Reds", "quantile")
plotVar(as.numeric(boroughsize$girls_low), "girl low", 5, "Reds", "quantile")
plotVar(as.numeric(boroughsize$boy_toofat), "boy tf", 5, "Blues", "quantile")
plotVar(as.numeric(boroughsize$boys_low), "boy low", 5, "Blues", "quantile")

cordata3 <- data.frame(as.numeric(boroughsize$girl_toofat),
                       as.numeric(boroughsize$girl_aboutright),
                       as.numeric(boroughsize$girl_toothin),
                       as.numeric(boroughsize$boy_toofat),
                       as.numeric(boroughsize$boy_aboutright),
                       as.numeric())

whatabout2 <- subset(whatabout, select = -c("Authorith_code", "Authority_name", "loc_name"))


# can we model the likelihood of girls thinking they're too fat?
model <- lm(shapeprof$girl_toofat ~ shapeprof$Childhood_Obesity_Prevalance_..._2015.16+shapeprof$Gross_Annual_Pay._.2016.)
plot(model)
summary(model)
head(shapeprof)

model <- lm(cordata2$girltoofat ~ cordata2$Obesity+cordata2$`House$`)
summary(model)

model <- lm(profsize$girl_toofat ~ ., data = profsize[,3:99])
summary(model)

# look at correlation between obesity and self perception
obcor <- data.frame(as.numeric(profsize$girl_toofat),
                    as.numeric(profsize$boy_toofat),
                    as.numeric(profsize$girl_toothin),
                    as.numeric(profsize$boy_toothin),
                    as.numeric(profsize$Childhood_Obesity_Prevalance_..._2015.16),
                    as.numeric(profsize$People_aged_17._with_diabetes_...),
                    as.numeric(profsize$X._of_population_from_BAME_groups_.2016.),
                    as.numeric(profsize$Female_life_expectancy._.2012.14.),
                    as.numeric(profsize$Gross_Annual_Pay._.2016.),
                    as.numeric(profsize$Teenage_conception_rate_.2014.),
                    as.numeric(profsize$Worthwhileness_score_2011.14_.out_of_10.))

colnames(obcor) <- c("girltf", "boytf", "girltt", "boytt", "obesity", "diabetes", 
                     "bame", "flifexp", "grosspay", "conception", "worthwhile")
cor(obcor)

# plot relationship between girls' self perception and obesity rates
library("ggpubr")
# fat girls & obesity (NEG): significant
ggscatter(obcor, x = "girltf", y = "obesity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Fat`", ylab = "Childhood Obesity Rates")

# fat girls & income
ggscatter(obcor, x = "girltf", y = "grosspay", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Fat`", ylab = "Income")

# thin girls & BAME
ggscatter(obcor, x = "girltt", y = "bame", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Thin`", ylab = "%BAME")

# thin & fat girls (significant)
ggscatter(obcor, x = "girltf", y = "girltt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Fat`", ylab = "% of Girls Self-Describing as `Too Thin`")

# thin & obesity
ggscatter(obcor, x = "girltt", y = "obesity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Thin`", ylab = "Childhood Obesity")

# thin & obesity
ggscatter(obcor, x = "girltf", y = "worthwhile", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Girls Self-Describing as `Too Fat`", ylab = "Worthwhileness")

# boys & obesity: not significant
ggscatter(obcor, x = "boytt", y = "obesity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "% of Boys Self-Describing as `Too Thin`", ylab = "Childhood Obesity Rates")

# plot girls' self perception in a map
par(mfrow = c(1,1))
plotVar(shapeprof$girl_toofat, "Girl - Too Fat", 6, colorscheme = "Purples", method = "jenks")
plotVar(as.numeric(shapeprof$Childhood_Obesity_Prevalance_..._2015.16), "Childhood Obesity", 6, colorscheme = "Purples", method = "jenks")
plotVar(shapeprof$girl_toothin, "Girl - Too Thin", 6, colorscheme = "Purples", method = "jenks")
plotVar(as.numeric(shapeprof$X._of_population_from_BAME_groups_.2016.), "BAME", 6, colorscheme = "Purples", method = "jenks")
plotVar(as.numeric(shapeprof$Gross_Annual_Pay._.2016.), "Gross Annual Pay", 6, colorscheme = "Purples", method = "jenks")

par(mfrow = c(1,2))
plotVar(shapeprof$girl_toofat, "Girl - Too Fat", 6, colorscheme = "Purples", method = "jenks")
plotVar(as.numeric(shapeprof$Childhood_Obesity_Prevalance_..._2015.16), "Childhood Obesity", 6, colorscheme = "Purples", method = "jenks")

# linear regression - too fat and obesity
model <- lm(obcor$girltf ~ obcor$obesity)
summary(model)
plot(model)
# todo: add this to dataframe and then plot
resid(model)

# linear regression - too thin and diversity
model2 <- lm(girltt ~ bame, data = obcor)
summary(model2)

# lin - too fat & income
model3 <- lm(girltf ~ obesity + girltt + conception, data = obcor)
summary(model3)

plotVar(as.numeric(shapeprof$Childhood_Obesity_Prevalance_..._2015.16), "Girl - Too Fat", 6, colorscheme = "Purples", method = "jenks")
