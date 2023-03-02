library(chron)
library(tidyverse)

#Options from Stakahama
Sys.setlocale("LC_TIME","C")
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots

wdirAW <- "C:/Users/aliwa/AirPollution/Project"
wdirLC <- ""
setwd(wdirAW)

# Function from STakahama
ReadTSeries <- function(filename, timecolumn="datetime", timeformat="%d.%m.%Y %H:%M") {
  ## read the table, strip units in column names, rename time column
  ##   and change data type of time column from a string of characters to
  ##   a numeric type so that we can perform operations on it
  data <- read.table(filename, skip=5, header=TRUE, sep=";", check.names=FALSE)
  names(data) <- sub("[ ].*$","",names(data)) # strip units for simplification
  names(data) <- sub("Date/time", timecolumn, names(data), fixed=TRUE)
  data[,timecolumn] <- as.chron(data[,timecolumn], timeformat) - 1/24 # end time -> start time
  ## extract additional variables from the time column
  data[,"year"] <- years(data[,timecolumn])
  data[,"month"] <- months(data[,timecolumn])
  data[,"day"] <- days(data[,timecolumn])
  data[,"hour"] <- hours(data[,timecolumn])
  data[,"dayofwk"] <- weekdays(data[,timecolumn])
  data[,"daytype"] <- ifelse(data[,"dayofwk"] %in% c("Sat","Sun"), "Weekend", "Weekday")
  data[,"season"] <- Month2Season(unclass(data[,"month"]))
  ## return value
  data
}

Month2Season <- function(month) {
  ## month is an integer (1-12)
  ## a factor with levels {"DJF", "MAM", "JJA", "SON"} is returned
  seasons <- c("DJF", "MAM", "JJA", "SON")
  index <- findInterval(month %% 12, seq(0, 12, 3))
  factor(seasons[index], seasons)
}


LUG<-ReadTSeries("./data/LUG.csv")
RIG<-ReadTSeries("./data/RIG.csv")