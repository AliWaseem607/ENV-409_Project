library(chron)
library(tidyverse)

#Options from Stakahama
Sys.setlocale("LC_TIME","C")
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots

wdir <- "C:/Users/aliwa/AirPollution/Project"
setwd(wdir)

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

timecolumn <- "Date/time"
timeformat="%d.%m.%Y %H:%M"
data <- read.table("./data/RIG.csv", skip=5, header=TRUE, sep=";", check.names=FALSE)
names(data) <- sub("[ ].*$","",names(data)) # strip units for simplification
names(data) <- sub("Date/time", "Date/time", names(data), fixed=TRUE)
data[,"Date/time"] <- as.chron(data[,"Date/time"], timeformat) - 1/24 # end time -> start time
class(data[1,"Date/time"])
months(data[1,"Date/time"])
days(data[1,"Date/time"])
hours(data[1,"Date/time"])
minutes(data[1,"Date/time"])

for (i in 1:8760){
  print(i)
  weekdays(data[i,"Date/time"])
}
data[2,"Date/time"]

data[,"year"] <- years(data[,timecolumn])
data[,"month"] <- months(data[,timecolumn])
data[,"day"] <- days(data[,timecolumn])
data[,"hour"] <- hours(data[,timecolumn])
data[,"dayofwk"] <- weekdays(data[,timecolumn])
data[,"daytype"] <- ifelse(data[,"dayofwk"] %in% c("Sat","Sun"), "Weekend", "Weekday")
data[,"season"] <- Month2Season(unclass(data[,"month"]))
