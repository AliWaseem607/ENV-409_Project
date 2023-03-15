library(chron)
library(tidyverse)

#Options from Stakahama
Sys.setlocale("LC_TIME","C")
options(stringsAsFactors=FALSE)
options(chron.year.abb=FALSE)
theme_set(theme_bw()) # just my preference for plots

wdirLC <- "C:/Users/loren/Desktop/MA2/Air Pollution/ENV-409_Project"
setwd(wdirLC)

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
  data[,"year"] <- chron::years(data[,timecolumn])
  data[,"month"] <- months(data[,timecolumn])
  data[,"day"] <- chron::days(data[,timecolumn])
  data[,"hour"] <- chron::hours(data[,timecolumn])
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

df <- full_join(cbind(site="LUG", ReadTSeries("./data/LUG.csv")),
                cbind(site="RIG", ReadTSeries("./data/RIG.csv")))

lf <- df %>%
  gather(variable, value,
         -c(site, datetime, season, year, month, day, hour, dayofwk, daytype))


ggplot(lf)+                                        # `lf` is the data frame
facet_grid(variable~site, scale="free_y")+         # panels created out of these variables
geom_line(aes(datetime, value, color=site))+       # plot `value` vs. `time` as lines
scale_x_chron()+                                   # format x-axis labels (time units)
theme(axis.text.x=element_text(angle=30, hjust=1)) # rotate x-axis labels

ggplot(lf) +
  facet_grid(variable ~ site, scale = "free_y") +
  geom_boxplot(aes(month, value), outlier.size = 0.5, outlier.shape = 3)
