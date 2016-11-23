# 1) put your data frame in order; 
# 2) set up a basic plot with the qplot or ggplot function; 
# 3) add one extra geom at at time (optional if you make a simple plot 
#                                   with qplot, since qplot sets up the first 
#                                   geometry for you); 
# 4) add the settings needed to make the plot look good.


## GGPLOT2 TUTORIAL
# Tutorial from: http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# Data from: http://datatoolkits.lincolninst.edu/subcenters/land-values/land-prices-by-state.asp


# set working directory
setwd("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R")

# load libraries
library(ggplot2)

# read data
housing <- read.csv("datasets\\landdata-states.csv", header=T)

# get a sense of data
head(housing[1:5])
names(housing)
housing$Date

# data manipulation
housing$Year <- as.numeric(substr(housing$Date, 1, 4))
housing$Qrtr <- as.numeric(substr(housing$Date, 5, 5))
housing$Date <- housing$Year + housing$Qrtr/4

housing$Home.Value <- gsub("\\$", '', housing$Home.Value)
housing$Home.Value <- as.numeric(gsub("\\,", '', housing$Home.Value))

# renaming columns 
colnames(housing) <- c("State", "Date", "Home.Value", "Structure.Cost", "Land.Value", "Land.Share", "Home.Price.Index", "Land.Price.Index")


# plotting histogram
ggplot(housing, aes(x = Home.Value)) + geom_histogram()
hist(housing$Home.Value)

# plotting values

# with base
plot(Home.Value ~ Date,
     data=subset(housing, State == "MA"))
points(Home.Value ~ Date, col="red",
       data=subset(housing, State == "TX"))
legend(19750, 400000,
       c("MA", "TX"), title="State",
       col=c("black", "red"),
       pch=c(1, 1))

# with ggplot2

roundUp <- function(x) 10^ceiling(log10(x))
maximum <- roundUp(max(housing$Home.Value))
intervals <- maximum/1000000


ggplot(subset(housing, State %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=State)) + geom_point() + scale_y_continuous(breaks=seq(0, maximum, maximum/1000000))


if (require(ggplot2movies)) {
  m <- ggplot(subset(movies, votes > 1000), aes(rating, votes)) +
    geom_point(na.rm = TRUE)
  
  m + scale_y_continuous(limits = c(0, 5000))
  m + scale_y_continuous(limits = c(1000, 10000))
  m + scale_x_continuous(limits = c(7, 8))
}



## GGPLOT2 tutorial for time
# Tutorial from: http://neondataskills.org/R/time-series-plot-ggplot/