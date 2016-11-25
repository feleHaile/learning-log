# 1) put your data frame in order; 
# 2) set up a basic plot with the ggplot function; 
# 3) add one extra geom at at time (optional if you make a simple plot 
#                                   with qplot, since qplot sets up the first 
#                                   geometry for you); 
# 4) add the settings needed to make the plot look good, such as axis labels

# notes:
# axis and points plotted are on different settings

# main functions of ggplot

# scales
# geom types and data transformation
# faceting


# Useful GGPLOT2 functions ------------------------------------------------
 
# geom_type
# labs
# scale_x_continuous
 
# ggsave
 
# under aes 
# color
# shape

# GGPLOT2 tutorial --------------------------------------------------------

# Tutorial from: http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# Data from: http://datatoolkits.lincolninst.edu/subcenters/land-values/land-prices-by-state.asp

# set working directory
setwd("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R")

# load libraries
library(ggplot2)

# read data
housing <- read.csv("datasets\\landdata-states.csv", header=T)
head(housing)

# get a sense of data
head(housing[1:5])
names(housing)
housing$Date

# data manipulation
housing$Year <- as.numeric(substr(housing$Date, 1, 4))
housing$Qrtr <- as.numeric(substr(housing$Date, 6, 6))
housing$Date <- housing$Year + housing$Qrtr/4

housing$Home.Value <- gsub("\\$", '', housing$Home.Value)
housing$Home.Value <- as.numeric(gsub("\\,", '', housing$Home.Value))

housing$Structure.Cost <- gsub("\\$", '', housing$Structure.Cost)
housing$Structure.Cost <- as.numeric(gsub("\\,", '', housing$Structure.Cost))

housing$Land.Value <- gsub("\\$", '', housing$Land.Value)
housing$Land.Value <- as.numeric(gsub("\\,", '', housing$Land.Value))

# renaming columns 
colnames(housing) <- c("State", "Date", "Home.Value", "Structure.Cost", "Land.Value", "Land.Share", "HPI", "LPI", "year", "quarter")

## plotting histogram
hist(housing$Home.Value) # base
ggplot(housing, aes(x = Home.Value)) + geom_histogram() # ggplot

## plotting values

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
intervals <- maximum/10
ma.tx.housing <- subset(housing, State %in% c("MA", "TX"))

ggplot(ma.tx.housing,
       aes(x=Date,
           y=Home.Value,
           color=State)) + geom_crossbar()

# tried geom_point, geom_area(), geom_bin2d(), boxplot()
# does not work with geom_bar() 
# requires more variables geom_crossbar()

# help.search("geom_", package = "ggplot2")

## scatterplot
hp2001Q1 <- subset(housing, Date == 2001.25) 
ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = Land.Value)) +
  geom_point()

ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = log(Land.Value))) +
  geom_point()

# predictions; adding on values
hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1))

p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))

p1 + geom_point(aes(color = Home.Value)) +
  geom_line(aes(y = pred.SC))

# smoothers
p1 +
  geom_point(aes(color = Home.Value)) +
  geom_smooth()

# labels
p1 + 
  geom_text(aes(label=State), size = 3)

# colours
p1 +
  geom_point(aes(size = 2, # incorrect! 2 is not a variable
             color="red")) # this is fine -- all points red

ggplot(housing, aes(x = HPI, y = LPI)) +
  geom_point()
ggplot(housing, aes(x = HPI, y = LPI)) +
  geom_point(color="blue")

###################################################

## histogram

p2 <- ggplot(housing, aes(x=Home.Value))
p2 + geom_histogram(stat = "bin", binwidth=10000) + theme_bw()

## transformation (stat arg)

housing.sum <- aggregate(housing["Home.Value"], housing["State"], FUN=mean)
# rbind(head(housing.sum), tail(housing.sum))
ggplot(housing.sum, aes(x=State, y=Home.Value)) + 
  geom_bar(stat="identity") + theme_bw()

# x = lpi, y = hdi

ex2 <- ggplot(housing, aes(x=HPI, y=LPI)) 
ex2 + geom_point() + geom_line(stat="smooth", method="loess")
ex2 + geom_point() + geom_smooth(span = .4)

ex2 + geom_point() 

## scales
p3 <- ggplot(housing,
             aes(x = State,
                 y = HPI)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6))

p4 <- p3 + geom_point(aes(color= Date), 
                      alpha = 0.5, 
                      size = 1.5, 
                      position = position_jitter(width = 0.2,
                                                 height = 0))

p4 + scale_x_discrete(name="\nState Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = "green", 
                         high = "red")

## faceting

p5 <- ggplot(housing, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color=State))
p5 <- p5 + geom_line() + facet_wrap(~State, ncol=10) + theme_linedraw() # with n columns
p5 + facet_grid(~State) # in columns


## themes

p5 + theme_minimal() + theme(text = element_text(color="#28AADC"))

# arguments involved
theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "grey", fill = "grey"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = muted("grey")))

p5 + theme_new

#http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
#https://www.rdocumentation.org/packages/plyr/versions/1.8.4/topics/ddply
#http://seananderson.ca/2013/12/01/plyr.html

############################################################################

# ECONOMIST GRAPH ---------------------------------------------------------

dat <- as.data.frame(read.csv("datasets/EconomistData.csv", header=T))

e1 <- ggplot(dat, aes(x=CPI, y=HDI, color=Region))

e1 + geom_point()

# add trendline
e2 <- e1 + geom_smooth(aes(group = 1), 
                       method = "lm", 
                       formula = y ~ log(x),
                       se = FALSE,
                       color = "red") 

# point shape

# Testing possible shapes
###################################################################
df <- data.frame(x=1:5, y=1:25, z=1:25)
s <- ggplot(df, aes(x=x, y=y))
s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()
s + geom_point(aes(shape = z), size = 4, colour = "Red") +
  scale_shape_identity()
s + geom_point(aes(shape = z), size = 3 , colour = "Black", fill = "Grey") +
  scale_shape_identity()
###################################################################

e3 <- e2 + geom_point(shape=1, size=2.5, stroke=1.25)
e3

# Labelling points
names(dat)
dat$Country


# How to choose these points?? 
# Out of SE
labels <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

e4 <- e3 + geom_text(aes(label=Country), colour = "Grey40", 
                     data = subset(dat, Country %in% labels))

e4

# region labels with order

dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))


# levels are for order (ordinal variable), 
# labels are for printing

e4$data <- dat
e4

# add title and format axes

e5 <- e4 +
  scale_x_continuous(name = "\n\n\nCorruption Perceptions Index, 2011 (10=least corrupt)\n\n\n",
                     limits = c(.9, 10.5),
                     breaks = 1:10) +
  scale_y_continuous(name = "Human Development Index, 2011 (1=Best)\n\n\n",
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, by = 0.1)) +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F")) +
  ggtitle("Corruption and Human development")

e5

# theme tweaks

library(grid)
e6 <- e5 +
  theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # position the legend in the upper left 
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
  )

e6

# modelling the R2

# calculating
model.R2 <- summary(lm(HDI ~ log(CPI), data = dat))$r.squared

grid.text("Sources: Transparency International; UN Human Development Report",
          x = .02, y = .03,
          just = "left",
          draw = TRUE)
grid.segments(x0 = 0.81, x1 = 0.825,
              y0 = 0.90, y1 = 0.90,
              gp = gpar(col = "red"),
              draw = TRUE)
grid.text(paste0("R² = ",
                 as.integer(model.R2*100),
                 "%"),
          x = 0.835, y = 0.90,
          gp = gpar(col = "gray20"),
          draw = TRUE,
          just = "left")

dev.off()


# GGPLOT2 tutorial with time ----------------------------------------------
# Tutorial from: http://neondataskills.org/R/time-series-plot-ggplot/

# load libraries
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

# set working directory
setwd("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R")

# read csv: daily met
met.daily <- read.csv(
  file="datasets\\HARV\\FisherTower-Met\\Met_HARV_Daily_2009_2011.csv",
  stringsAsFactors = FALSE, header=T)

# convert data to date
met.daily $date <- as.Date(met.daily $date)

# read csv: monthly temperature
temp.mon <-read.csv(
  file="datasets\\HARV\\FisherTower-Met\\Temp_HARV_Monthly_09_11.csv",
  stringsAsFactors=FALSE
)

# convert data to date
temp.mon$date <- as.Date(temp.mon$datetime)


# names
names(met.daily)
names(temp.mon)

### GGPLOT 

gg <- ggplot(met.daily, aes(date, airt))


# plot a scatterplot
ggplot(met.daily, aes(date, airt)) +
  geom_point(na.rm=TRUE)

# customize a scatterplot
gg1 <- gg + geom_point(na.rm=TRUE, color="purple", size=1)
gg1

# further labels
gg2 <- gg1 + ggtitle("Air temperature from 2009-2011\n") + 
  xlab("\n\nDate") + ylab("Air Temperature (C)\n\n")
gg2

# name plot objects
gg3 <- gg2 + scale_x_date(labels=date_format("%b %y"))
gg3

# adjust ticks
gg3b <- gg2 + scale_x_date(labels=date_format("%b %Y"),
                           breaks=date_breaks("6 months"))
gg3b

# plot only time which is preferred
start <- as.Date("2011-01-01")
end <- as.Date("2012-01-01")

gg.2011 <- gg1 + ggtitle("Air temperature \n 2011") +
  geom_point(na.rm=TRUE, color="purple", size=1) +
  xlab("Date") + ylab("Air Temperature (C)") +
  scale_x_date(limits=c(start,end), breaks=date_breaks("1 year"),
                 labels=date_format("%b %Y"))
gg.2011




# DRAWING ON DIAGRAM ------------------------------------------------------

# Source: https://stat.ethz.ch/R-manual/R-devel/library/grid/html/grid.text.html

grid.newpage()
x <- stats::runif(20)
y <- stats::runif(20)
rot <- stats::runif(20, 0, 360)
grid.text("SOMETHING NICE AND BIG", x=x, y=y, rot=rot,
          gp=gpar(fontsize=20, col="grey"))
grid.text("SOMETHING NICE AND BIG", x=x, y=y, rot=rot,
          gp=gpar(fontsize=20), check=TRUE)


grid.newpage()
draw.text <- function(just, i, j) {
  grid.text("ABCD", x=x[j], y=y[i], just=just)
  grid.text(deparse(substitute(just)), x=x[j], y=y[i] + unit(2, "lines"),
            gp=gpar(col="grey", fontsize=8))
}

x <- unit(1:4/5, "npc")
y <- unit(1:4/5, "npc")

grid.grill(h=y, v=x, gp=gpar(col="grey"))

draw.text(c("bottom"), 1, 1)
draw.text(c("left", "bottom"), 2, 1)
draw.text(c("right", "bottom"), 3, 1)
draw.text(c("centre", "bottom"), 4, 1)
draw.text(c("centre"), 1, 2)
draw.text(c("left", "centre"), 2, 2)
draw.text(c("right", "centre"), 3, 2)
draw.text(c("centre", "centre"), 4, 2)
draw.text(c("top"), 1, 3)
draw.text(c("left", "top"), 2, 3)
draw.text(c("right", "top"), 3, 3)
draw.text(c("centre", "top"), 4, 3)
draw.text(c(), 1, 4)
draw.text(c("left"), 2, 4)
draw.text(c("right"), 3, 4)
draw.text(c("centre"), 4, 4)


# GRID ARRANGE  -----------------------------------------------------------

# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
