# OVERVIEW ----------------------------------------------------------------

# 1) put your data frame in order; 
# 2) set up a basic plot with the ggplot function; 
# 3) add one extra geom at at time (optional if you make a simple plot 
#                                   with qplot, since qplot sets up the first 
#                                   geometry for you); 
# 4) add the settings needed to make the plot look good, such as axis labels

# notes:
# axis and points plotted are on different settings

# Main functions of ggplot2, and arguments involved

# scales
# geom types and data transformation # http://docs.ggplot2.org/current/
# faceting
# themes: colour schemes, legend positions
# labels
# annotation

# COMPLEMENTARY LIBRARIES -------------------------------------------
library(ggvis)     # for interactivity purposes
library(grid)      # for annotation purposes 
library(gridExtra) # for annotation purposes


# USEFUL GGPLOT2 FUNCTIONS ------------------------------------------------
 
# geom_type
# labs
# scale_x_continuous
 
# ggsave
 
# under aes 
# color
# shape

# TUT1: BASIC PRIMER FOR GGPLOT2 ------------------------------------------------
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

# TUT2: ECONOMIST GRAPH ---------------------------------------------------------

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



# TUT3: PLOTTING TIME SERIES WITH GGPLOT2 ---------------------------------------
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




# TUT4: DRAWING ON DIAGRAM ------------------------------------------------------

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

# TUT5: GRID ARRANGE  -----------------------------------------------------------

# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
# http://r-statistics.co/ggplot2-Tutorial-With-R.html

library(grid)
my_grob <- grobTree(textGrob("This text is at x=0.1 and y=0.9, relative!\n Anchor point is at 0,0", x=0.1,  y=0.9, hjust=0,
                            gp=gpar(col="firebrick", fontsize=25, fontface="bold")))
ggplot(mtcars, aes(x=cyl)) + geom_bar() + annotation_custom(my_grob) + labs(title="Annotation Example")


# TUT6: SPATIAL DATA ------------------------------------------------------

# Tutorial: https://www.codementor.io/spark/tutorial/exploratory-geographical-data-using-sparkr-and-ggplot2
# https://github.com/jadianes/spark-r-notebooks/blob/master/notebooks/nb1-spark-sql-basics/nb1-spark-sql-basics.ipynb


population_data_files_url <- 'http://www2.census.gov/acs2013_1yr/pums/csv_pus.zip'
housing_data_files_url <- 'http://www2.census.gov/acs2013_1yr/pums/csv_hus.zip'

library(RCurl)

population_data_file <- getBinaryURL(population_data_files_url)

# Set Spark home and R libs
Sys.setenv(SPARK_HOME='C:/programs/spark-2.0.2-bin-hadoop2.7')
.libPaths(c(file.path(Sys.getenv('SPARK_HOME'), 'R', 'lib'), .libPaths()))

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
library(rJava)
library(backports)

sc <- sparkR.session(spark.master='local', sparkPackages="com.databricks:spark-csv_2.11:1.2.0")

housing_a_file_path <- file.path('', 'nfs','data','2013-acs','ss13husa.csv')
housing_b_file_path <- file.path('', 'nfs','data','2013-acs','ss13husb.csv')

housing_a_df <- read.df(sc, 
                        housing_a_file_path, 
                        header='true', 
                        source = "com.databricks.spark.csv", 
                        inferSchema='true')

housing_b_df <- read.df(sqlContext, 
                        housing_b_file_path, 
                        header='true', 
                        source = "com.databricks.spark.csv", 
                        inferSchema='true')


# TUT7: SPATIAL DATA2 -----------------------------------------------------

# http://spatial.ly/wp-content/uploads/2013/12/intro-spatial-rl-3.pdf

x <- 1:400
y <- sin(x/10) * exp (x* -0.01)

plot (x, y)

setwd("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R\\datasets")

library(ggmap)      # extends ggplot2 for maps
library(rgdal)      # spatial data 
library(rgeos)      # vector data 
library(maptools)   # mapping functions
library(plyr)       # processing data
library(tidyr)      # processing data
library(tmap)       # creating maps

lnd <- readOGR(dsn = "map", layer = "london_sport")
head(lnd@data, n = 2)
mean(lnd$Partic_Per)

# plot all vectors
plot(lnd) 

# based on certain criteria
sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25
plot(lnd[sel, ]) 

# colours
plot(lnd, col = "lightblue")
sel <- lnd$Partic_Per > 25
plot(lnd[ sel, ], col = "turquoise", add = TRUE) 


# spatial map
library(rgeos)

par(mfrow=c(2,1))
plot(lnd, col = "grey")                             ## plot basemap 

## find London's geographic centroid (add ", byid = T" for all)
cent_lnd <- gCentroid(lnd[lnd$name == "City of London",])       ## getting the centroid coordinate of london
points(cent_lnd, cex = 3)

## set 10 km buffer
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 

## method 1 of subsetting selects any intersecting zones
lnd_central <- lnd[lnd_buffer,] # the selection is too big!

## test the selection for the previous method - uncomment below
plot(lnd_central, col = "lightblue", add = T)
plot(lnd_buffer, add = T) # some areas just touch the buffer

## method2 of subsetting selects only points within the buffer
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) ## create spatialpoints, obtain the crs by using
                                                                ## proj4string; finding the centroids of all
sel <- lnd_cents[lnd_buffer,]                                   ## select centroids of cities inside buffer
points(sel)                                                     ## show where the points are located
lnd_central <- lnd[sel,]                                        ## select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)

## Add text to the plot!
text(coordinates(cent_lnd), "Central\nLondon")


############## Selecting quadrants

dev.off()
## create outline of london by merging all polygons 
london = gUnaryUnion(lnd, lnd$dummy)
london = SpatialPolygonsDataFrame(london, data.frame(dummy = c("london")), match.ID = FALSE)   ## adding dataframe

## centroid of london
centrelondon = gCentroid(london, byid = TRUE)

# coordinates for start and end
c1 = c(centrelondon$x, centrelondon$x)
c2 = c(90, -90)
c3 = c(90, -90)
c4 = c(centrelondon$y,centrelondon$y)

# using line strings
L1 = Line(cbind(c1, c2))
L2 = Line(cbind(c3, c4))

# create lines
Ln1 = Lines(list(L1), ID = "a")
Ln2 = Lines(list(L2), ID = "b")

# convert the lines into SpatialLines
Ls1 <- SpatialLines(LinesList = list(Ln1))
Ls2 <- SpatialLines(LinesList = list(Ln2))

# convert into SpatialLinesDataFrame
Longitude = SpatialLinesDataFrame(Ls1, data.frame(Z = c("1", "2"), row.names = c("a","b")))
Latitude = SpatialLinesDataFrame(Ls2, data.frame(Z = c("1", "2"), row.names = c("a","b")))

# test whether or not coordinate is east or north
## east/west
east <- coordinates(lnd)[,1] > Longitude@lines[[1]]@Lines[[1]]@coords[,1][1]    # each coordinate in london 
west <- coordinates(lnd)[,1] < Longitude@lines[[1]]@Lines[[1]]@coords[,1][1]
## north/south
north <- coordinates(lnd)[,2] > Latitude@lines[[1]]@Lines[[1]]@coords[,2][1]
south <-coordinates(lnd)[,2] < Latitude@lines[[1]]@Lines[[1]]@coords[,2][1]

lnd@data$quadrant[east & north] <- "northeast"
lnd@data$quadrant[west & north] <- "northwest"
lnd@data$quadrant[east & south] <- "southeast"
lnd@data$quadrant[west & south] <- "southwest"
#lnd@data    # data component

summary(lnd)
plot(lnd)

plot(lnd)
plot(lnd[east & north,],add = TRUE, col = "red" )
plot(lnd[west & north,],add = TRUE, col = "blue" )
plot(lnd[east & south,],add = TRUE, col = "green" )
plot(lnd[west & south,],add = TRUE, col = "yellow" )        
llgridlines(lnd, lty= 3, side ="EN", offset = -0.5)         # plotting grid lines

############## Creating new vector data

## creating new vector data

vec <- vector(mode = "numeric", length = 3)
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4))

class(vec)
class(df)

## new data

mat <- as.matrix(df)                                        # create matrix object with as.matrix

sp1 <- SpatialPoints(coords = mat)
class(sp1)

spdf <- SpatialPointsDataFrame(sp1, data = df)
class(spdf)

############# Setting projections

proj4string(lnd) <- NA_character_ # remove CRS information from lnd
proj4string(lnd) <- CRS("+init=epsg:27700") # assign a new CRS

# EPSG codes available here: http://www.epsg-registry.org/ 
###################################################################
EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code
###################################################################

lnd84 <- spTransform(lnd, CRS("+init=epsg:4326")) # reproject
saveRDS(object = lnd84, file = "map\\lnd84.Rds")

# remove
rm(lnd84)

############# Attribute joins
library(rgdal)

lnd <- readOGR(dsn = "map", "london_sport")
plot(lnd)
nrow(lnd) 

# new crime data object
crime_data <- read.csv("map\\mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)

head(crime_data, 3)                                                    # display first 3 lines
head(crime_data$CrimeType)                                             # information about crime type

crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]
head(crime_theft, 2)                              # take a look at the result (replace 2 with 10 to see more rows)

# Calculate the sum of the crime count for each district, save result
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)
# Show the first two rows of the aggregated crime data
head(crime_ag, 2)

# Compare the name column in lnd to Borough column in crime_ag to see which rows match.
lnd$name %in% crime_ag$Borough

# Return rows which do not match
lnd$name[!lnd$name %in% crime_ag$Borough] 
crime_ag$Borough[!crime_ag$Borough %in% lnd$name]

names(crime_ag)
crime_ag[crime_ag$Borough == 'NULL' & crime_ag$CrimeCount < 4000,]

library(dplyr)

head(lnd$name)
head(crime_ag$Borough) 
crime_ag <- rename(crime_ag, name = Borough)
lnd@data <- left_join(lnd@data, crime_ag)


library(tmap)
qtm(lnd, "CrimeCount") # plot the basic map
qtm(lnd, "Partic_Per")


############# Clipping data

library(rgdal)
# create new stations object using the "lnd-stns" shapefile.
stations <- readOGR(dsn = "map", layer = "lnd-stns")
proj4string(stations) # this is the full geographical detail.
proj4string(lnd) # what's the coordinate reference system (CRS)
bbox(stations) # the extent, 'bounding box' of stations
bbox(lnd) # return the bounding box of the lnd object

# Create reprojected stations object
stations27700 <- spTransform(stations, CRSobj = CRS(proj4string(lnd))) 
stations <- stations27700                         # overwrite the stations object
rm(stations27700)                                 # remove the stations27700 object to clear up
plot(lnd)                                         # plot London for context (see Figure 9)
points(stations)                                  # overlay the station points

# method 1
stations_backup <- stations                       # backup the stations object
stations <- stations_backup[lnd, ]
plot(stations)                                    # test the clip succeeded (see Figure 10)

#method 2
sel <- over(stations_backup, lnd)                 # target and source layers
stations2 <- stations_backup[!is.na(sel[,1]),]    # rows which are NOT na from the first column of sel

############# Spatial aggregation

stations_agg <- aggregate(x = stations["CODE"], by = lnd, FUN = length)
head(stations_agg@data)
names(stations@data)

lnd$n_points <- stations_agg$CODE

lnd_n <- aggregate(stations["NUMBER"], by = lnd, FUN = mean)
lnd_n@data

# plotting colours
brks <- quantile(lnd_n$NUMBER)
labs <- grey.colors(n = 4)
q <- cut(lnd_n$NUMBER, brks, labels = labs,
         include.lowest = T)
summary(q)                                       # check what we've created


# plotting map
qc <- as.character(q)                            # convert to character class to plot
plot(lnd_n, col = qc)                            # plot (not shown in printed tutorial)
legend(legend = paste0("Q", 1:4), fill = levels(q), "topright")
areas <- sapply(lnd_n@polygons, function(x) x@area)

plot(lnd_n$NUMBER, areas)

levels(stations$LEGEND)                            # see A roads and rapid transit stations (RTS) (not shown)
sel <- grepl("A Road Sing|Rapid", stations$LEGEND) # selection for plotting; A Road Sing or Rapid
sym <- as.integer(stations$LEGEND[sel])            # symbols
points(stations[sel,], pch = sym)                  # plot points
legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym)) # plot legend
 

# TUT8: TMAP, GGPLOT, LEAFLET ---------------------------------------------

## TMAP

vignette(package = "tmap") # available vignettes in tmap
vignette("tmap-nutshell")

# Create our first tmap map (not shown)
qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues")
qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = c("Blues"), ncol = 2)

lnd@data$Pop_2001 <- as.integer(lnd@data$Pop_2001)

tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords=TRUE, drop.shapes=TRUE) +
  tm_layout(legend.show = TRUE, title.position = c("center", "center"), title.size = 20)


## GGMAP

p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))
p + geom_point()
p + geom_point(aes(colour=Partic_Per, size=Pop_2001)) # not shown
p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) +
  geom_text(size = 2, aes(label = name))


library(rgeos)
lnd_f <- fortify(lnd)               # make as data.frame

lnd$id <- row.names(lnd)            # allocate an id variable to the sp data
head(lnd@data, n = 2)               # final check before join (requires shared variable name)

library(dplyr)
lnd_f <- left_join(lnd_f, lnd@data) # join the data

map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "% Sports\nParticipation") +
  ggtitle("London Sports Participation")

map + scale_fill_gradient(low = "white", high = "black")


## Leaflet

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)


london_data <- read.csv("datasets\\map\\census-historic-population-borough.csv")

library(tidyr)                                                           # if not install it, or skip the next two steps
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)
head(ltidy, 2)                                                           # check the output (not shown)
head(london_data, 2)


ltidy <- rename(ltidy, ons_label = Area.Code)                            # rename Area.code variable
lnd_f <- left_join(lnd_f, ltidy)                                         #
head(lnd_f, 2)                                                           #    

lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)       # replace the name values

ggplot(data = lnd_f,                                                     # the input data
       aes(x = long, y = lat, fill = pop/1000, group = group)) +         # define variables
  geom_polygon() +                                                       # plot the boroughs
  geom_path(colour="black", lwd=0.05) +                                  # borough borders
  coord_equal() +                                                        # fixed x and y scales
  facet_wrap(~ date) +                                                   # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red",         # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(),                                     # change the theme options
        axis.title = element_blank(),                                    # remove axis titles
        axis.ticks = element_blank())                                    # remove axis ticks


# http://docs.ggplot2.org/current/geom_map.html
# http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)

states_map <- map_data("state")
ggplot(crimes, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat)

last_plot() + coord_map()
ggplot(crimesm, aes(map_id = state)) +
  geom_map(aes(fill = value), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  facet_wrap( ~ variable)


# ddply = dataframe to dataframe
# dlply = dataframe to list (getting coefficients from model outputs)
# ldply = list to dataframe (fitting multiple models)



# More resources ------------------------------------------------------------

# https://www.datacamp.com/community/tutorials/15-questions-about-r-plots#gs.qZ4VmBA
# https://rpubs.com/chrisbrunsdon/99675
# https://pakillo.github.io/R-GIS-tutorial/
# http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization