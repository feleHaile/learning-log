## renaming rows

colnames(variable) <- "newname"

########################################

## merging tables based on one variable
# based on tutorial: http://stat545.com/bit001_dplyr-cheatsheet.html

# load library
library(dplyr)
library(readr)

# data frame 1
superheroes <- "
    name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
superheroes <- read_csv(superheroes, trim_ws = TRUE, skip = 1)
superheroes <- as.data.frame(superheroes)

# data frame 2

publishers <- "
publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"
publishers <- read_csv(publishers, trim_ws = TRUE, skip = 1)
publishers <- as.data.frame(publishers)

# a quick view of the data frame
head(superheroes)
head(publishers)

superheroes
publishers

# inner join: match values from y, all columns x and y; if there are duplicates of x-y matches, return all. mutating join.

ij <- inner_join(superheroes, publishers, by="publisher")
print(ij)

# semi join: match values from y, only columns from x. no duplicates. (filtering join)

sj <- semi_join(superheroes, publishers, by="publisher")
print(sj)

# left join: all values from x. all matches are returned (mutating join)

lj <- left_join(superheroes, publishers, by="publisher")
print(lj)

# anti join: return x which are not matching in y, retain x columns. filtering

aj <- anti_join(superheroes, publishers, by="publisher")
print(aj)

# full join: return all rows and all columns of x and y even though no matches

fj <- full_join(superheroes, publishers, by="publisher")
print(fj)

########################################

## long to wide variable
# based on tutorial: https://tgmstat.wordpress.com/2013/10/31/reshape-and-aggregate-data-with-the-r-package-reshape2/

# load library
library(reshape2) # main library needed in this tutorial
library(plyr) # needed to access . function

# load data
data(airquality)
head(airquality)

# melt data, making it into long format
melted <- melt(data=airquality, id=c("Month", "Day"), na.rm=TRUE)
head(melted)

# cast data, making it back into long format
acast(data = melted, formula = Month ~ variable, fun.aggregate =mean, 
      subset = .(variable == "Ozone"))