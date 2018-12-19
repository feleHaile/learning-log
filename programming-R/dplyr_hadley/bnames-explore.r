library(plyr)

# transform: adding on extra columns
# mutate: new data frame
# summarise: adds on columns

setwd("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R\\datasets")
bnames <- read.csv("bnames.csv", stringsAsFactors = FALSE)
head(bnames)

# Whole dataset transformations ---------------------------------------------


# function to count the number of letters
letter <- function(x, n = 1) {
  if (n < 0) {
    nc <- nchar(x)                  
    n <- nc + n + 1
  }
  tolower(substr(x, n, n))
}

letter('eunice', -2)
letter('eunice', -1)

# function to count the number of vowels
vowels <- function(x) {
  nchar(gsub("[^aeiou]", "", x))
}

bnames <- transform(bnames,
  first = letter(name, 1),
  last = letter(name, -1),
  length = nchar(name),
  vowels = vowels(name)
)


# Whole dataset summaries ----------------------------------------------------

summarise(bnames, 
  max_perc = max(percent),
  min_perc = min(percent))

# Group-wise transformations  ------------------------------------------------

# Want to calculate rank of each name in each year (per sex).  This is easy if
# we have a single sex for a single year:
one <- subset(bnames, sex == "boy" & year == 2008)
one$rank <- rank(-one$percent, ties.method = "first")
# or
one <- transform(one, rank = rank(-percent, ties.method = "first")) # note: transform is a base package

##############
transform(airquality, Ozone = -Ozone)
transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)
#############

head(one)

# Conceptually if we want to perform this same task for every sex in every 
# year, we need to split up the data, apply the transformation to every piece
# and then join the pieces back together

# This is what ddply does
bnames <- ddply(bnames, c("sex", "year"), transform, 
  rank = rank(-percent, ties.method = "first"))

# rank variables from highest to lowest

# ddply basically works as follows:

# split
pieces <- split(bnames, list(bnames$sex, bnames$year))
# apply
results <- vector("list", length(pieces)) # create a vector
for(i in seq_along(pieces)) {
  piece <- pieces[[i]]
  piece <- transform(piece, rank = rank(-percent, ties.method = "first"))
  results[[i]] <- piece
}
# combine
result <- do.call("rbind", results)


# Group-wise summaries -------------------------------------------------------

# Group-wise summaries are much more interesting!

ddply(bnames, c("name"), summarise, tot = sum(percent))
ddply(bnames, c("length"), summarise, tot = sum(percent))
ddply(bnames, c("year", "sex"), summarise, tot = sum(percent))


fl <- ddply(bnames, c("year", "sex", "first"), summarise, tot = sum(percent))
library(ggplot2)
ggplot(fl, aes(x=year, y=tot, colour=sex)) +
          geom_line() +
          facet_wrap(~ first, ncol=5)


# Some R functions implement special cases of group-wise summaries.  For 
# example, when you are summing a single variable, you can use xtabs:
as.data.frame(xtabs(percent ~ name, data = bnames))
# These methods tend to be much faster, but they are harder to remember