## renaming rows
colnames(variable) <- "newname"

########################################

# dplyr: joins and using piping (%)
# plyr: summarize, transform, etc. (through ddply)
# reshape: melt and cast

########################################

# dplyr - joins -----------------------------------------------------------

## merging tables based on one variable
# based on tutorial: http://stat545.com/bit001_dplyr-cheatsheet.html

# load library
library(dplyr) # library for data manipulation
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

# dplyr - piping ----------------------------------------------------------

setwd('C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R\\datasets')
library(dplyr)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)
names(msleep)

# select() - similar to excel filter
# filter() - similar to excel filter
# arrange() - similar to excel filter
# mutate() - similar to formula
# summarise() - similar to pivot
# group_by()

# select

## using the select function
sleepData <- select(msleep, name, sleep_total)
head(sleepData)

## select all the columns except this one
head(select(msleep, -name))

## selecting a range of columns
head(select(msleep, name:order))

## select based on a particular string
head(select(msleep, starts_with("sl")))

#starts_with, ends_with, contains, matches
#num_range, one_of, everything
#current_vars()

# filter
filter(msleep, sleep_total >= 16)
filter(msleep, sleep_total >= 16, bodywt >= 1)
filter(msleep, order %in% c("Perissodactyla", "Primates"))

# piping function
msleep %>% 
  select(name, sleep_total) %>% 
  head

# arrange
msleep %>% arrange(order) %>% head

# select, arrange

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  head

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, -sleep_total) %>% 
  filter(sleep_total >= 16)

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>% 
  filter(sleep_total >= 16)

# mutate
msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total, 
         bodywt_grams = bodywt * 1000) %>%
  head

# summarize
msleep %>% 
  summarise(avg_sleep = mean(sleep_total))

msleep %>% 
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())

# using pivot
msleep %>% 
  group_by(order) %>%  #pivot table columns
  summarise(avg_sleep = mean(sleep_total), # pivot table rows
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())

head(msleep)


########### EXERCISE
msleep %>% 
  filter(order == 'Primates') %>%
  select(sleep_total) %>%
  mutate(avg_sleep = mean(sleep_total))

msleep %>% 
  group_by(order) %>%
  summarize(avg_sleep = mean(sleep_total))

########################################

# reshape -----------------------------------------------------------------


## long to wide variable
# based on tutorial: https://tgmstat.wordpress.com/2013/10/31/reshape-and-aggregate-data-with-the-r-package-reshape2/

# load library
library(reshape2) # main library needed in this tutorial
library(plyr) # needed to access . function

# load dat-a
data(airquality)
head(airquality)

# melt data, making it into long format
melted <- melt(data=airquality, id=c("Month", "Day"), na.rm=TRUE)
head(melted)

# cast data, making it back into long format
acast(data = melted, formula = Month ~ variable, fun.aggregate =mean, 
      subset = .(variable == "Ozone"))

########################################

# ddply -------------------------------------------------------------------

## ddply 
# http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
# https://www.rdocumentation.org/packages/plyr/versions/1.8.4/topics/ddply


data <- read.table(header=TRUE, text='
 subject sex condition before after change
                   1   F   placebo   10.1   6.9   -3.2
                   2   F   placebo    6.3   4.2   -2.1
                   3   M   aspirin   12.4   6.3   -6.1
                   4   F   placebo    8.1   6.1   -2.0
                   5   M   aspirin   15.2   9.9   -5.3
                   6   F   aspirin   10.9   7.0   -3.9
                   7   F   aspirin   11.6   8.5   -3.1
                   8   M   aspirin    9.5   3.0   -6.5
                   9   F   placebo   11.5   9.0   -2.5
                   10   M   placebo   11.9  11.0   -0.9
                   11   F   aspirin   11.4   8.0   -3.4
                   12   M   aspirin   10.0   4.4   -5.6
                   13   M   aspirin   12.5   5.4   -7.1
                   14   M   placebo   10.6  10.6    0.0
                   15   M   aspirin    9.1   4.3   -4.8
                   16   F   placebo   12.1  10.2   -1.9
                   17   F   placebo   11.0   8.8   -2.2
                   18   F   placebo   11.9  10.2   -1.7
                   19   M   aspirin    9.1   3.6   -5.5
                   20   M   placebo   13.5  12.4   -1.1
                   21   M   aspirin   12.0   7.5   -4.5
                   22   F   placebo    9.1   7.6   -1.5
                   23   M   placebo    9.9   8.0   -1.9
                   24   F   placebo    7.6   5.2   -2.4
                   25   F   placebo   11.8   9.7   -2.1
                   26   F   placebo   11.8  10.7   -1.1
                   27   F   aspirin   10.1   7.9   -2.2
                   28   M   aspirin   11.6   8.3   -3.3
                   29   F   aspirin   11.3   6.8   -4.5
                   30   F   placebo   10.3   8.3   -2.0
                   ')

library(plyr)

cdata <- ddply(.data = data, 
               .variables = c("sex", "condition"), 
               .fun = summarise,
               N    = length(change),
               mean = mean(change),
               sd   = sd(change),
               se   = sd / sqrt(N)
)
cdata

## handling missing data

dataNA <- data
dataNA$change[11:14] <- NA

cdata <- ddply(.data = dataNA, 
               .variables = c("sex", "condition"), 
               .fun = summarise,
               N    = sum(!is.na(change)),
               mean = mean(change, na.rm=TRUE),
               sd   = sd(change, na.rm=TRUE),
               se   = sd / sqrt(N)
)
cdata





# make new variable career_year based on the
# start year for each player (id)

names(baseball)

base2 <- ddply(baseball, .(id), mutate,
               career_year = year - min(year) + 1
)

# ddply by hadley ---------------------------------------------------------

# http://plyr.had.co.nz/09-us
# see the other tutorial folder

# # tidyr -----------------------------------------------------------------
??tidyr
library(tidyr)


