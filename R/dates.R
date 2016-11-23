library(lubridate)   # for reading dates

# convert to date
date <- as.POSIXct(date)

# convert to date 2

date <- as.Date(date)

# convert date partially

csvConverted$mmyyofde <- strftime(csvConverted$dateofde, "%m-%Y")  