# convert to date [date and time included]
date <- "2016-09-03"
date <- as.POSIXct(date)

# convert to date [dates only]
date2 <- "2 Jan 2016"
date2 <- as.Date(date, format="%d %b %Y")

# %m or %M month in numerics
# %b or %B month in alphabet
# %d day
# %y or %Y year

# convert date partially
mon.year <- strftime(date, "%b-%Y")  


library(lubridate)   # for reading dates


# lubridate's as_date
dt_utc <- ymd_hms("2010-08-03 00:50:50")
dt_europe <- ymd_hms("2010-08-03 00:50:50", tz="Europe/London")
c(as_date(dt_utc), as.Date(dt_utc))
c(as_date(dt_europe), as.Date(dt_europe))
