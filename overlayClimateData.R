

library(rWBclimate)
library(taxize)
library(spocc)
library(plyr)

### Create path to store kml's
dir.create("~/kmltmp")
options(kmlpath = "~/kmltmp")
options(stringsAsFactors = FALSE)

usmex <- c(273:284, 328:365)
### Download KML's and read them in.
usmex.basin <- create_map_df(usmex)

## Download temperature data
temp.dat <- get_historical_temp(usmex, "decade")
temp.dat <- subset(temp.dat, temp.dat$year == 2000)

# Bind temperature data to map data frame
usmex.map.df <- climate_map(usmex.basin, temp.dat, return_map = F)

## Grab some species occurrence data for the 8 tree species.

splist <- c("Acer saccharum", "Abies balsamea", "Arbutus xalapensis", 
            "Betula alleghaniensis", "Chilopsis linearis", 
            "Conocarpus erectus", "Populus tremuloides", "Larix laricina")

## get data from bison and gbif
splist <- sort(splist)
out <- occ(query = splist, from = c("bison", "gbif"), limit = 100)


## scrub names
out <- fixnames(out, how = "query")


## Create a data frame of all data.
out_df <- occ2df(out)


### grab common names
cname <- ldply(sci2comm(get_tsn(splist), db = "itis", simplify = TRUE), 
               function(x) {
  return(x[1])
})[, 2]




### Now let's create a vector of common names for easy plotting But first
### order on names so we can just add the names
out_df <- out_df[order(out_df$name), ]
### strip NA values and 0 values of coordinates
out_df <- out_df[!is.na(out_df$lat), ]
out_df <- out_df[out_df$lat > 0, ]
out_df$common <- rep(cname, table(out_df$name))


## Now just create the base temperature map
usmex.map <- ggplot() + geom_polygon(data = usmex.map.df, aes(x = long, y = lat, 
                                                              group = group, fill = data, alpha = 0.9)) + 
  scale_fill_continuous("Average annual \n temp: 1990-2000", low = "yellow", 
                        high = "red") + guides(alpha = F) + theme_bw(10)

  