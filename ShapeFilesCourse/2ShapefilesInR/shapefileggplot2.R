library(rgdal)

# Step 1: read in shapefile and render using rgdal package
sf = readOGR(dsn="./shapefiles", layer="geo_export_9eebc173-0cec-43b0-9ade-db4b507f6cb2")

plot(sf)

##Boilerplate Code Begin##

# Step 2: convert shapefile to a data frame that ggplot2 can render
# To learn more: https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
library(gpclib)
library(maptools)
library(ggplot2)
library(dplyr)

gpclibPermit()

sf@data$id = rownames(sf@data)
sf.points = fortify(sf, region="id")
sf.df = inner_join(sf.points, sf@data, by="id")

##Boilerplate Code End##

# Compare these columns to what you see when you inspect regions with QGIS
head(sf.df)

# Step 3: render the data frame with ggplot!
# To learn more about ggplot2, read "R Graphics Cookbook" by Winston Chang?
# http://www.amazon.com/R-Graphics-Cookbook-Winston-Chang/dp/1449316956
ggplot(sf.df) + aes(long,lat,group=group) + geom_polygon(color="white")
