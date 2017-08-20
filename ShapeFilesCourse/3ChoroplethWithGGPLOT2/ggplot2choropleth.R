# For reference, here is code from the previous lessons
# This shows you how to load in the shapefile and convert it to a data frame,
# which is a prerequisite for working with it in ggplot2

# Step 1: read in shapefile and render using readOGR
library(rgdal)
# setwd("~/Desktop/Supervisorial Districts as of April 2012/")
# sf = readOGR(dsn=".", layer="geo_export_167e4320-87af-4efb-904f-3eee8b9c2239")
sf = readOGR(dsn="./shapefiles", layer="geo_export_9eebc173-0cec-43b0-9ade-db4b507f6cb2")

# Step 2: convert shapefile to a data frame that ggplot2 can render
library(gpclib)
library(maptools)
library(ggplot2)
library(dplyr)

gpclibPermit()

sf@data$id = rownames(sf@data)
sf.points = fortify(sf, region="id")
sf.df = inner_join(sf.points, sf@data, by="id")

# Step 3: render the shapefiles / data frame with ggplot2
ggplot(sf.df, aes(long,lat,group=group)) + geom_polygon()

# --------------------------------

head(sf.df) # map / shapefile
head(df)    # data 

#library(dplyr)
?inner_join 
final.df = inner_join(sf.df, df, by=c("supervisor" = "Supervisor District"))
head(final.df)

ggplot(final.df, aes(long, lat, group=group, fill=n)) + geom_polygon() 
