# For reference, here is code from the previous lessons
# This shows you how to load in the shapefile and convert it to a data frame,

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

# Step 3: verify that the map can be rendered
ggplot(sf.df, aes(long,lat,group=group)) + geom_polygon()

# --------------------------------

# read in the data
library(readr)
df = read_csv("./data/Case_Data_from_San_Francisco_311__SF311_.csv")

df = df %>% select(`Supervisor District`, Category) %>% filter(Category == "Noise Report") %>%
  group_by(`Supervisor District`) %>% summarise(n = n())

# verify that the data looks right
df
