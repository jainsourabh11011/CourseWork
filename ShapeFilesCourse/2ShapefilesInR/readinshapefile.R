library(rgdal)

# setwd("~/Desktop/Supervisorial Districts as of April 2012/")
# sf = readOGR(dsn=".", layer="geo_export_167e4320-87af-4efb-904f-3eee8b9c2239")
sf = readOGR(dsn="./shapefiles", layer="geo_export_9eebc173-0cec-43b0-9ade-db4b507f6cb2")

class(sf)

plot(sf)

# Link for the book "Applied Spatial Data Analysis with R"
# http://www.amazon.com/Applied-Spatial-Data-Analysis-Use/dp/1461476178