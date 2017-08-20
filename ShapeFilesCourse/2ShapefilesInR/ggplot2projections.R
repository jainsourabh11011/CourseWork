# map projections
# to learn more about projections: https://en.wikipedia.org/wiki/Map_projection
packages = c("mapproj", "choroplethrMaps", "gridExtra")
packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages) > 0) 
{
  install.packages(packages)
}

library(mapproj)
library(choroplethrMaps)
library(gridExtra)

?coord_map#ggplot2 defaults to Mercator Projection

data(state.map)#choroplethrMaps

default = ggplot(state.map) + aes(long,lat,group=group) + geom_polygon() + ggtitle("Default Projection")

mercator = ggplot(state.map) + aes(long,lat,group=group) + geom_polygon() + coord_map(projection = "mercator") + 
     ggtitle("Mercator Projection")

grid.arrange(default, mercator, ncol=2)
