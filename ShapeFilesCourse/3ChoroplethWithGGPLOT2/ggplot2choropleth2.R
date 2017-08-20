sf = readOGR(dsn="./shapefiles", layer="geo_export_9eebc173-0cec-43b0-9ade-db4b507f6cb2")

ggplot(final.df, aes(long, lat, group=group, fill=n)) + geom_polygon() 

# inverting scale - light to dark. This is the convention in choropleth maps
ggplot(final.df, aes(long, lat, group=group, fill=n)) + geom_polygon() + scale_fill_continuous(low="#eff3ff", high="#084594") 

# remove the background
# From "Making a Map with a Clean Background" of "R Graphics Cookbook" by Winston Chang.  
theme_clean = function()
{
  theme(
    axis.title        = element_blank(),
    axis.text         = element_blank(),
    panel.background  = element_blank(),
    panel.grid        = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    panel.margin      = unit(0, "lines"),
    plot.margin       = unit(c(0, 0, 0, 0), "lines"),
    complete          = TRUE
  )
}

ggplot(final.df, aes(long, lat, group=group, fill=n)) + geom_polygon() + scale_fill_continuous(low="#eff3ff", high="#084594") +
  theme_clean()

# add mercator projection
library(mapproj)
ggplot(final.df, aes(long, lat, group=group, fill=n)) + geom_polygon() + scale_fill_continuous(low="#eff3ff", high="#084594") +
  theme_clean() + coord_map()

# add title
ggplot(final.df, aes(long, lat, group=group, fill=n)) + geom_polygon() + scale_fill_continuous(low="#eff3ff", high="#084594") +
  theme_clean() +  coord_map() + ggtitle("San Francisco Noise Complaints by Supervisor District")

# add legend name
ggplot(final.df, aes(long, lat, group=group, fill=n)) + geom_polygon() + scale_fill_continuous(name="Number of Complaints", 
     low="#eff3ff", high="#084594") + theme_clean() + coord_map() + ggtitle("San Francisco Noise Complaints by Supervisor District")
