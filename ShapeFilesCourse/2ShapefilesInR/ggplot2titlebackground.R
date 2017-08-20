# basic plot
ggplot(sf.df) + aes(long,lat,group=group) + geom_polygon()

# add a title
ggplot(sf.df) + aes(long,lat,group=group) + geom_polygon() + ggtitle("San Francisco Supervisor Districts")

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

ggplot(sf.df) + aes(long,lat,group=group) + geom_polygon() + ggtitle("San Francisco Supervisor Districts") + theme_clean()  
