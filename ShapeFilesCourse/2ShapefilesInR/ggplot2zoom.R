ggplot(sf.df) + aes(long,lat,group=group) + geom_polygon()

head(sf.df)

# zoom / subset
district3 = sf.df[sf.df$supervisor == 3, ] #pretty easy , just subsetting the DF
ggplot(district3) + aes(long,lat,group=group) + geom_polygon()

