# default map
sf = SFChoropleth$new(df)
sf$render()

# review: what are the regions?
head(sf.df)
unique(sf.df$region)

sf = SFChoropleth$new(df)
sf$set_zoom(c(1,2,3)) # the first 3 supervisor districts
sf$render()

sf = SFChoropleth$new(df)
sf$set_zoom(c(1,2,3)) # the first 3 supervisor districts 
sf$set_num_colors(1) # continuous scale
sf$render()

sf$render_with_reference_map()
