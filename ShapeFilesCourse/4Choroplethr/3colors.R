sf = SFChoropleth$new(df)
sf$render() #has 7 colors by default

# 2 colors = above / below median
sf = SFChoropleth$new(df)
sf$set_num_colors(2) 
sf$render()

# continuous scale - 1 color 
sf = SFChoropleth$new(df)
sf$set_num_colors(1) #useful for seeing the outliers
sf$render()

# max - 9 colors
sf = SFChoropleth$new(df)
sf$set_num_colors(9) #does not add too much more information on tehi map but may with others
sf$render()
