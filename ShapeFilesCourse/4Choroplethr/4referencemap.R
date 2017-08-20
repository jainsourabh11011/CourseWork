sf = SFChoropleth$new(df)
sf$render()

# "reference map" = "google map"
sf$render_with_reference_map()#the warnings can be safely ignored.

# create the image from the course home page
sf = SFChoropleth$new(df)
sf$set_num_colors(1) # continuous scale
sf$title  = "San Francisco Noise Reports by Supervisor District"
sf$legend = "Number of Noise Reports"
sf$render_with_reference_map()
