# create map with choroplethr object

# step 1: data needs a column named "region" and a column named "value"
head(df)
colnames(df) = c("region", "value")
head(df)

# step 2: create a new object
# notice the warning
sf = SFChoropleth$new(df)
#Gives warning because there are Supervisor districts in the data that do not make sense:  0

# step 3: call the "render" function
# note the inverted scale, clear background and projection
sf$render() 

# add a title and legend
sf$title  = "San Francisco Noise Reports by Supervisor District"
sf$legend = "Number of Noise Reports"
sf$render() 
