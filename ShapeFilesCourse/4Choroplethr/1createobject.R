# create the choroplethr "object"

# step 1: map needs a column named "region" for cohorpletherR to work (everything is a region or a value)
head(sf.df)
sf.df$region = sf.df$supervisor
head(sf.df)

# step 2: create the R6 class - can copy and paste and just run ot
library(R6)
#The SF prefix could be anything appropriate for the map you want to make
SFChoropleth = R6Class("SFChoropleth",
  inherit = choroplethr:::Choropleth,
  public = list(
    
    # initialize with a map of San Francisco Supervisor Districts
    initialize = function(user.df)
    {
      super$initialize(sf.df, user.df)
    }
  )
)

# how to learn more about object oriented programming
# 1. My article: "Object Oriented Choropleths: 
# https://github.com/arilamstein/choroplethr/wiki/Object-Oriented-Choropleths

