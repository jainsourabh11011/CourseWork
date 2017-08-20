R.Version() # mine is 3.2.3

update.packages()

packages = c("gpclib",
             "rgdal",
             "maptools",
             "dplyr",
             "ggplot2")

packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages) > 0) 
{
  install.packages(packages)
}