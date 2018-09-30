#####USE THIS CODE IF NEED TO UPDATE H2O################

# # The following two commands remove any previously installed H2O packages for R.
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {if (! (pkg %in% rownames(installed.packages()))) {install.packages(pkg)}}
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wright/2/R")
# NOTE:  Need to match R Version and AWS Repo Version h2o/rel-wright/2/R" - the 2 makes a difference

##########################################################

pkgs <- c(
    "h2o",        # High performance machine learning
    "lime",       # Explaining black-box models
    "recipes",    # Creating ML preprocessing recipes
    "tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
    "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
    "glue",       # Pasting text
    "cowplot",    # Handling multiple ggplots
    "GGally",     # Data understanding - visualizations
    "skimr",      # Data understanding - summary information
    "fs",         # Working with the file system - directory structure
    "readxl",     # Reading excel files
    "writexl"     # Writing to excel files
)


if(!require(easypackages)){install.packages("easypackages")}
library(easypackages)
packages(pkgs, prompt = FALSE)
