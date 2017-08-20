# To get the 311 data as a csv visit 
# https://data.sfgov.org/ -> infrastructure

#install.packages("readr")
library(readr)

df = read_csv("./data/Case_Data_from_San_Francisco_311__SF311_.csv")
head(df)

colnames(df)
unique(df$Category)
unique(df$"Supervisor District")

# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
library(dplyr)
df = df %>% select(`Supervisor District`, Category) %>% filter(Category == "Noise Report") %>%
  group_by(`Supervisor District`) %>% summarise(n = n())

df
