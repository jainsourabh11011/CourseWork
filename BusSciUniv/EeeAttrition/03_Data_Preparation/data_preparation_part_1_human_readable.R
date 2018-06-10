# DATA PREPARATION ----

# Human Readable ----

pkgs <- c(
     "tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
     "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
     "stringr", 
     "forcats",   # Categorical data
     "readxl"     # Reading excel files
)

if(!require(easypackages)){install.packages("easypackages")}
library(easypackages)
packages(pkgs, prompt = FALSE)

# Load Data
path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Tidying the Data ----
#  Want to replace numbers with values for some variables in - 
definitions_raw_tbl

# Below, fill replaces missing values (NAs) with the closest entry - very cool!
definitions_raw_tbl %>% fill(X__1, .direction = "down")

# Filter NAs in 2nd column
definitions_raw_tbl %>% fill(X__1, .direction = "down") %>% filter(!is.na(X__2))

# Separate info in column 2 using separate from tidyr
definitions_raw_tbl %>% fill(X__1, .direction = "down") %>% filter(!is.na(X__2)) %>% 
     separate(X__2, into = c("key", "value"), sep =  " '", remove = TRUE)
     
# Make better col names and remove last tick mark on col 3
definitions_raw_tbl %>% fill(X__1, .direction = "down") %>% filter(!is.na(X__2)) %>% 
     separate(X__2, into = c("key", "value"), sep =  " '", remove = TRUE) %>% 
     rename(column_name = X__1) %>% 
     mutate(key = as.numeric(key)) %>% 
     mutate(value = value %>% str_replace(pattern = "'", replacement = ""))

# Save work
definitions_tbl <- definitions_raw_tbl %>% fill(X__1, .direction = "down") %>% 
     filter(!is.na(X__2)) %>% 
     separate(X__2, into = c("key", "value"), sep =  " '", remove = TRUE) %>% 
     rename(column_name = X__1) %>% 
     mutate(key = as.numeric(key)) %>% 
     mutate(value = value %>% str_replace(pattern = "'", replacement = ""))

# Break the definitions tbl into a list of dfs - Mapping Over Lists
definitions_list <- definitions_tbl %>% 
     split(.$column_name) %>% #split df into multiple dfs within a list
     map(~ select(., -column_name)) %>% # remove column_name col leaving key and value
     map(~ mutate(., value = as_factor(value)))# Creates factors in the order they appear - not alphabetically like as.factor

definitions_list[[1]] # or
definitions_list[["Education"]]

for(i in seq_along(definitions_list)){
     list_name <- names(definitions_list)[i]
     colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
     # I did not know colnames worked with lists!
}

# Join list with training data

list(HRData = train_raw_tbl) %>% append(definitions_list, after = 1)#everything in 1 list
# Use lists to collect objects that need to be iterated over using purrr functions

list(HRData = train_raw_tbl) %>% append(definitions_list, after = 1) %>% 
     reduce(left_join) %>% glimpse()# reduce iterates over the list and applies a function

list(HRData = train_raw_tbl) %>% append(definitions_list, after = 1) %>% 
     reduce(left_join) %>% 
     select(-one_of(names(definitions_list)))# Remove the variables that are not human understandable

list(HRData = train_raw_tbl) %>% append(definitions_list, after = 1) %>% 
     reduce(left_join) %>% 
     select(-one_of(names(definitions_list))) %>% 
     set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>% 
     select(sort(names(.)))# change column names and sort columns alphabetically

data_merged_tbl <- list(HRData = train_raw_tbl) %>% append(definitions_list, after = 1) %>% 
     reduce(left_join) %>% 
     select(-one_of(names(definitions_list))) %>% 
     set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>% 
     select(sort(names(.)))

glimpse(data_merged_tbl)

# Factoring Character Data

data_merged_tbl %>% select_if(is.character) %>%  glimpse()

data_merged_tbl %>% distinct(BusinessTravel)#Order is needed

# Char to factor
data_merged_tbl %>% mutate_if(is.character, as.factor) 

# Reorder some factors
data_merged_tbl %>% mutate_if(is.character, as.factor) %>%  select_if(is.factor)%>% 
     map(levels)

# Business Travel
data_merged_tbl %>% mutate_if(is.character, as.factor) %>%
     mutate(BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
            MaritalStatus = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced"))

data_processed_tbl <- data_merged_tbl %>% mutate_if(is.character, as.factor) %>%
     mutate(BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
            MaritalStatus = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced"))

data_processed_tbl %>% select_if(is.factor) %>% map(levels)


# Processing Pipeline
process_hr_data_readable <- function(data, definitions_tbl){
     
     definitions_list <- definitions_tbl %>% fill(X__1, .direction = "down") %>% 
          filter(!is.na(X__2)) %>% 
          separate(X__2, into = c("key", "value"), sep =  " '", remove = TRUE) %>% 
          rename(column_name = X__1) %>% 
          mutate(key = as.numeric(key)) %>% 
          mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>% 
          split(.$column_name) %>% 
          map(~ select(., -column_name)) %>% 
          map(~ mutate(., value = as_factor(value)))
     
     for(i in seq_along(definitions_list)){
          list_name <- names(definitions_list)[i]
          colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value")) }
     
     data_merged_tbl <- list(HRData = data) %>% append(definitions_list, after = 1) %>% 
          reduce(left_join) %>% 
          select(-one_of(names(definitions_list))) %>% 
          set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>% 
          select(sort(names(.))) %>% 
          mutate_if(is.character, as.factor) %>%
          mutate(BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
                 MaritalStatus = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced"))
     
     return(data_merged_tbl)
}

# Test Pipeline

process_hr_data_readable(train_raw_tbl, definitions_raw_tbl) %>% glimpse()

source("00_Scripts/data_processing_pipeline.R")

train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)

#Education
#  Before
train_raw_tbl %>% ggplot(aes(Education)) + geom_bar()

# After
train_readable_tbl %>% ggplot(aes(Education)) + geom_bar()

# Business Travel
#  Before
train_raw_tbl %>% ggplot(aes(BusinessTravel)) + geom_bar()

# After
train_readable_tbl %>% ggplot(aes(BusinessTravel)) + geom_bar()
