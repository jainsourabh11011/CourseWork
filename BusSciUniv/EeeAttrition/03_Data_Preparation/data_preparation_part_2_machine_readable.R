# DATA PREPARATION ----

# Machine Readable ----

# Setup ----
setwd("~/GitHub/CourseWork/BusSciUniv/EeeAttrition")
pkgs <- c(
     "recipes", 
     "tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
     "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
     "readxl"     # Reading excel files
)

if(!require(easypackages)){install.packages("easypackages")}
library(easypackages)
packages(pkgs, prompt = FALSE)

# Load Data
path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# Plot Faceted Histogram Function ----

plot_hist_facet <- function(data, bins = 10, ncol = 5, fct_reorder = FALSE, fct_rev = FALSE,
                            fill = palette()[[3]], color = "white", scale = "free"){
# palette()[[3]] is just a hex code "18BC9C"     
     data_factored <- data %>% 
          mutate_if(is.character, as.factor) %>% 
          mutate_if(is.factor, as.numeric) %>% 
          gather(key = key, value = value, factor_key = TRUE)# recall gather does wide df to long df
     
     if(fct_reorder){
          data_factored <- data_factored %>% 
               mutate(key = as.character(key) %>% as.factor())}#using as.character then as.factor arranges factors alphabetically
     
     if(fct_rev){
          data_factored <- data_factored %>% 
               mutate(key = fct_rev(key))}
     
     g <- data_factored %>% 
          ggplot(aes(x = value, group = key)) +
          geom_histogram(bins = bins, fill = fill, color = color) +
          facet_wrap(~ key, ncol = ncol, scale = scale)+ theme_tq()
                
     return(g)
}

# Test it
train_raw_tbl %>% plot_hist_facet(bins = 10, ncol =5, fct_rev = T)

train_raw_tbl %>% 
     select(Attrition, everything()) %>% #Make Attrition first then everything else
     plot_hist_facet(bins = 10, ncol =5, fct_rev = F)

# Data Preprocessing With Recipes ----
#  See https://topepo.github.io/recipes/reference/index.html

# Plan:  Correlation Analysis
#  Always do correlation analysis befor emodeling
#  Impute/Zero Variance ----
#   Imputation:  The act of filling in missing values within features.  
#   Common methods are:  
#    filling in by recency - tidyr:  fill
#    filling by similarity - knn impute

# We do not have missing vaues in the current data.
# However there are features with no variance

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl)
# The ~ separates the outcome from predictors.  The . is R shorthand to select all remaining variables
recipe_obj %>% step_zv(all_predictors()) %>% prep()
# NOTE:  prep() does not transform the data - it just determines is is needed to transform
#        bake() performs transformation in new data (even if it is the same data you have already used)
recipe_obj %>% step_zv(all_predictors()) %>% prep() %>% bake(newdata = train_readable_tbl)

#  Transformations ----
#   Changes the data to remove skew (log), stabilize variance (Box Cox) or make stationary (difference for time series)
#   Normality is required for linear models that depend on correlation (correlation analysis, linear and logistic regression)
#   Non-linear models like random forest handle non-linear data because they depend on how the data can be segregated

# Filter skewed features
train_readable_tbl %>% select_if(is.numeric) %>% 
     map_df(skewness) %>% gather(factor_key = TRUE) %>% 
     arrange(desc(value))
# NOTE:  high value --> fat tail on left, very low --> fat tail on right

# Pick a cutoff value - evalaute the values from above and the plot
#  Appears to be a big difference between:  
#   YearsWithCurrManager       0.802  
#   TrainingTimesLastYear      0.55
#  The plots kind show this too - TrainingTimesLastYear does not look too skewed

skewed_feature_names <- train_readable_tbl %>% select_if(is.numeric) %>% 
     map_df(skewness) %>% gather(factor_key = TRUE) %>% 
     arrange(desc(value)) %>% 
     filter(value >= 0.8) %>% 
     pull(key) %>% as.character()

train_readable_tbl %>% select(skewed_feature_names) %>% plot_hist_facet()
# NOTE:  2 variables that are actually factors persisted - JobLevel and StockOptionLevel

factor_names <- c("JobLevel", "StockOptionLevel")

skewed_feature_names <- train_readable_tbl %>% select_if(is.numeric) %>% 
     map_df(skewness) %>% gather(factor_key = TRUE) %>% 
     arrange(desc(value)) %>% 
     filter(value >= 0.8) %>% 
     filter(!key %in% factor_names) %>% 
     pull(key) %>% as.character()

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
     step_zv(all_predictors()) %>% 
     step_YeoJohnson(skewed_feature_names) %>% 
     step_num2factor(factor_names)
# step_YeoJohnson - power transformation.  Instead of sqrt, looks for diff roots b/t -5 to 5

#See the results
recipe_obj %>% prep() %>% bake(train_readable_tbl) %>% 
     select(skewed_feature_names) %>% 
     plot_hist_facet()
# Skew is gone!

#  Discretize 
#   Make a continuous varaible discrete (buckets of values)
#   Discretation can hurt correlations.  Often best not to discretize unless there is a specific need to do so
#    Example - explaining the difference between millenials and Gen-X

#  Center/Scaling ----
#   Algos that require scaling include kmeans, Deep Learning, PCA, SVMs
#   Scaling does not tpyically hurt algos - so do it

train_readable_tbl %>% select_if(is.numeric) %>% plot_hist_facet()
# Note the x axis values are different from one and other

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
     step_zv(all_predictors()) %>% 
     step_YeoJohnson(skewed_feature_names) %>% 
     step_num2factor(factor_names) %>% 
     step_center(all_numeric()) %>% #MUST center before scale (C comes before S)
     # centering subtracts out the means
     step_scale(all_numeric())# ensure the same range X values

recipe_obj$steps[[4]]# before prep

prepared_recipe <- recipe_obj %>% prep()#Note:  recipe output is a list

prepared_recipe$steps[[4]]# after prep

prepared_recipe %>% bake(newdata = train_readable_tbl) %>% 
     select_if(is.numeric) %>% 
     plot_hist_facet()

#  Dummy Variables ----
#   One hot encoding - categorical varibles into separate columns of 1/0.  Important to detect patterns in unordered data
#  Interaction Variables / Engineered Features
#   Some advanced algos like deep learning can detect interactions automatically.  However,
#   it is often difficult to tell what the interation is which may not help teh business
#   case but will improve model accuracy

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
     step_zv(all_predictors()) %>% 
     step_YeoJohnson(skewed_feature_names) %>% 
     step_num2factor(factor_names) %>% 
     step_center(all_numeric()) %>% 
     step_scale(all_numeric())

dummied_recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
     step_zv(all_predictors()) %>% 
     step_YeoJohnson(skewed_feature_names) %>% 
     step_num2factor(factor_names) %>% 
     step_center(all_numeric()) %>%
     step_scale(all_numeric()) %>% 
     step_dummy(all_nominal())# selects char and factor variables

dummied_recipe_obj %>% prep() %>% bake(newdata = train_readable_tbl) %>% 
     select(contains("JobRole")) %>% 
     plot_hist_facet(ncol = 3)

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
     step_zv(all_predictors()) %>% 
     step_YeoJohnson(skewed_feature_names) %>% 
     step_num2factor(factor_names) %>% 
     step_center(all_numeric()) %>%
     step_scale(all_numeric()) %>% 
     step_dummy(all_nominal())

#  Multivariate Transformations
#   Includes PCA which is a dimensionalty reduction technoque.  Useful with very wide data

# Final Recipe ----

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
     step_zv(all_predictors()) %>% 
     step_YeoJohnson(skewed_feature_names) %>% 
     step_num2factor(factor_names) %>% 
     step_center(all_numeric()) %>%
     step_scale(all_numeric()) %>% 
     step_dummy(all_nominal()) %>% 
     prep()

train_tbl <- bake(recipe_obj, newdata = train_readable_tbl)

train_tbl %>% glimpse()    

test_tbl <- bake(recipe_obj, newdata = test_readable_tbl)

# Correlation Analysis ----

# pairwise.complete.obs is not the default is what you nealy always want to use.
# use the default - "everything" - risk of getting missing value errors

# correlation analysis only identifies linear relationships - if there is an exponential relationship
# a random forest will find the relationship

# Correlation is a good barometer but not necessarily a definitve answer - it is a guide

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE){
     
     feature_expr <- enquo(target)
     feature_name <- quo_name(feature_expr)
     
     data_cor <- data %>%
          mutate_if(is.character, as.factor) %>% 
          mutate_if(is.factor, as.numeric) %>% #correlation analysis only works with numeric data
          cor(use = use) %>% 
          as.tibble() %>% 
          mutate(feature = names(.)) %>% 
          select(feature, !! feature_expr) %>% 
          filter(!(feature == feature_name)) %>% # Remove Attrition record
          mutate_if(is.character, as_factor)
     
     if(fct_reorder){
          data_cor <- data_cor %>% 
               mutate(feature = fct_reorder(feature, !! feature_expr)) %>% 
               #fct_reorder resorders factor by another column
               #It changes the levels of a factor but not the DF - use arrange for taht
               arrange(feature)}
     
     if(fct_rev){
          data_cor <- data_cor %>% 
               mutate(feature = fct_rev(feature)) %>% 
               arrange(feature)}
     
     return(data_cor)
}

train_tbl %>% get_cor(Attrition_Yes, fct_reorder = T, fct_rev = T)

# Plot Correlations ----

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], color_neg = palette_light()[[2]]) {
     
     feature_expr <- enquo(target)
     feature_name <- quo_name(feature_expr)
     
     data_cor <- data %>%
          get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
          mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
          mutate(Correlation = case_when(
               (!! feature_expr) >= 0 ~ "Positive",
               TRUE                   ~ "Negative") %>% as.factor())
     
     g <- data_cor %>%
          ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
          geom_point(aes(color = Correlation), size = size) +
          geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
          geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
          expand_limits(x = c(-1, 1)) +
          theme_tq() +
          scale_color_manual(values = c(color_neg, color_pos)) 
     
     if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
     
     return(g)
     
}

train_tbl %>% plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

train_tbl %>%
     select(Attrition_Yes, contains("JobRole")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# Correlation Evaluation ---

#  1. Decriptive Features: age, gender, marital status, etc

train_tbl %>% select(Attrition_Yes, Age, contains("Gender"), contains("MaritalStatus"), NumCompaniesWorked, 
                    contains("Over18"), DistanceFromHome) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

#  2. Employment Features

train_tbl %>% select(Attrition_Yes, contains("employee"), contains("department"), contains("job")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)
# contains is NOT case sentitive

#  3. Compensation Features

train_tbl %>% select(Attrition_Yes, contains("income"), contains("rate"), contains("salary"), 
                         contains("stock")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

#  4. Survey Results

train_tbl %>% select(Attrition_Yes, contains("satisfaction"), contains("life")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

#  5. Performance Data

train_tbl %>% select(Attrition_Yes, contains("performance"), contains("involvement")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

#  6. Work-Life Features

train_tbl %>% select(Attrition_Yes, contains("overtime"), contains("travel")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

#  7. Training and Education

train_tbl %>% select(Attrition_Yes, contains("training"), contains("education")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

#  8. Time-Based Features

train_tbl %>% select(Attrition_Yes, contains("years")) %>% 
     plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

