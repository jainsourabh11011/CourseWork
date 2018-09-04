# H2O MODELING ----

# 1. Setup ----

# Load Libraries

pkgs <- c(
     "h2o",        # High performance machine learning
     "recipes",    # Creating ML preprocessing recipes
     "tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
     "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
     "glue",       # Pasting text
     "cowplot",    # Handling multiple ggplots
     "fs",         # Working with the file system - directory structure
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

#Machine Learning Preprocessing ----
#  H2O performs most preprocessing steps including dummy variables, factors, transformations 
#  Data just has to be in readable formal with factors and numerical data

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>%
     step_zv(all_predictors()) %>%
     step_num2factor(JobLevel, StockOptionLevel) %>% 
     prep()
# recall these factors were not handled as factors

train_tbl <- bake(recipe_obj, newdata = train_readable_tbl)
test_tbl <- bake(recipe_obj, newdata = test_readable_tbl)

glimpse(test_tbl)

# 2. Modeling ----

h2o.init()

train_tbl <- as.h2o(train_tbl)#h2o requires a special h2o df

split_h2o <-  h2o.splitFrame(train_tbl, ratios = c(0.85), seed = 1234) 
#outputs a list of 2 (could have more splits) like ratios = c(0.85, 0.075) gives 3 splits

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]

test_h2o <- as.h2o(test_tbl)

y <- "Attrition" # label is required
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
     x = x, 
     y = y, 
     training_frame = train_h2o, # develops model
     validation_frame = valid_h2o, # tunes hyperparameters via grid search automatically
     leaderboard_frame = test_h2o,# Test set held out from training & tuning
     max_runtime_secs = 30,# default is 3600 = 1 hour
     nfolds = 5# 10 folds more standrard but trying to reduce model runtimes for this experiment
)

typeof(automl_models_h2o)
slotNames(automl_models_h2o)

# S4 classes use the @ rather than $
automl_models_h2o@leaderboard

automl_models_h2o@leader

# Select a model other than the leader by creating Custom h2o Extract Function

automl_models_h2o@leaderboard %>% 
     as.tibble() %>% # Makes things easier to use
     slice(1) %>% # Allows you to select row or rows
     pull(model_id) %>% # extracts a single variable in a df
     h2o.getModel()

extract_h2o_model_name_by_position <- function(h2o_leaderboard, position = 1, verbose = TRUE){
     
     model_name <- h2o_leaderboard %>% 
          as.tibble() %>%
          slice(position) %>%
          pull(model_id)
     
     if(verbose) message(model_name)
     
     return(model_name)
}

# Test new function
extract_h2o_model_name_by_position(automl_models_h2o@leaderboard, position = 3) %>% 
     h2o.getModel()

# Save and Load Models

#  Save Model
automl_models_h2o@leaderboard

# Commented out to make the R file run as source
# h2o.getModel("GLM_grid_0_AutoML_20180827_113110_model_0") %>% 
#      h2o.saveModel(path = "04_Modeling/h2o_models/")
# 
# h2o.getModel("StackedEnsemble_BestOfFamily_0_AutoML_20180827_113110") %>% 
#      h2o.saveModel(path = "04_Modeling/h2o_models/")
# 
# h2o.getModel("GBM_grid_0_AutoML_20180827_113110_model_4") %>% 
#      h2o.saveModel(path = "04_Modeling/h2o_models/")
# 
# h2o.getModel("DeepLearning_0_AutoML_20180827_113110") %>% 
#      h2o.saveModel(path = "04_Modeling/h2o_models/")

#  Load Model
h2o.loadModel("04_Modeling/h2o_models/GLM_grid_0_AutoML_20180827_113110_model_0")

# NOTE:  getModel returns a reference to an existing model in the H2O instance.
#        loadModel loads a saved H2O model from disk. 

# Making Predictions

stacked_ensemble_h2o <- 
     h2o.loadModel("04_Modeling/h2o_models/GLM_grid_0_AutoML_20180827_113110_model_0")

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))
typeof(predictions)

predictions_tbl <- predictions %>% as.tibble() 

predictions_tbl

# AutoML Model Parameters

automl_models_h2o@leaderboard

deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_0_AutoML_20180827_113110")

deeplearning_h2o@allparameters

# Cross Validation

#  k=5 gives 6 models - the last uses all the data using the parameters found so far

#h2o.cross_validation_models(deeplearning_h2o)

h2o.auc(deeplearning_h2o, train = T, valid = T, xval = T)# xval-cross-validation AUC

# 3. Visualize the Leaderboard ----

automl_models_h2o@leaderboard

#  Data Transformation
data_transformed <- automl_models_h2o@leaderboard %>% 
     as.tibble() %>% 
     mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>% 
     # str_plit splits string into list of strings separated by the pattern
     # simplify = T makes it a matrix rather than a list
     slice(1:10) %>% 
     rownames_to_column() %>% 
     mutate(
          model_id = as_factor(model_id) %>% reorder(auc), 
          model_type = as.factor(model_type)
     ) %>% 
     gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>% 
     # to use color and group color, need long format for ggplot
     mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())

# Data Visualization
data_transformed %>% 
     ggplot(aes(value, model_id, color = model_type)) +
     geom_point(size =3) +
     geom_label(aes(label = round(value, 2), hjust = "inward")) +
     facet_wrap(~ key, scales = "free_x") +
     theme_tq() +
     scale_color_tq() +
     labs(title = "H2O Leaderboard Metrics",
          subtitle = paste0("Ordered by:  AUC"),
          y = "Model Position, Model_id", x = "")


# Visualize the H2O leaderboard to help with model selection
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
     
     # Setup inputs
     order_by <- tolower(order_by[[1]])
     
     leaderboard_tbl <- h2o_leaderboard %>%
          as.tibble() %>%
          mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[,1]) %>%
          rownames_to_column(var = "rowname") %>%
          mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
     
     # Transformation
     if (order_by == "auc") {
          
          data_transformed_tbl <- leaderboard_tbl %>%
               slice(1:n_max) %>%
               mutate(
                    model_id   = as_factor(model_id) %>% reorder(auc),
                    model_type = as.factor(model_type)
               ) %>%
               gather(key = key, value = value, 
                      -c(model_id, model_type, rowname), factor_key = T)
          
     } else if (order_by == "logloss") {
          
          data_transformed_tbl <- leaderboard_tbl %>%
               slice(1:n_max) %>%
               mutate(
                    model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
                    model_type = as.factor(model_type)
               ) %>%
               gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)
          
     } else {
          stop(paste0("order_by = '", order_by, "' is not a permitted option."))
     }
     
     # Visualization
     g <- data_transformed_tbl %>%
          ggplot(aes(value, model_id, color = model_type)) +
          geom_point(size = size) +
          facet_wrap(~ key, scales = "free_x") +
          theme_tq() +
          scale_color_tq() +
          labs(title = "Leaderboard Metrics",
               subtitle = paste0("Ordered by: ", toupper(order_by)),
               y = "Model Postion, Model ID", x = "")
     
     if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
     
     return(g)
     
}

# Test Model Visualization
automl_models_h2o@leaderboard %>% plot_h2o_leaderboard(order_by = "logloss")

# 4. Assessing Performance ---- 

deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_0_AutoML_20180827_113110")
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_0_AutoML_20180827_113110")
glm_h2o <- h2o.loadModel("04_Modeling/h2o_models/GLM_grid_0_AutoML_20180827_113110_model_0")

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(performance_h2o)
performance_h2o %>% slotNames()

performance_h2o@metrics

# Classifier Sumarry Metrics

h2o.performance(stacked_ensemble_h2o, train = T, valid = F, xval = F)# gives auc of the test data
# the train, val and xval arguments only work for models - not performance objects                )
h2o.auc(stacked_ensemble_h2o, train = T, valid = T, xval = T)

#Gini Coefficient:  AUC = (GiniCoeff +1)/2)
h2o.giniCoef(performance_h2o)# again gives result based on the test data
h2o.giniCoef(stacked_ensemble_h2o, train = T, valid = T, xval = T)

# logloss is a mtetric that measures the class probability from the model against the actual value in binary format (0/1)
# https://www.r-bloggers.com/making-sense-of-logarithmic-loss/
h2o.logloss(performance_h2o)
h2o.logloss(stacked_ensemble_h2o, train = T, valid = T, xval = T)

# Must understand threshold, precision and recall
# Top Row - Predictions; Left Column - Actual
# Threshold is the value that determines which class probability is a 1 or 0
#  Example:  if a 1 is assigned if the probability is above the threshold value - which is a probability
# F1 is the optimal balance between precsion and recall.  The threshold  maximizes F1.  However,
# this is not always the best case.  An expected vlaue optimization is required when the costs of
# false positives and false negatives are known.
# F1 = ( * (precision * recall))/ (precision + recall) - hence balances precision & recall
# PRECISION - Measures false positives (eg, predicted to leave but actually stay)
#  Precision = TP/(TP + FP) :  It detects how frequently the algo over-picks the YES class
# RECALL - Measures false negatives (eg, predicted to stay but actually leave)
#  Recall = TP/(TP + FN) - It provides a metric for under-picking YES
# Because there are differnet costs associated with FN and FP; FN often cost a company more.  
#  This is where Expected Value (EV) comes into play - this discussion is coming.
h2o.confusionMatrix(performance_h2o)# test data
h2o.confusionMatrix(stacked_ensemble_h2o)#training data

# h2o.metric converts a performance object into a series of metrics (precision, recall, f1, etc)
# that vary by threshold
performance_h2o %>% h2o.metric()
performance_h2o %>% h2o.metric() %>% as.tibble()
performance_h2o %>% h2o.metric() %>% as.tibble() %>% glimpse()

performance_tbl <- performance_h2o %>% h2o.metric() %>% as.tibble()

# Precision vs Recall Plot
h2o.confusionMatrix(performance_h2o)# test data

# Numbers below may vary
precision <- 20/(20 + 5)
recall <- 20/(20 + 16)
F1 <- (2*(precision * recall))/(precision + recall)# 0.6557

performance_tbl %>% filter(f1 == max(f1)) # 0.656 - it is not always exact but always close
# threshold from record above is 0.496 - the f1 max threshold
#  the precision in this record is 0.800 and recall = 0.556
# Because there are differnet costs associated with FN and FP; FN often cost a company more.  
#  This is where Expected Value (EV) comes into play - this discussion is coming.

# Plot F1
performance_tbl %>% ggplot(aes(x = threshold)) + 
     geom_line(aes(y = precision), color = "blue", size = 1) +
     geom_line(aes(y = recall), color = "red", size = 1) +
     geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) +
     theme_tq() +
     labs(title = "Precision vs Recall", y = "value")

# ROC Plot

load_model_performance_metrics <- function(path, test_tbl){
     
     model_h2o <- h2o.loadModel(path)
     perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
     
     perf_h2o %>% h2o.metric() %>% as.tibble() %>% mutate(auc = h2o.auc(perf_h2o)) %>% 
          select(tpr, fpr, auc)
}

# Test new function
path <- "04_Modeling/h2o_models/DeepLearning_0_AutoML_20180827_113110"
load_model_performance_metrics(path, test_tbl)

# Iterate all models in the folder using fs package to help

fs::dir_info(path = "04_Modeling/h2o_models/") # only need 1st column called path
fs::dir_info(path = "04_Modeling/h2o_models/") %>% select(path)

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h2o_models/") %>% 
     select(path) %>% 
     mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>% 
     unnest()#it was a list now its flat

# Data prep for plotting
# recall as_factor maintains order
# recall numeric to factor must first convert to character
model_metrics_tbl %>% mutate(
     path = str_split(path, pattern = "/", simplify = T)[, 3] %>% as_factor(),
     auc = auc %>% round(3) %>% as.character() %>% as_factor()) %>% 
     
     ggplot(aes(x = fpr, y = tpr, color = path, linetype = auc)) +
     geom_line(size = 1) + 
     theme_tq() + 
     scale_color_tq()+
     theme(legend.direction = "vertical") +
     labs(
          title ="ROC Plot", 
          subtitle = "Performance of 4 Top Performing Models")

# Plot Precision vs Recall Across Different Models
# Same as aove but add precison and recall
load_model_performance_metrics <- function(path, test_tbl){
     
     model_h2o <- h2o.loadModel(path)
     perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
     
     perf_h2o %>% h2o.metric() %>% as.tibble() %>% mutate(auc = h2o.auc(perf_h2o)) %>% 
          select(tpr, fpr, auc, precision, recall)# ADDED
}

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h2o_models/") %>% 
     select(path) %>% 
     mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>% 
     unnest()#it was a list now its flat

model_metrics_tbl %>% mutate(
     path = str_split(path, pattern = "/", simplify = T)[, 3] %>% as_factor(),
     auc = auc %>% round(3) %>% as.character() %>% as_factor()) %>% 
     
     ggplot(aes(x = recall, y = precision, color = path, linetype = auc)) + #CHANGED
     geom_line(size = 1) + 
     theme_tq() + 
     scale_color_tq()+
     theme(legend.direction = "vertical") +
     labs(
          title ="Precision vs Recall Plot", 
          subtitle = "Performance of 4 Top Performing Models")

# Gain and Lift ----

#  Emphasizes how much model improves results
# gain and lift are results based metrics
# Combine predictions with test data
ranked_predictions_tbl <- predictions_tbl %>% 
     bind_cols(test_tbl) %>% 
     select(predict:Yes, Attrition) %>% 
     arrange(desc(Yes))
# 20 Predicted YES - 16 actually left
# If 16% leave each year of 220 employees = 35 employees leaving
# If 35 expected to quit, we just gained 16 of 35 or 46% in first 20 cases
# Lift:  If expectation is 1.6, we beat the expectation by 16/1.6 = 10x in first 20 cases

ranked_predictions_tbl %>% 
     mutate(ntile = ntile(Yes, n = 10)) %>% 
     group_by(ntile) %>% 
     summarise(
          cases = n(),
          responses = sum(Attrition == "Yes")) %>% 
     arrange(desc(ntile)) %>% 
# 17 of 22 actually left
     mutate(group = row_number()) %>% 
     select(group, cases, responses) %>% 
     mutate(
          cumulative_responses = cumsum(responses),
          pct_responses = responses / sum(responses),
          gain = cumsum(pct_responses),
          # we gain ability to target 75% of quitter focused on 1st 3 groups
          cumulative_pct_cases = cumsum(cases)/sum(cases),
          lift = gain/cumulative_pct_cases,
          gain_baseline = cumulative_pct_cases,
          lift_baseline = gain_baseline / cumulative_pct_cases
     )

# calculated_gain_lift_tbl - mucheasier 
# h2o provide smost of the info we need!

gain_lift_tbl  <-  performance_h2o %>% 
     h2o.gainsLift() %>% 
     as.tibble()

gain_lift_tbl %>% glimpse()
# group = ntiles by 16:  220 leave /16 ntiles = 13.75 employees/group
# cumulative_data_fraction = cumulative_pct_cases
# gain is really the cumulative_capture_rate
# cumulative_lift is what we will use - sames as the calculated lift above = gain/cumulative_pct_cases
#  cumulative_lift = cumulative_capture_rate / cumulative_data_fraction

gain_transformed_tbl <-  gain_lift_tbl %>% 
     select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
     select(-contains("lift")) %>% 
     mutate(baseline = cumulative_data_fraction) %>% 
     rename(gain = cumulative_capture_rate) %>% 
     gather(key = key, value = value, gain, baseline)
# Remember to use gather() to stack the data to use ggplot color, group aaesthetics or facet_wrap
gain_transformed_tbl %>% 
     ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
     geom_line(size = 1.5) +
     theme_tq() +
     scale_color_tq() +
     labs(
          title = "Gain Chart",
          x = "Cumulative Data Fraction",
          y = "Gain")

# Lift Chart
lift_transformed_tbl <-  gain_lift_tbl %>% 
     select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>% 
     select(-contains("capture")) %>% 
     mutate(baseline = 1) %>% 
     rename(lift = cumulative_lift) %>% 
     gather(key = key, value = value, lift, baseline)

lift_transformed_tbl %>% 
     ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
     geom_line(size = 1.5) +
     theme_tq() +
     scale_color_tq() +
     labs(
          title = "Lift Chart",
          x = "Cumualtive Data Fraction",
          y = "Lift")

# Performance Visualization

# set vars to help build the large function below
h2o_leaderboard <- automl_models_h2o@leaderboard
newdata <- test_tbl
order_by <- "auc"
max_models <- 4
size = 1
model_id <- "GLM_grid_0_AutoML_20180827_113110_model_0"

plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5){
     # Inputs
     leaderboard_tbl <- h2o_leaderboard %>% 
          as.tibble() %>% 
          slice(1:max_models)#returns max_model records, default is 3
     
     newdata_tbl <- newdata %>% as.tibble()
     
     order_by <- tolower(order_by[[1]])
     order_by_expr <- rlang::sym(order_by)#converts a string stored within a varaible to a 
     #column name(symbol) that is unevaluated so we can use it later using !! (bang bang)
     
     h2o.no_progress()
     
     # 1. Model Metrics
     
     get_model_performance_metrics <- function(model_id, test_tbl){
          
          model_h2o <- h2o.getModel(model_id)
          perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
          
          perf_h2o %>% 
               h2o.metric() %>% 
               as.tibble() %>% 
               select(threshold, tpr, fpr, precision, recall)
     }
     
     model_metrics_tbl <- leaderboard_tbl %>% 
          mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>% 
          unnest() %>% 
          mutate(
               model_id = as_factor(model_id) %>% 
                    fct_reorder(!! order_by_expr, .desc = ifelse(order_by =="auc", TRUE, FALSE)),
               
               auc = auc %>% 
                    round(3) %>% 
                    as.character() %>% 
                    as_factor() %>% 
                    fct_reorder(as.numeric(model_id)),#fct_reorder requires numeric value
               logloss = logloss %>% 
                    round(4) %>% 
                    as.character() %>% 
                    as_factor() %>% 
                    fct_reorder(as.numeric(model_id))
               )
     # 1A. ROC Plot
     p1 <- model_metrics_tbl %>% 
          ggplot(aes_string(x = "fpr", y = "tpr", color = "model_id", linetype = order_by)) +
          geom_line(size = size) + 
          theme_tq() +
          scale_color_tq() +
          labs(title = "ROC", x = "FPR", y = "TPR") +
          theme(legend.direction = "vertical")
     
     # 1B. Precision vs Recall Plot
     p2 <- model_metrics_tbl %>% 
          ggplot(aes_string(x = "recall", y = "precision", color = "model_id", linetype = order_by)) +
          geom_line(size = size) + 
          theme_tq() +
          scale_color_tq() +
          labs(title = "Precision vs Recall", x = "Recall", y = "Precision") +
          theme(legend.position = "none")
     
    # 2. Gain / Lift
     get_gain_lift <- function(model_id, test_tbl){
          
          model_h2o <- h2o.getModel(model_id)
          perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
          
          perf_h2o %>%
               h2o.gainsLift() %>% 
               as.tibble() %>% 
               select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)} 
     
     gain_lift_tbl <- leaderboard_tbl %>% 
          mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>% 
          unnest() %>% 
          mutate(
               model_id = as_factor(model_id) %>% 
                    fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
               auc = auc %>% 
                    round(3) %>% 
                    as.character() %>% 
                    as_factor() %>% 
                    fct_reorder(as.numeric(model_id)),#fct_reorder requires numeric value
               logloss = logloss %>% 
                    round(4) %>% 
                    as.character() %>% 
                    as_factor() %>% 
                    fct_reorder(as.numeric(model_id))) %>% 
          rename(
               gain = cumulative_capture_rate,
               lift = cumulative_lift
          )
     # 2A. Gain Plot
     
     p3 <- gain_lift_tbl %>% 
          ggplot(aes_string(x = "cumulative_data_fraction", y = "gain", 
                            color= "model_id", linetype = order_by)) +
          geom_line(size = size) +
          geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "black", size = size) +
          theme_tq() +
          scale_color_tq() +
          expand_limits(x = c(0, 1), y = c(0, 1)) +
          labs(title = "Gain", x = "Cumulative Data Fraction", y = "Gain") +
          theme(legend.position = "none")
     
     # 2B. Lift Plot
     p4 <- gain_lift_tbl %>% 
          ggplot(aes_string(x = "cumulative_data_fraction", y = "lift", 
                            color= "model_id", linetype = order_by)) +
          geom_line(size = size) +
          geom_segment(x = 0, y = 1, xend = 1, yend = 1, color = "black", size = size) +
          theme_tq() +
          scale_color_tq() +
          expand_limits(x = c(0, 1), y = c(0, 1)) +
          labs(title = "Lift", x = "Cumulative Data Fraction", y = "Lift") +
          theme(legend.position = "none")
     
     # Combine using cowplot
     # below glus is used - a better method of pastin strings that paste0.  Uses{} to intermix
     #  code with text.  Uses only one string that many strings - just easier than paste0
     
     p_legend <- cowplot::get_legend(p1)
     p1 <- p1 + theme(legend.position = "none")
     
     p <- cowplot::plot_grid(p1, p2, p3, p4, ncol=2)
     
     p_title <- ggdraw() +
          draw_label("H2O Model Metrics", size = 18, fontface = "bold",
                     colour = palette_light()[[1]])#MUST use COLOUR
     p_subtitle <- ggdraw() +
          draw_label(glue("Ordered by {toupper(order_by)}"), size = 10, colour = palette_light()[[1]])
     
     ret <- plot_grid(p_title, p_subtitle, p, p_legend, ncol = 1,
                     rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
                    # the model plots p are given a spacing of 1
     
     h2o.show_progress()
     
     return(ret)
}

# Test Final Visualization ----

automl_models_h2o@leaderboard %>% 
     plot_h2o_performance(newdata = test_tbl, order_by = "auc", max_models = 3)
automl_models_h2o@leaderboard %>% 
     plot_h2o_performance(newdata = test_tbl, order_by = "logloss", max_models = 2)

-------------------------------
# Grid Search & CV ----

# AWESOME resource:  https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/

#deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_0_AutoML_20180827_113110")

h2o.performance(deeplearning_h2o, newdata = as.h2o(test_tbl))#AUC 0.8624698

# do CV even with grid search
# epochs can lead to overfitting
# grid_id simplycopied from the variable in this function - makes it easier, not required, can be anything
deeplearning_grid_01 <-  h2o.grid(
     algorithm = "deeplearning",
     grid_id = "deeplearning_grid_01",
     
     # from h2o.deeplearning()
     x = x,
     y = y,
     training_frame = train_h2o,
     validation_frame = valid_h2o,
     nfolds = 5, 
     
     hyper_params = list(
          hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
          epochs = c(10, 50, 100)
     )
)

deeplearning_grid_01

h2o.getGrid("deeplearning_grid_01", sort_by = "auc", decreasing = T)

# deeplearning_grid_01_model_15 <- h2o.getModel("deeplearning_grid_01_model_15")
# 
# deeplearning_grid_01_model_15 %>% h2o.auc(train = T, valid = T, xval = T)
# 
# deeplearning_grid_01_model_15 %>% 
#      h2o.performance(newdata = as.h2o(test_tbl))

