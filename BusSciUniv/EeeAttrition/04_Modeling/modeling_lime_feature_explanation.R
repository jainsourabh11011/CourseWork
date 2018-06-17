# LIME FEATURE EXPLANATION ----

# See http://uc-r.github.io/lime

# 1. Setup ----

# Load Libraries 
if(!require(easypackages)){install.packages("easypackages")}
library(easypackages)
packages("h2o", "recipes", "readxl", "tidyverse", "tidyquant", "lime", prompt = TRUE)

# Load Data
setwd("~/GitHub/CourseWork/BusSciUniv/EeeAttrition")
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_num2factor(JobLevel, StockOptionLevel) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, newdata = train_readable_tbl)
test_tbl  <- bake(recipe_obj, newdata = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_AllModels_0_AutoML_20180617_085805")

automl_leader

# 3. LIME ----

# 3.1 Making Predictions ----

automl_leader %>% h2o.predict(newdata = as.h2o(test_tbl)) %>% as.tibble()

# Focus on record 5 since it is predicted to turnover

automl_leader %>% h2o.predict(newdata = as.h2o(test_tbl)) %>% as.tibble() %>% 
     bind_cols(test_tbl %>%  select(Attrition, EmployeeNumber))

predictions_tbl <- automl_leader %>% h2o.predict(newdata = as.h2o(test_tbl)) %>% as.tibble() %>% 
     bind_cols(test_tbl %>%  select(Attrition, EmployeeNumber))

test_tbl %>% slice(5) %>% glimpse()

# 3.2 Single Observation ----
# LIME is used to determine which features contribute to the prediction (and by how much) for a single (local) observation

# 2 steps:  Build explainer with lime() then Create Explanation with explain()
# h2o, keras and caret have been integrated into lime().  For otehr packages, use special functions model_type(), predict_model -
# see lime documentation

explainer <- train_tbl %>% select(-Attrition) %>% 
     lime(model           = automl_leader, 
          bin_continuous  = TRUE, 
          N_bins          = 4,
          quantile_bins   = TRUE)
# bin_continuous makes it easy to detect what causes a continuous feature to have a high feature weight in the lime explanation
# quantile_bins tells how to distribute observations within the bins.  If TRUE, cuts will be evenly distributed within each bin
# 4-5 bins is typically sufficient

# explainer #lots of data

explanation <- test_tbl %>% slice(5) %>% select(-Attrition) %>% 
     lime::explain(
          explainer = explainer,
          n_labels = 1,
          n_features = 8,
          n_permutations = 5000,
          kernel_width = 0.5)

# n_labels - binary classifcation therefore just one label - need to explore this concept
# n_features - number of imporatnt features to return to explain the record prediction
# n_permutations - the more test/evalauations, the better the explanations (typically)
# kernel_width - Affects the LIME linear model (R^2) and therefore should be tuned to ensure you get the best explanations - IMPORTANT

# # LIMT Algorithm 6 Steps
# # # 1. Given an observation, permute it to create replicated feature data with slight value modifications
# # # 2. Compute similarity distance measure between original and permuted observations
# # # 3. Apply selected machine learning model to predict outcomes of permuted data
# # # 4. Select m number of features to best describe predicted outcomes
# # # 5. Fit a simple model to the permuted data explaining the complex model outcome with m features from
# # # #  the permuted data weighted by its similarity to the original observation.
# # # 6. Use the resulting feature weights to explain local behavior
# The larger n_permutations often the better rthe results.

explanation$model_r2 #0.282685
# Want R^2 to be as high as possible - optimize by adjusting kernel_width

# In this case, kernel_width = 0.5 works relatively well.  Trying 0.2 - 1.5 : 0.2 performed best

explanation <- test_tbl %>% slice(5) %>% select(-Attrition) %>% 
     lime::explain(
          explainer = explainer,
          n_labels = 1,
          n_features = 8,
          n_permutations = 5000,
          kernel_width = 0.2)
explanation$model_r2

#  kernel_width - R^2 values:  0.2:0.29714; 0.3:0.2752115 0.4:0.269511; 1.0:0.2849686; 1.5:0.2769159

# Explore other tuning parameters

explanation %>% as.tibble() %>% select(feature:prediction)

# feature_weight - indicates the importance.  +/- indicates support or contradict.  
# Positive weights support the positive binary classification (Yes or attrition in this case)

# Plot LIME Features

plot_features(explanation = explanation, ncol = 1)
# Note 4 < NumCompaniesWorked is a label from continuous binning we performed earlier
# The red contradict features represent those features that encourge an emplyee to stay (supports the NO classification)
# The GREEN outweigh the RED

# 3.3 Mulitle Explanations ----

# Modify the slice function
explanation <- test_tbl %>% slice(1:20) %>% select(-Attrition) %>% 
     lime::explain(
          explainer = explainer,
          n_labels = 1,
          n_features = 8,
          n_permutations = 5000,
          kernel_width = 0.2)

explanation %>% as.tibble()

range(explanation$model_r2)

plot_features(explanation = explanation, ncol = 4)

plot_explanations(explanation = explanation)

# 5, 11, 20 20 are likely to leave
# Overtime = Yes is dark red on the No column contradicting a NO response

# 4.0 Challenge Solutions

# 4.1 Recreating plot_features

library(glue)# Helps with text

data_transformed <- explanation %>% filter(case ==1) %>% as.tibble() %>% 
     mutate(
          feature_desc = as.factor(feature_desc) %>% 
               fct_reorder(abs(feature_weight), .desc = FALSE),
          key = ifelse(feature_weight > 0, "Supports", "Contradicts") %>% 
               fct_relevel("Supports"),
          case_text = glue("Case: {case}"),
          label_text = glue("Label: {label}"),
          prob_text = glue("Probability:  {round(label_prob, 2)}"),
          r2_text = glue("Explanation Fit:  {model_r2 %>% round(2)}")) %>% 
     select(feature_desc, feature_weight, key, case_text:r2_text)

data_transformed

data_transformed %>% ggplot(aes(feature_desc, feature_weight, fill = key)) +
     geom_bar(stat = "identity") + # could replace with geom_col() alone
     coord_flip() + 
     theme_tq() +
     scale_fill_tq() +
     labs(y = "Weight", x = "Feature") +
     facet_wrap(~ case_text + label_text + prob_text + r2_text,
                ncol = 1, scales = "free")

plot_features_tq <- function(explanation, ncol){
     
     data_transformed <- explanation %>% as.tibble() %>% 
          mutate(
               feature_desc = as.factor(feature_desc) %>% 
                    fct_reorder(abs(feature_weight), .desc = FALSE),
               key = ifelse(feature_weight > 0, "Supports", "Contradicts") %>% 
                    fct_relevel("Supports"),
               case_text = glue("Case: {case}"),
               label_text = glue("Label: {label}"),
               prob_text = glue("Probability:  {round(label_prob, 2)}"),
               r2_text = glue("Explanation Fit:  {model_r2 %>% round(2)}")) %>% 
          select(feature_desc, feature_weight, key, case_text:r2_text)

     data_transformed %>% ggplot(aes(feature_desc, feature_weight, fill = key)) +
          geom_bar(stat = "identity") + # could replace with geom_col() alone
          coord_flip() + 
          theme_tq() +
          scale_fill_tq() +
          labs(y = "Weight", x = "Feature") +
          facet_wrap(~ case_text + label_text + prob_text + r2_text,
                     ncol = ncol, scales = "free")
}

# New function example

explanation %>% filter(case %in% 1:6) %>% plot_features_tq(ncol=2)

# 4.2 Recreating plot_explanations ----

# Recall as_factor changes from character in the order that it appears

plot_explanations_tq <- function(explanation) {
     
     data_transformed <- explanation %>%
          as.tibble() %>%
          mutate(
               case    = as_factor(case),
               order_1 = rank(feature)) %>% 
          group_by(feature) %>%
          mutate(order_2 = rank(feature_value)) %>%
          ungroup() %>%
          mutate(order = order_1 * 1000 + order_2) %>% # A trick to get one order that preserves the hierarchy of feature and feature_value
          mutate(
               feature_desc = as.factor(feature_desc) %>% 
                    fct_reorder(order, .desc =  T)) %>%
          select(case, feature_desc, feature_weight, label)
     
     data_transformed %>%
          ggplot(aes(case, feature_desc)) +
          geom_tile(aes(fill = feature_weight)) +
          facet_wrap(~ label) +
          theme_tq() +
          scale_fill_gradient2(low = palette_light()[[2]], 
                               mid = "white",
                               high = palette_light()[[1]]) +
          theme(
               panel.grid = element_blank(),
               legend.position = "right",
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
          labs(y = "Feature", x = "Case", fill = glue("Feature
                                                      Weight"))
}
# Note glue in labs statement above.  With the added return, it stacks Feature and Weight label

plot_explanations(explanation)

plot_explanations_tq(explanation)
