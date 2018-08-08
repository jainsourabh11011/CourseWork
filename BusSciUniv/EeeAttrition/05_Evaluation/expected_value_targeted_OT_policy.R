# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)

# Load Data
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

# Replace this with your model!!! (or rerun h2o.automl)
#automl_leader <- h2o.loadModel("04_Modeling/h2o_models/GBM_grid_0_AutoML_20180806_163444_model_4")

automl_leader <- h2o.loadModel("CourseWork/BusSciUniv/EeeAttrition/04_Modeling/h2o_models/GBM_grid_0_AutoML_20180806_163444_model_4")

automl_leader

# 3. Primer: Working With Threshold & Rates ----
#  Prob_Leave >= 30% are targetted

performance_h2o <- automl_leader %>% h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>% 
     h2o.confusionMatrix()
# The rows are actuals No and Yes; the columns are predicted No and Yes
#  The False Negatives in Yes row / No column are the most expensive to miss incorrectly - Predicted to STAY but actually LEAVES
#  False Positives are not so damaging - predict someone will leave but actually stays - no harm done
# The F1 threshold in the output determines the mix in the confusion matrix
#   F1 is the optimal balance between Precision and Recall.  However, the threshold @ Max F1 is not typically the optimal value 
#   for the business case because FNs are typically more costly then FPs.

rate_by_threshold_tbl <- performance_h2o %>% 
     h2o.metric() %>% 
     as.tibble() # provides observation by threshold level
glimpse(rate_by_threshold_tbl)
# want to minimize false negatives

rate_by_threshold_tbl %>% select(threshold, f1, tnr:tpr)

rate_by_threshold_tbl %>% select(threshold, f1, tnr:tpr) %>% filter(f1 == max(f1)) %>% slice(1)
# slice(1) guaranttees to retun only 1 record (max and min oftern return mutiple records)

rate_by_threshold_tbl %>% select(threshold, f1, tnr:tpr) %>% 
     gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>% 
     mutate(key = fct_reorder2(key, threshold, value)) %>% 
     ggplot(aes(threshold, value, color = key)) +
     geom_point() + 
     geom_smooth() +
     theme_tq() +
     scale_color_tq() +
     theme(legend.position = "right") +
     labs(title = "Expected Rates",
          y = "Value", x = "Threshold")

# factor_key: If FALSE, the default, the key values will be stored as a character vector. If TRUE, will be stored as a factor, 
# which preserves the original ordering of the columns.

# fct_reorder2 is used when plotting using x and y axis features to control the legend.  Note the order of the legend matches the 
# order of the values on the right side of the plot

# - tnr + fpr = 1

# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

# Code below is simply copied from expected_value_no_OT_polict.R file section 3.1
source("00_Scripts/assess_attrition.R")

predictions_with_OT_tbl <- automl_leader %>% 
     h2o.predict(newdata = as.h2o(test_tbl)) %>% 
     as.tibble() %>% 
     bind_cols(
          test_tbl %>% 
               select(EmployeeNumber, MonthlyIncome, OverTime))

ev_with_OT_tbl <- predictions_with_OT_tbl %>%
     mutate(
          attrition_cost = calculate_attrition_cost(
               n = 1,
               salary = MonthlyIncome * 12,
               net_revenue_per_employee = 250000
          )
     ) %>%
     mutate(
          cost_of_policy_change = 0 
     ) %>%
     mutate(
          expected_attrition_cost = 
               Yes * (attrition_cost + cost_of_policy_change) +
               No *  (cost_of_policy_change)
     )

# Attrition Cost occurs only if the employee leaves.  The Yes column is the liklihood of leaving.  
# When combined, expected attrition cost results

total_ev_with_OT_tbl <- ev_with_OT_tbl %>% 
     summarise(total_expected_attrition_cost_0 = sum(expected_attrition_cost))

total_ev_with_OT_tbl

# 4.2 Calculating Expected Value With Targeted OT ----

max_f1_tbl <- rates_by_threshold_tbl %>% 
     select(threshold, f1, tnr:tpr) %>% 
     filter(f1 == max(f1)) %>% 
     slice(1)

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr

threshold <- max_f1_tbl$threshold
# Max f1 identifies minmizes the FNR and FPR - see plot below

rate_by_threshold_tbl %>% select(threshold, f1, tnr:tpr) %>% 
     gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>% 
     mutate(key = fct_reorder2(key, threshold, value)) %>% 
     ggplot(aes(threshold, value, color = key)) +
     geom_point() + 
     geom_smooth() +
     theme_tq() +
     scale_color_tq() +
     theme(legend.position = "right") +
     labs(title = "Expected Rates",
          y = "Value", x = "Threshold")+
     geom_vline(xintercept = threshold, color = "blue")

# Anyone with a risk higher than threshold, convert their OT to NO

test_targeted_OT_tbl <- test_tbl %>% 
     add_column(Yes = predictions_with_OT_tbl$Yes) %>% 
     mutate(
          Overtime = case_when(
               Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime)),
               TRUE ~ OverTime
          )
     ) %>% 
     select(-Yes)

test_targeted_OT_tbl

predictions_targeted_OT_tbl <- automl_leader %>% 
     h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) %>% 
     as.tibble() %>% 
     bind_cols(
          test_tbl %>% 
               select(EmployeeNumber, MonthlyIncome, OverTime),
          test_targeted_OT_tbl %>% 
               select(OverTime)
     ) %>% 
     rename(OverTime_0 = OverTime,
            OverTime_1 = OverTime1)

predictions_targeted_OT_tbl

avg_overtime_pct <- 0.10

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>% 
     mutate(
          attrition_cost = calculate_attrition_cost(
               n = 1, 
               salary = MonthlyIncome * 12,
               net_revenue_per_employee = 250000)
     ) %>% 
     mutate(
          #cost only when OverTime_0 changes from Yes to No in OverTime_1
          cost_of_policy_change = case_when(
               OverTime_0 == "Yes" & OverTime_1 =="No" ~ attrition_cost * avg_overtime_pct, TRUE ~ 0 #Try w/o TRUE to learn
          )
     ) %>% 
     mutate(
          cb_tn = cost_of_policy_change,# cb  means cost benefit
          cb_fp = cost_of_policy_change,
          cb_tp = cost_of_policy_change + attrition_cost,
          cb_fn = cost_of_policy_change + attrition_cost,
          expected_attrition_cost = 
               Yes * (tpr * cb_tp + fnr * cb_fn) + 
               No *  (tnr * cb_tn + fpr * cb_fp)
     )

ev_targeted_OT_tbl

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>% summarise(
     total_expected_attrition_cost_1 = sum(expected_attrition_cost))

total_ev_targeted_OT_tbl

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
     total_ev_with_OT_tbl,
     total_ev_targeted_OT_tbl) %>% 
     mutate(
          savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
          pct_savings = savings / total_expected_attrition_cost_0
     )

savings_tbl

# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

# 5.2 Optimization ----



# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

# 6.2 Sensitivity Analysis ----
