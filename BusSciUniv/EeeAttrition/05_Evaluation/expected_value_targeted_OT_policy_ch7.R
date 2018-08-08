# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(imager)


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
automl_leader <- h2o.loadModel("04_Modeling/h2o_models/GBM_grid_0_AutoML_20180806_163444_model_4")

#automl_leader <- h2o.loadModel("CourseWork/BusSciUniv/EeeAttrition/04_Modeling/h2o_models/GBM_grid_0_AutoML_20180806_163444_model_4")

automl_leader


# 3. Primer: Working With Threshold & Rates ----

performance_h2o <- automl_leader %>%
    h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
    h2o.confusionMatrix()
# The rows are actuals No and Yes; the columns are predicted No and Yes
#  The False Negatives in Yes row / No column are the most expensive to miss incorrectly - Predicted to STAY but actually LEAVES
#  False Positives are not so damaging - predict someone will leave but actually stays - no harm done
# The F1 threshold in the output determines the mix in the confusion matrix
#   F1 is the optimal balance between Precision and Recall.  However, the threshold @ Max F1 is not typically the optimal value 
#   for the business case because FNs are typically more costly then FPs.

rates_by_threshold_tbl <- performance_h2o %>%
    h2o.metric() %>%
    as.tibble()

rates_by_threshold_tbl %>% glimpse()
# want to minimize false negatives

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>%
    mutate(key = fct_reorder2(key, threshold, value)) %>%
    ggplot(aes(threshold, value, color = key)) +
    geom_point() +
    geom_smooth() +
    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "right") +
    labs(
        title = "Expected Rates",
        y = "Value", x = "Threshold"
    )
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
            select(EmployeeNumber, MonthlyIncome, OverTime)
    )

predictions_with_OT_tbl


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

ev_with_OT_tbl
# Attrition Cost occurs only if the employee leaves.  The Yes column is the liklihood of leaving.  
# When combined, expected attrition cost results

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
    summarise(
        total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )

total_ev_with_OT_tbl


# 4.2 Calculating Expected Value With Targeted OT ----


max_f1_tbl <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

max_f1_tbl

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
        OverTime = case_when(
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
    rename(
        OverTime_0 = OverTime,
        OverTime_1 = OverTime1
    )

predictions_targeted_OT_tbl


avg_overtime_pct <- 0.10

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>%
    mutate(
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12,
            net_revenue_per_employee = 250000
        )
    ) %>%
    mutate(
        cost_of_policy_change = case_when(
            OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
            TRUE ~ 0
        )
    ) %>%
    mutate(
        cb_tn = cost_of_policy_change,
        cb_fp = cost_of_policy_change,
        cb_tp = cost_of_policy_change + attrition_cost,
        cb_fn = cost_of_policy_change + attrition_cost,
        expected_attrition_cost = 
            Yes * (tpr*cb_tp + fnr*cb_fn) +
            No *  (tnr*cb_tn + fpr*cb_fp)
    ) 
#cost only when OverTime_0 changes from Yes to No in OverTime_1
#Try w/o TRUE ~ 0 to learn

ev_targeted_OT_tbl

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>%
    summarize(
        total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )

total_ev_targeted_OT_tbl 

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
    total_ev_with_OT_tbl,
    total_ev_targeted_OT_tbl
) %>%
    mutate(
        savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
        pct_savings = savings / total_expected_attrition_cost_0
    )

savings_tbl

# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

# The function defaults is set up for a No OT Policy because the threshold is so low anyone with OT
# gets converted to No OT

data <- test_tbl
h2o_model <- automl_leader

calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {
     
     data_0_tbl <- as.tibble(data)
     
     # 4. Expected Value 
     # 4.1 Calculating Expected Value With OT 
     
     pred_0_tbl <- h2o_model %>%
          h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
          as.tibble() %>%
          bind_cols(
               data_0_tbl %>%
                    select(EmployeeNumber, MonthlyIncome, OverTime)
          )
     
     ev_0_tbl <- pred_0_tbl %>%
          mutate(
               attrition_cost = calculate_attrition_cost(
                    n = 1,
                    salary = MonthlyIncome * 12,
                    net_revenue_per_employee = 250000)) %>%
          mutate(cost_of_policy_change = 0) %>%
          mutate(
               expected_attrition_cost = 
                    Yes * (attrition_cost + cost_of_policy_change) +
                    No *  (cost_of_policy_change)
          )
     
     total_ev_0_tbl <- ev_0_tbl %>%
          summarise(
               total_expected_attrition_cost_0 = sum(expected_attrition_cost)
          )
     
     # 4.2 Calculating Expected Value With Targeted OT
     
     data_1_tbl <- data_0_tbl %>%
          add_column(Yes = pred_0_tbl$Yes) %>%
          mutate(
               OverTime = case_when(
                    Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
                    TRUE ~ OverTime
               )
          ) %>%
          select(-Yes) 
     
     pred_1_tbl <- h2o_model %>%
          h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
          as.tibble() %>%
          bind_cols(
               data_0_tbl %>%
                    select(EmployeeNumber, MonthlyIncome, OverTime),
               data_1_tbl %>%
                    select(OverTime)
          ) %>%
          rename(
               OverTime_0 = OverTime,
               OverTime_1 = OverTime1
          )
     
     avg_overtime_pct <- 0.10
     
     ev_1_tbl <- pred_1_tbl %>%
          mutate(
               attrition_cost = calculate_attrition_cost(
                    n = 1,
                    salary = MonthlyIncome * 12,
                    net_revenue_per_employee = 250000)
          ) %>%
          mutate(
               cost_of_policy_change = case_when(
                    OverTime_1 == "No" & OverTime_0 == "Yes" 
                    ~ attrition_cost * avg_overtime_pct,
                    TRUE ~ 0
               ))%>%
          mutate(
               cb_tn = cost_of_policy_change,
               cb_fp = cost_of_policy_change,
               cb_fn = attrition_cost + cost_of_policy_change,
               cb_tp = attrition_cost + cost_of_policy_change,
               expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
                    No * (tnr*cb_tn + fpr*cb_fp)
          )
     
     total_ev_1_tbl <- ev_1_tbl %>%
          summarise(
               total_expected_attrition_cost_1 = sum(expected_attrition_cost)
          )
     
     # 4.3 Savings Calculation
     
     savings_tbl <- bind_cols(
          total_ev_0_tbl,
          total_ev_1_tbl
     ) %>%
          mutate(
               savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
               pct_savings = savings / total_expected_attrition_cost_0
          )
     
     return(savings_tbl$savings)
}

# Test new function

# No OT Policy
calculate_savings_by_threshold(test_tbl, automl_leader, threshold = 0,
                               tnr = 0, fnr = 0, tpr = 1, fpr = 1)#Use plot to help set these values
# Do nothing policy - set threshold really high
calculate_savings_by_threshold(test_tbl, automl_leader, threshold = 1,
                               tnr = 1, fnr = 1, tpr = 0, fpr = 0)

#  Threshold @ Max F1
calculate_savings_by_threshold(test_tbl, automl_leader,
                               threshold = max_f1_tbl$threshold,
                               tnr = max_f1_tbl$tnr,
                               fnr = max_f1_tbl$fnr,
                               fpr = max_f1_tbl$fpr,
                               tpr = max_f1_tbl$tpr)

max_F1_savings <- calculate_savings_by_threshold(test_tbl, automl_leader,
                               threshold = max_f1_tbl$threshold,
                               tnr = max_f1_tbl$tnr,
                               fnr = max_f1_tbl$fnr,
                               fpr = max_f1_tbl$fpr,
                               tpr = max_f1_tbl$tpr)

# 5.2 Optimization with purrr----

smpl <- seq(1, 220, length.out = 20) %>% round(digits = 0)
# smpl just allows us to reduce computational time, 20 samples rather than all 220 records

partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)
# partial() is used to partially fill a function with statis arguments (preloading).  Useful for mapping arguments like "data"
# that never change during the iteration

rates_by_threshold_optimzed_tbl <- rates_by_threshold_tbl %>% 
     select(threshold, tnr:tpr) %>% 
     slice(smpl) %>% 
     mutate(
          savings = pmap_dbl(
               .l = list(
                    threshold = threshold,
                    tnr = tnr,
                    fnr = fnr,
                    fpr = fpr,
                    tpr = tpr),
               .f = partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)
          )
     )
# pmap interates over each of the columns in a list

rates_by_threshold_optimzed_tbl

rates_by_threshold_optimzed_tbl %>% 
     ggplot(aes(threshold, savings)) +
     geom_line(color = palette_light()[[1]]) +
     geom_point(color = palette_light()[[1]]) +
     
     # Optimal Point
     geom_point(shape = 21, size = 5, color = palette_light()[[3]],
                data = rates_by_threshold_optimzed_tbl %>% 
                     filter(savings == max(savings))) +
     geom_label(aes(label = scales::dollar(savings)),
                vjust = -1, color = palette_light()[[3]],#moves label up
                data = rates_by_threshold_optimzed_tbl %>% 
                     filter(savings == max(savings))) +
     # F1 Max
     geom_vline(xintercept = max_f1_tbl$threshold, color = palette_light()[[5]], size = 1.2) +
     annotate(geom = "label", label = scales::dollar(max_F1_savings),
              x = max_f1_tbl$threshold, y = max_F1_savings, vjust = -1, hjust = -.2, color = palette_light()[[1]]) +
     # No OT Policy
     geom_point(shape = 21, size = 5, color = palette_light()[[2]],
                data = rates_by_threshold_optimzed_tbl %>% 
                     filter(threshold == min(threshold))) +
     geom_label(aes(label = scales::dollar(savings)),
                vjust = -1, color = palette_light()[[2]],#moves label up
                data = rates_by_threshold_optimzed_tbl %>% 
                     filter(threshold == min(threshold))) +
     # Do nothing policy
     geom_point(shape = 21, size = 5, color = palette_light()[[2]],
                data = rates_by_threshold_optimzed_tbl %>% 
                     filter(threshold == max(threshold))) +
     geom_label(aes(label = scales::dollar(round(savings, 0))),
                vjust = -1, color = palette_light()[[2]],#moves label up
                data = rates_by_threshold_optimzed_tbl %>% 
                     filter(threshold == max(threshold))) +
     # Aesthetics
     theme_tq() +
     expand_limits(x = c(-0.1, 1.1), y = 4e5) +
     scale_x_continuous(labels = scales::percent, 
                        breaks = seq(0, 1, by = 0.2)) +
     scale_y_continuous(labels = scales::dollar) +
     labs(
          title = "Optimization Results: Expected Savings Maximized",
          x = "Threshold (%)", y = "Savings"
     )
# Value in lower right should be 0 ideally but the threshold does not have a value = 1 in the data

# Label amount of far left in red:  No OT Policy - anywhen doing OT will be changed to no OT (Yes to No) - everyone targeted.  
# It does reduce turnover by the amount shown (remeber, this is just a sample of the overall data)

# Label amount of left in green:  Optimization - Threshold at Max savings - Target employees by weighted analysis of cost of FN 
# and cost of FP (Predict to leave but stays - does not cost much).  FNs are more costly that FPs - this is the core problem 
# in this exercise - that causes the threshold < Max F1

# Label amount oin dark blue:  Max F1 Score - target employees by balancing FNs and FPs

# Label amount far right in dark blue:  Do nothing - Threshol d= 1 - Threshold is so high that no one is targeted - no one has a
# 100% chance of leaving - theortically saving = $0.00

# 6 Sensitivity Analysis ----

# So far we have made assumptions.  Some are:

# 1.  Net revenue/employee = $250k
# 2.  Average OT = 10%

# 6.1 Create calculate_savings_by_threshold_2() ----

# data <- test_tbl
# h20 <- automl_leader

calculate_savings_by_threshold_2 <- function(data, h2o_model, threshold = 0,
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             avg_overtime_pct = 0.10,
                                             net_revenue_per_employee = 250000) {
     data_0_tbl <- as.tibble(data)
     
     # 4. Expected Value 
     # 4.1 Calculating Expected Value With OT 
     
     pred_0_tbl <- h2o_model %>%
          h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
          as.tibble() %>%
          bind_cols(
               data_0_tbl %>%
                    select(EmployeeNumber, MonthlyIncome, OverTime)
          )
     
     ev_0_tbl <- pred_0_tbl %>%
          mutate(
               attrition_cost = calculate_attrition_cost(
                    n = 1,
                    salary = MonthlyIncome * 12,
                    # Changed in _2 ----
                    net_revenue_per_employee = net_revenue_per_employee) 
          ) %>%
          mutate(
               cost_of_policy_change = 0
          ) %>%
          mutate(
               expected_attrition_cost = 
                    Yes * (attrition_cost + cost_of_policy_change) +
                    No *  (cost_of_policy_change)
          )
     
     
     total_ev_0_tbl <- ev_0_tbl %>%
          summarise(
               total_expected_attrition_cost_0 = sum(expected_attrition_cost)
          )
     
     # 4.2 Calculating Expected Value With Targeted OT
     
     data_1_tbl <- data_0_tbl %>%
          add_column(Yes = pred_0_tbl$Yes) %>%
          mutate(
               OverTime = case_when(
                    Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
                    TRUE ~ OverTime
               )
          ) %>%
          select(-Yes) 
     
     pred_1_tbl <- h2o_model %>%
          h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
          as.tibble() %>%
          bind_cols(
               data_0_tbl %>%
                    select(EmployeeNumber, MonthlyIncome, OverTime),
               data_1_tbl %>%
                    select(OverTime)
          ) %>%
          rename(
               OverTime_0 = OverTime,
               OverTime_1 = OverTime1
          )
     
     avg_overtime_pct <- avg_overtime_pct # Changed in _2 ----
     
     ev_1_tbl <- pred_1_tbl %>%
          mutate(
               attrition_cost = calculate_attrition_cost(
                    n = 1,
                    salary = MonthlyIncome * 12,
                    # Changed in _2 ----
                    net_revenue_per_employee = net_revenue_per_employee)
          ) %>%
          mutate(
               cost_of_policy_change = case_when(
                    OverTime_1 == "No" & OverTime_0 == "Yes" 
                    ~ attrition_cost * avg_overtime_pct,
                    TRUE ~ 0
               ))%>%
          mutate(
               cb_tn = cost_of_policy_change,
               cb_fp = cost_of_policy_change,
               cb_fn = attrition_cost + cost_of_policy_change,
               cb_tp = attrition_cost + cost_of_policy_change,
               expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
                    No * (tnr*cb_tn + fpr*cb_fp)
          )
     
     total_ev_1_tbl <- ev_1_tbl %>%
          summarise(
               total_expected_attrition_cost_1 = sum(expected_attrition_cost)
          )
     
     # 4.3 Savings Calculation
     
     savings_tbl <- bind_cols(
          total_ev_0_tbl,
          total_ev_1_tbl
     ) %>%
          mutate(
               savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
               pct_savings = savings / total_expected_attrition_cost_0
          )
     
     return(savings_tbl$savings)
}

# 6.2 Sensitivity Analysis ----

max_savings_rate_tbl <- rates_by_threshold_optimzed_tbl %>% filter(savings == max(savings))

calculate_savings_by_threshold_2(
     data = test_tbl,
     h2o_model = automl_leader,
     threshold = max_savings_rate_tbl$threshold,
     tnr = max_savings_rate_tbl$tnr,
     fnr = max_savings_rate_tbl$fnr,
     fpr = max_savings_rate_tbl$fpr,
     tpr = max_savings_rate_tbl$tpr
     )

calculate_savings_by_threshold_preloaded <- partial(
     calculate_savings_by_threshold_2,
     # Function Arguments
     data = test_tbl,
     h2o_model = automl_leader,
     threshold = max_savings_rate_tbl$threshold,
     tnr = max_savings_rate_tbl$tnr,
     fnr = max_savings_rate_tbl$fnr,
     fpr = max_savings_rate_tbl$fpr,
     tpr = max_savings_rate_tbl$tpr
)
# If the cost/benefit change, the settings need to be recalibrated/re-optimzed

# Test
calculate_savings_by_threshold_preloaded(
     avg_overtime_pct = 0.10, 
     net_revenue_per_employee = 250000)


sensitivity_tbl <- list(
     avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
     net_revenue_per_employee = seq(200000, 400000, by = 50000)
) %>% 
     cross_df() %>% 
     # cross_df produces all combinations of a list - useful for grid search and sensitivty analysis
     mutate(
          savings = pmap_dbl(
               .l = list(
                    avg_overtime_pct = avg_overtime_pct,
                    net_revenue_per_employee = net_revenue_per_employee
               ),
               .f = calculate_savings_by_threshold_preloaded
          )
     )

sensitivity_tbl %>% 
     ggplot(aes(avg_overtime_pct, net_revenue_per_employee)) +
     geom_tile(aes(fill = savings)) +
     geom_label(aes(label = savings %>% round(0) %>% scales::dollar())) +
     theme_tq() +
     theme(legend.position = "none") +
     scale_fill_gradient2(
          low = palette_light()[[2]],
          mid = "white",
          high = palette_light()[[1]]
          #midpoint = mean(sensitivity_tbl$savings)
          ) + 
     scale_x_continuous(
          labels = scales::percent,
          breaks = seq(0.05, 0.30, by = 0.05)
     ) +
     scale_y_continuous(labels = scales::dollar) +
     labs(
          title = "Profitability: Expected Savings Sensitivity Analysis",
          subtitle = "How sensitive is savings to net revenue per employee and average overtime percentage?",
          x = "Average Overtime Percentage",
          y = "Net Revenue per Employee"
     )
# As OT increases, it get more expensive to hire new resources and project timelines lengthen
# Breakeven is the column that goes from positive to negative