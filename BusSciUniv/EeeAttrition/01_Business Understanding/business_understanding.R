
# BUSINESS UNDERSTANDING ----
source("00_Scripts/required_packages.R")

# Load Data
path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl <- read_excel(path_train, sheet = 1)

# Data Subset
dept_job_role_tbl <- train_raw_tbl %>% 
     select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

# 1. Business Science Problem Framework ----

# 1A.  View Business as a Machine ----

# BSU's:  Department and Job Role
# Define Objectives:  Retain high performers
# Assess Outcomes: TBD

dept_job_role_tbl %>% 
     
     group_by(Attrition) %>%  
     summarise(n = n()) %>% 
     ungroup() %>% 
     mutate(pct = n/sum(n))

# 16.1 % Attrition

# 1B:  Understand the Drivers ----

# Investigate the Objectives:  16% Attrition

# Synthesize Outcomes:  High counts and high percetnages (learned after hypothesizing drivers)

# Hypothesize Drivers:  Job Role and Departments

# By Department
dept_job_role_tbl %>% 
     
     group_by(Department, Attrition) %>%  
     summarise(n = n()) %>% 
     ungroup() %>% 
     
     group_by(Department) %>% 
     mutate(pct = n/sum(n))
# Might be something going on by department

# Job Role
dept_job_role_tbl %>% 
     
     group_by(Department, JobRole, Attrition) %>%  
     summarise(n = n()) %>% 
     ungroup() %>% 
     
     group_by(Department, JobRole) %>% 
     mutate(pct = n/sum(n)) %>% 
     ungroup() %>% 
     
     filter(Attrition %in% c("Yes"))
#  Appears attrition is affected by job role

# 1C: Measure the drivers ----

# Collect Information on Employee Attrition - Ongoing task 

# Develop KPIs: Industry KPIs
# 8.8% is the industry benchmark for utility companies

dept_job_role_tbl %>% 
     
     group_by(Department, JobRole, Attrition) %>%  
     summarise(n = n()) %>% 
     ungroup() %>% 
     
     group_by(Department, JobRole) %>% 
     mutate(pct = n/sum(n)) %>% 
     ungroup() %>% 
     
     filter(Attrition %in% c("Yes")) %>% 
     arrange(desc(pct)) %>% 
     mutate(above_industry_avg = case_when(
            pct > 0.088 ~ "Yes",
            TRUE ~ "No"))

# Note:  The case_when() steps are evaluated in order.  Always end with "TRUE ~ " + the
# value you want items not meeting the criteria above to get

# 1D: Uncover Problems and Opportunities ----

calculate_attrition_cost <-  function(
     
     n = 1,
     salary = 80000,
     
     # Direct Results
     separation_cost = 500,
     vacancy_cost = 10000,
     acquisition_cost = 4900,
     placement_cost = 3500,
     
     # Productivity Costs
     net_revenue_per_employee = 250000,
     workdays_per_year = 240,
     workdays_position_open = 40,
     workdays_onboarding = 60,
     onboarding_efficiency = 0.50
     
){
     # The code below is the R interpretation of the Excel Spreadsheet
     direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
     
     productivy_cost <- net_revenue_per_employee / workdays_per_year *
          (workdays_position_open + workdays_onboarding * onboarding_efficiency)
     
     salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
     
     cost_per_employee <- direct_cost + productivy_cost - salary_benefit_reduction
     
     total_cost <- n * cost_per_employee
     
     return(total_cost)
}

# Calculate Cost by Job Role ----

dept_job_role_tbl %>% 
     
     group_by(Department, JobRole, Attrition) %>%  
     summarise(n = n()) %>%                       # replace with dplyr::count
     ungroup() %>% 
     
     group_by(Department, JobRole) %>% 
     mutate(pct = n/sum(n)) %>%                  # create count_to_pct()
     ungroup() %>% 
     
     filter(Attrition %in% c("Yes")) %>% 
     arrange(desc(pct)) %>% 
     mutate(above_industry_avg = case_when(     # create assess_attrition()
          pct > 0.088 ~ "Yes",
          TRUE ~ "No")) %>% 
     
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000))

# 1st use dplyr::count to replace code chunk above

dept_job_role_tbl %>% 
     
     count(JobRole, Attrition) %>% 
     
     group_by(JobRole) %>% 
     mutate(pct = n/sum(n)) %>%                  # create count_to_pct()
     ungroup() %>% 
     
     filter(Attrition %in% c("Yes")) %>% 
     arrange(desc(pct)) %>% 
     mutate(above_industry_avg = case_when(     # create assess_attrition()
          pct > 0.088 ~ "Yes",
          TRUE ~ "No")) %>% 
     
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000))

#Create functions to replace the code above

# count_to_pct

count_to_pct <- function(data, ..., col = n){
     
     grouping_vars_expr <- quos(...)
     col_expr <- enquo(col)
     
     ret <- data %>% group_by(!!! grouping_vars_expr) %>% 
          mutate(pct = (!! col_expr)/sum(!! col_expr)) %>% ungroup()
                 
     return(ret)     
}

dept_job_role_tbl %>% 
     
     count(Department, JobRole, Attrition) %>%
     
     count_to_pct(Department, JobRole) %>% 
     
     filter(Attrition %in% c("Yes")) %>% 
     arrange(desc(pct)) %>% 
     mutate(above_industry_avg = case_when(     # create assess_attrition()
          pct > 0.088 ~ "Yes",
          TRUE ~ "No")) %>% 
     
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000))

# assess_attrition

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct){
     
     attrition_col_expr <- enquo(attrition_col)
     
     data %>% filter((!! attrition_col_expr) %in% attrition_value) %>% 
          arrange(desc(pct)) %>% 
          mutate(above_industry_avg = case_when(
               pct > baseline_pct ~ "Yes", 
               TRUE ~ "No"))
}

dept_job_role_tbl %>% 
     
     count(Department, JobRole, Attrition) %>% 
     
     count_to_pct(Department, JobRole) %>% 
     
     assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
          
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000))

# Visualization of Attrition Cost ----

dept_job_role_tbl %>% 
     
     count(Department, JobRole, Attrition) %>% 
     
     count_to_pct(Department, JobRole) %>% 
     
     assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
     
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)) %>% 

     # Data Manipulation 
     mutate(name = str_c(Department, JobRole, sep = ": ") %>%  as_factor()) %>% #must be factors to order
     mutate(name = fct_reorder(name, cost_of_attrition)) %>% # orders the names by cost
     mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2),
                              "M", sep = "")) %>% 
     # Plotting
     ggplot(aes(x = cost_of_attrition, y = name)) +
     geom_segment(aes(xend = 0, yend = name), color = palette_light()[[1]]) +
     geom_point(aes(size = cost_of_attrition), color = palette_light()[[1]]) +
     scale_x_continuous(label = scales::dollar) +
     geom_label(aes(label = cost_text, size = cost_of_attrition), 
                hjust = "inward", color = palette_light()[[1]]) + 
     theme_tq() +
     scale_size(name = c(4,5)) +
     labs(title = "Estimated Cost of Attrition: By Dept and Job Role",
          y = "", x = "Cost of Attrition") +
     theme(legend.position = "none")
     
# plot_attrition()

plot_attrition <- function(data, ..., .value, 
                           fct_reorder = TRUE,
                           fct_rev = FALSE,
                           include_lbl = TRUE,
                           color = palette_light()[[1]],
                           units = c("0", "K", "M")) {
# Inputs

     group_vars_expr <- quos(...)
     if(length(group_vars_expr) == 0)
          group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
          # rlang::sym turns a single character string into an expression.  
          # The expression is typically captured in enquo() or quos() to delay evaluation.
     
     value_expr <- enquo(.value)# for data manipulation
     value_name <- quo_name(value_expr)# for ggplot - makes a character string in quotes
     
     units_val <- switch(units[[1]],
                         "M" = 1e6,
                         "K" = 1e3,
                         "0" = 1)
     if (units[[1]] == "0") units <- "" #Make it empty because it would get appended to labels in the plot
     
# Data Manipulation
     
     #Function factory - a function that produces a function
     usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)

     data_manipulated <- data %>% 
          mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>% 
          #Above () removed around !!! group_vars_expr otherwise str_c sees it a single input
          mutate(value_text = str_c(usd(!! value_expr / units_val),
                                   units[[1]], sep = ""))
     
     if(fct_reorder){data_manipulated <- data_manipulated %>% 
          mutate(name = fct_reorder(name, (!! value_expr))) %>% 
          arrange(name)}
     
     if(fct_rev){data_manipulated <- data_manipulated %>% 
          mutate(name = fct_rev(name)) %>% 
          arrange(name)}
          
# Visualization
# NOTE:  ggplot does not work with tidyeval framework - requires use of aes_string
     
     g <- data_manipulated %>% 
          ggplot(aes_string(x = value_name, y = "name")) +
          geom_segment(aes(xend = 0, yend = name), color = color) +
          geom_point(aes_string(size = value_name), color = color) +
          scale_x_continuous(label = scales::dollar) +
          theme_tq() +
          scale_size(name = c(4,5)) +
          theme(legend.position = "none")
     
     if(include_lbl){
          g <-  g +
               geom_label(aes_string(label = "value_text", size = value_name), 
                          hjust = "inward", color = color)
     }
     return(g)
} 

# Test plot code

dept_job_role_tbl %>% 
     
     count(Department, JobRole, Attrition) %>% 
     count_to_pct(Department, JobRole) %>% 
     assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)) %>% 
     plot_attrition(Department, JobRole, .value = cost_of_attrition, units = "M")
     
dept_job_role_tbl %>% 
     
     count(JobRole, Attrition) %>% 
     count_to_pct(JobRole) %>% 
     assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)) %>% 
     plot_attrition(JobRole, .value = cost_of_attrition, units = "M") +
     labs(
          title = "Estimated Cost of Attrition by Job Role",
          x = "Cost of Attrition",
          subtitle = "Appears Sales Executive and Laboratory Technician are the biggest drivers of cost",
          y = ""
     )

     