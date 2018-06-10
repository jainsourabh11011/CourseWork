# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")


# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("Challenges/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl

# Q1: Which Job Role has the highest total cost of attrition? ----
dept_jobrole_productivty_tbl <- dept_jobrole_tbl %>% 
     count(Department, JobRole, Attrition) %>% 
     count_to_pct(Department, JobRole) %>% 

     left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>% 
     
     assess_attrition(Attrition,attrition_value = 'Yes',baseline_pct = 0.088) %>%
     
     mutate(attrition_cost = calculate_attrition_cost(n = n, salary = Salary_Average,
                                                      net_revenue_per_employee = Revenue_Average))

dept_jobrole_productivty_tbl %>% plot_attrition(Department, JobRole, .value = attrition_cost)

#ANSWER - Sales Executive

# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----
dept_jobrole_productivty_tbl %>% plot_attrition(Department, JobRole, .value = attrition_cost, units = "M")

# ANSWER - $2.28M

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----
dept_jobrole_tbl %>% 
     
     count(JobRole,Attrition) %>% 
     count_to_pct(JobRole) %>%
     left_join(productivity_cost_by_role_tbl,by=c('JobRole')) %>%
     assess_attrition(Attrition,attrition_value = 'Yes',baseline_pct = 0.088) %>%
     mutate(cost_of_attrition = calculate_attrition_cost(n = n, 
                                                         salary = Salary_Average,net_revenue_per_employee=Revenue_Average)) %>%
     arrange(desc(cost_of_attrition)) %>%
     mutate(total=sum(cost_of_attrition)) %>%
     top_n(4,wt = cost_of_attrition) %>%
     summarise(cost_of_attrition=sum(cost_of_attrition)/unique(total))

# OR

dept_jobrole_productivty_tbl %>% 
     
     arrange(desc(attrition_cost)) %>%
     mutate(cumulative_attrition = cumsum(attrition_cost)) %>%
     mutate(cumulative_percent = cumulative_attrition / sum(attrition_cost)) %>%
     select(Department, JobRole, n, attrition_cost:cumulative_percent)

# ANSWER 86.1%

# Q4. Which Department has the highest total cost of attrition? ----
dept_productivty_tbl <- dept_jobrole_tbl %>% 
     count(Department, JobRole, Attrition) %>% 
     count_to_pct(Department, JobRole) %>% 
     assess_attrition(Attrition, "Yes", kpi_industry_turnover_pct) %>% 
     
     left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>% 
     
     group_by(Department) %>% 
     summarise(n = sum(n),
               Salary_Average = sum(Salary_Average),
               Revenue_Average = sum(Revenue_Average)) %>% 
     
     mutate(attrition_cost = 
                 calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average))

dept_productivty_tbl %>% plot_attrition(Department, .value = attrition_cost)

# ANSWER Research & Development

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----

dept_productivty_tbl %>% 
     count_to_pct(col = attrition_cost) %>%
     arrange(desc(pct))

# ANSWER 67.4%