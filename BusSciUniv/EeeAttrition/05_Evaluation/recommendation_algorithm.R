# RECOMMENDATION ALGORITHM ----


# 1. Creating a "Correlation Funnel" - This is a discretized correlation plot that enables us to visualize cohort relationships 
#  to the target variable, attrition. 
# 2. Using A Recommendation Algorithm Worksheet To Develop Strategies - This is key to having a structured approach to developing 
#  data-driven recommendations. It's also a communication tool for executives & process stakeholders to showcase the logic 
#  you used and to incorporate their input into the strategies.
# 3. Implementing Strategies Into R Code - Develop a function called recommend_strategies() that outputs 
#  strategies by employee. This function is integral to building A Shiny Web App

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or laters is installed. If not restart & install.packages("recipes") to update.


# Data
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



# 2.0 Correlation Analysis - Machine Readable ----
source("00_Scripts/plot_cor.R")

# 2.1 Recipes ----

# https://tidymodels.github.io/recipes/articles/Ordering.html
# While your project’s needs may vary, here is a suggested order of potential steps that should work for most problems:
#      
# Impute
# Individual transformations for skewness and other issues
# Discretize (if needed and if you have no other choice)
# Create dummy variables
# Create interactions
# Normalization steps (center, scale, range, etc)
# Multivariate transformation (e.g. PCA, spatial sign, etc)

train_readable_tbl %>% glimpse()
# Numeric features need to be binned to compare cohorts within the population. Discretation is the process of converting
#  numeric data to factors via binning

factor_names <- c("JobLevel", "StockOptionLevel")

# Recipe - copy from 03_Data_Preparation - data_preparation_part_2_machine_readable.R to get started

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
     step_zv(all_predictors()) %>% 
     step_num2factor(factor_names) %>% 
     step_discretize(all_numeric(), options = list(min_unique = 1)) %>% 
     step_dummy(all_nominal(), one_hot = TRUE) %>% 
     prep()
# one_hot provides a column for every category when dummying variables versus the default of one less column 
#  than the number of variables  This is very beneficial for correlation analysis interpretability
#(It does give some warnings. . . ok in this case b/c there is not enought data for breaks)

train_corr_tbl <- bake(recipe_obj, new = train_readable_tbl)
train_corr_tbl %>% glimpse()

# Need to understand binning strategy
# tidy() is a context-specific function that converts non-data-frame objects to data frames (tibbles).  
# From broom package.  recipes has a special impelementation of tidy()

tidy(recipe_obj)#provides overall heirarchy of recipe object

tidy(recipe_obj, 3)# number returns the details of the operation in the recipe heirarchy provided by tidy(recipe_obj)

# 2.2 Correlation Visualization ----

# Data Manipulation

train_corr_tbl %>%  glimpse()

corr_level <- 0.06

correlation_results_tbl <- train_corr_tbl %>%
     select(-Attrition_No) %>%
     get_cor(Attrition_Yes, fct_reorder = T, fct_rev = T) %>%
     filter(abs(Attrition_Yes) >= corr_level) %>%
     mutate(
          relationship = case_when(
               Attrition_Yes > 0 ~ "Supports",
               TRUE ~ "Contradicts"
          )
     ) %>%
     mutate(feature_text = as.character(feature)) %>%
     separate(feature_text, into = "feature_base", sep = "_", extra = "drop") %>%
     mutate(feature_base = as_factor(feature_base) %>% fct_rev()) #as.factor converts in the order the data appears in the dataset

#Cool way to see factor ranks 0 try with/without the fct_rev() above
correlation_results_tbl %>% 
     select(feature_base) %>% 
     mutate(level = as.numeric(feature_base)) # as.numeric useful for getting the number associated with a factor
     
# Create Visualization

length_unique_groups <- correlation_results_tbl %>% 
     pull(feature_base) %>% 
     unique() %>% 
     length()

correlation_results_tbl %>% 
     ggplot(aes(Attrition_Yes, feature_base, color = relationship)) +
     geom_point() +
     geom_label(aes(label = feature), vjust = -0.5) +
     expand_limits(x = c(-0.3, 0.3),y = c(1, length_unique_groups + 1)) +
     theme_tq() +
     scale_color_tq() +
     labs(
          title = "Correlatiton Analysis: Recommendation Strategy Development",
          subtitle = "Discretizing features to help identify a strategy")
# Explain the plot:
# Overtime:  Overtime greatly influcens people leave.
# JobLevel:  Potential stratey - move resources from Level 1 to Level 2 to significantly risk them leaving

# 3.0 Recommendation Strategy Development Worksheet ----

# Copy information from worksheet to get started with the analysis

# 4.0 Recommendation Algorithm Development ----
#    This is where data scientists are important.  They apply critical thinking to develop a coded
#    algorithm combining data (data-driven) and knwoledge of a system(business-driven).  This
#    recomemndation algorithm becomes valuable to the busines sbecause once deployed, it drives
#    decision making from process owners (in this case Managers)

# 4.1 Personal Development (Mentorship, Education) ----

# YearsAtCompany
#  YAC - High - Likely to stay / YAC - Low - Likely to leave
#  Tie promotion if Low to advance faster / Mentor if YAC low

# TotalWorkingYears
#  TWY - High - Likely to stay /TWY - Low - Likely to leave
#  Tie Low TWY to training & formation/mentorship

# YearsInCurrentROle
#  More time in current role related to lower attrition
#  Incentivize specialization or promote - more professional than personal - its a mix so modified below
#  Incentivize specialization or promote / Mentorship Role

# JobInvolvement
#  High JI - likely to stay / low JI likely to leave
#  Create personal devleopment plan if low / High seek leadership role

# JobSatisfaction
#  Low JS more likely to leave
#  Low - personal development plan / High - Mentorship roles

# PerformanceRating - ADDED
#  If low, target for personal development plan / High possible mentor or leader
#  If you know features should be included based of your knowledge of the business, add them

# Good Better Best Approach

# (Worst Case) Create Personal Development plan:  Job Satisfaction, Job Involvement, ADDED PerformanceRating

# (Better Case) Promote training and formation:  YearsAtCompany, TotalWorkingYears
#  Not using YearsInCurrentRole is part of the 2 selected above

# (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, ADDED PerformanceRating, JobSatisfaction
#  Some people make great mentors, others are good leaders)
#  Mentors tend to be knowledgable, experienced and have good satisfaction metrics.

# (Best Case 2) Seek Leadership Role:  JobInvolvement, JobSatisfaction, ADDED PerformanceRating
#  Leaders tend to be very involved and have good performance metrics

# Below - Progressive Targeting:  The logic based system uses an if-style evalauation progessively isolating the
# most concerning groups to the least concerning groups

train_readable_tbl %>% 
     select(YearsAtCompany, TotalWorkingYears, YearsInCurrentRole, JobInvolvement, 
            JobSatisfaction, PerformanceRating) %>% 
     mutate_if(is.factor, as.numeric) %>% 
     mutate(
          personal_development_strategy = case_when(
               # (Worst Case) Create Personal Development plan:  Job Satisfaction, Job Involvement, ADDED PerformanceRating
               PerformanceRating == 1 |
                    JobSatisfaction == 1 |
                    JobInvolvement <= 2  # In Correlation Analysis Plot, Low and Medium supprted attrition
               ~ "Create Personal Development Plan",
               
               # (Better Case) Promote training and formation:  YearsAtCompany, TotalWorkingYears
               #  Not using YearsInCurrentRole is part of the 2 selected above
               # Need to know what is in the bin1, see below
               YearsAtCompany < 3 |
                    TotalWorkingYears < 6
               ~ "Promote Training and Formation",
                    
               # (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, ADDED PerformanceRating, JobSatisfaction
               #  Some people make great mentors, others are good leaders)
               #  Mentors tend to be knowledgable, experienced and have good satisfaction metrics.
               #  Typically someone with 3-5 years has sufficient knowledge to become a mentor - this drive the comparison vlaue below
               (YearsInCurrentRole > 3 | YearsAtCompany >= 5) & 
                    PerformanceRating >=3 &
                    JobSatisfaction ==4
               ~ "Seek Mentorship Role",
               
               # (Best Case 2) Seek Leadership Role:  JobInvolvement, JobSatisfaction, ADDED PerformanceRating
               #  Leaders tend to be very involved and have good performance metrics
               #  We want to identify leaders early.  The main difference between mentors and leaders is leaders do no thave a time component.
               #  Leaders have high JobInvolvement which is a measure of engagement.
               JobInvolvement >= 3 &
                    JobSatisfaction >= 3 &
                    PerformanceRating >= 3
               ~ "Seek Leadership Role",
               # When JI, JS and PR all >=4, only one record returned - too agressive - vary to achive results that satisfy busniess domain knowledge
               
               # Catch All
               TRUE ~ "Retain and Maintain"
          )
     ) # %>% 
     pull(personal_development_strategy) %>% table() # Many assigned to Personal Devleopment Plan - may be too much to manage. Use business domain knowledge to 
#                                                    change case_when logic to reduce if appropriate (308 if JobInvolvement == 1)

# Relate factor label to underlying numeric value
train_readable_tbl %>% pull(JobInvolvement) %>% levels()

# Discover the bins information
tidy(recipe_obj, number = 3) %>% 
     filter(str_detect(terms, "YearsAtCompany"))
tidy(recipe_obj, number = 3) %>% 
     filter(str_detect(terms, "TotalWorkingYears"))
tidy(recipe_obj, number = 3) %>% 
     filter(str_detect(terms, "YearsInCurrent"))
tidy(recipe_obj, number = 3) %>% 
     filter(str_detect(terms, "YearsAtCompany"))

# 4.2 Professional Development (Promotion Readiness) ----

# Focused on promotion, rotation and specialization in their job
# Rotation keeps people fresh, excited and helps understand multilple aspects of the business
# Specialization focuses on vecoming the best specialist.  Often for highly technical people that love their job
#    (high satisfaction) but are not interested in leadership

# JobLevel
#    Higher performers in entrly level positions (Level 1) are leaving.  Job Level 2 are staying.	
#    Promote faster for high performng employees

# YearsAtCompany (YAC)
#    YAC - High - Likely to stay / YAC - Low - Likely to leave	
#    Tie promotion if Low to advance faster / Mentor if YAC low


# YearsInCurrentRole (YiCR)	
#    More time in current role related to lower attrition	
#    Incentivize specialization or promote
#    Preferred as main drive over YAC

# Additional Features
     # JobInvolvement - important for promotion readiness, incentivizes involvement for leaders and early promotion
     # JobSatisfaction - Important for specialization, incentivizes satisfaction for mentors
     # PerformanceRating - Important for any promotion

# Good Better Best Approach
#  Typical Career Paths:  1 - Leadership (management); 2- SPecialist (Technical)
#  Both should have promotion schedules based on clear metrics

# Ready for rotation: YearsInCurrentRole, JobInvovement, PerformanceRating (LOW)

# Ready for Promotion Level 2:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating

# Ready for Promotion Level 3:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating

# Ready for Promotion Level 4:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating

# Ready for Promotion Level 5:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating

# Incentives Specialization:  YearsInCurrentRole, JobInvovement, PerformanceRating 

# Implement strategy into code:
train_readable_tbl %>% 
     select(JobLevel, YearsInCurrentRole, JobInvolvement, JobSatisfaction, PerformanceRating) %>% 
     mutate_if(is.factor, as.numeric) %>% 
     mutate(
          professional_development_strategy = case_when(
          # Progressively make it more difficult to beome promoted.  This is a good thing because it will incentivize
          # the attribute of performance metrics that are important to the business.  These key characteristics should be communicated
          # to employees during performance reviews.
               
               # Ready for rotation: YearsInCurrentRole, JobInvovement, PerformanceRating (LOW)
               YearsInCurrentRole >= 2 & JobSatisfaction <=2 ~ "Ready for Rotation",
               
               # Ready for Promotion Level 2:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating
               JobLevel == 1 & YearsInCurrentRole >= 2 & JobInvolvement >= 3 & PerformanceRating >= 3 ~ "Ready for Promotion",
               
               # Ready for Promotion Level 3:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating
               JobLevel == 2 & YearsInCurrentRole >= 2 & JobInvolvement >= 4 & PerformanceRating >= 3 ~ "Ready for Promotion",
               
               # Ready for Promotion Level 4:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating
               JobLevel == 3 & YearsInCurrentRole >= 3 & JobInvolvement >= 4 & PerformanceRating >= 3 ~ "Ready for Promotion",
               # Only a few added at this point but that is probaly OK, pretty high up in the company
               
               # Ready for Promotion Level 5:  JobLevel, YearsInCurrentRole, JobInvovement, PerformanceRating
               JobLevel == 4 & YearsInCurrentRole >= 4 & JobInvolvement >= 4 & PerformanceRating >= 3 ~ "Ready for Promotion",
               # Only a few added at this point but that is probaly OK, pretty high up in the company
               
               # Incentives Specialization:  YearsInCurrentRole, JobInvovement, PerformanceRating 
               YearsInCurrentRole >= 4 & JobSatisfaction >= 4 & PerformanceRating >= 3 ~ "Incentivizes Specialization",
               # These resources really like waht they do
               
               # Catch All
               TRUE ~ "Retain and Maintain"
          )
     ) #%>% pull(professional_development_strategy) %>% table() 

# Relate factor label to underlying numeric value
train_readable_tbl %>% pull(JobInvolvement) %>% levels()

# Discover the bins information
tidy(recipe_obj, number = 3) %>% 
     filter(str_detect(terms, "YearsInCurrentRole"))

# 4.3 Work Environment Strategy ----
# Overtime
#  Employees with high OT are leaving
#  Reduce OT - work life balance

# EnvironmentSatisfaction	
  # Employees with low environment satifaction are more likely to leave	
  # Improve workplace environment - review job assignment after a period of time in current role

# WorkLifeBalance
#  Bad worklife balance - more likely to leave	
#  Improve worklife balance

# BusinessTravel	
#  More business travel - more likely to leave / less travel - more likely to stay	
#  Reduce business travel where possible

# DistanceFromHome
#  High distance from home - more likely to leave	
#  Monitor work/life balance - Monitor with business travel strategy

# Additional Features
# JobInvolvement - 
#    important for reviewing a job assignment to give sufficient time in a role (min 2 years)
# YearsInCurrentRole - Important in keeping work environment satisfaction (Target Medim & Low)
#    This one was not captured in the other routines so decided it was important to capture

# Good Better Best Approach

# Improve work life: Overtime, WorklifeBalance
# Monitor Business Travel:  BusinessTravel, DistanceFromHome, WorkLifeBalance
# Review Job Assignment:  EnvironmentSatisfaction, YearsInCurrentRole
# Promote Job Engagement:  JobInvolvement - just a rational addition

# Implement strategy into code

train_readable_tbl %>% 
     select(OverTime, EnvironmentSatisfaction, WorkLifeBalance, BusinessTravel, DistanceFromHome, YearsInCurrentRole, JobInvolvement) %>% 
     mutate_if(is.factor, as.numeric) %>% 
     mutate(
          work_environment_strategy = case_when(
               # Improve work life: Overtime, WorklifeBalance
               OverTime ==2 | WorkLifeBalance ==1 ~ "Improve Work-Life Balance", 
               #Recall Overtime 2 == YES: train_readable_tbl %>% pull(OverTime) %>% levels()
               
               # Monitor Business Travel:  BusinessTravel, DistanceFromHome, WorkLifeBalance
               (BusinessTravel ==3 | DistanceFromHome >= 10) & WorkLifeBalance ==2 ~ "Monitor Business Travel",
               # WorkLifeBalance = 2 to differentiate from the case_when above
               
               # Review Job Assignment:  EnvironmentSatisfaction, YearsInCurrentRole
               EnvironmentSatisfaction ==1 & YearsInCurrentRole >=2 ~ "Review Job Assignment",
               # Give the new employee enough time to adjust therefore the 2 years
               
               # Promote Job Engagement:  JobInvolvement - just a rational addition
               JobInvolvement <=2 ~ "Promote Job Involvement",

               # Catch All
               TRUE ~ "Retain and Maintain"
          )
     ) %>% count(work_environment_strategy)

# Relate factor label to underlying numeric value
train_readable_tbl %>% pull(OverTime) %>% levels()
train_readable_tbl %>% pull(WorkLifeBalance) %>% levels()
train_readable_tbl %>% pull(BusinessTravel) %>% levels()

# Discover the bins information
tidy(recipe_obj, number = 3) %>% 
     filter(str_detect(terms, "DistanceFromHome"))

# 5.0 Recommendation Function ----

data <-  train_readable_tbl
employee_number <- 19

recommend_function <- function(data, employee_number){
     
     data %>% 
          filter(EmployeeNumber == employee_number) %>% 
          mutate_if(is.factor, as.numeric) %>%
          
          #Personal Development Strategy
          mutate(
               personal_development_strategy = case_when(

                    PerformanceRating == 1 | JobSatisfaction == 1 | JobInvolvement <= 2  
                    ~ "Create Personal Development Plan",

                    YearsAtCompany < 3 | TotalWorkingYears < 6
                    ~ "Promote Training and Formation",
                    
                    (YearsInCurrentRole > 3 | YearsAtCompany >= 5) & PerformanceRating >=3 & JobSatisfaction ==4
                    ~ "Seek Mentorship Role",
                    
                    JobInvolvement >= 3 & JobSatisfaction >= 3 & PerformanceRating >= 3
                    ~ "Seek Leadership Role",

                    # Catch All
                    TRUE ~ "Retain and Maintain"
               )
          ) %>% 
          # select(EmployeeNumber, personal_development_strategy)
          
          # Professional Development Strategy
          mutate(
               professional_development_strategy = case_when(

                    YearsInCurrentRole >= 2 & JobSatisfaction <=2 ~ "Ready for Rotation",

                    JobLevel == 1 & YearsInCurrentRole >= 2 & JobInvolvement >= 3 & PerformanceRating >= 3 ~ "Ready for Promotion",

                    JobLevel == 2 & YearsInCurrentRole >= 2 & JobInvolvement >= 4 & PerformanceRating >= 3 ~ "Ready for Promotion",

                    JobLevel == 3 & YearsInCurrentRole >= 3 & JobInvolvement >= 4 & PerformanceRating >= 3 ~ "Ready for Promotion",

                    JobLevel == 4 & YearsInCurrentRole >= 4 & JobInvolvement >= 4 & PerformanceRating >= 3 ~ "Ready for Promotion",

                    YearsInCurrentRole >= 4 & JobSatisfaction >= 4 & PerformanceRating >= 3 ~ "Incentivizes Specialization",

                    # Catch All
                    TRUE ~ "Retain and Maintain"
               )
          ) %>% 
           # select(EmployeeNumber, personal_development_strategy, professional_development_strategy)
          
          # Work Environment Strategy
          mutate(
               work_environment_strategy = case_when(

                    OverTime ==2 | WorkLifeBalance ==1 ~ "Improve Work-Life Balance", 

                    (BusinessTravel ==3 | DistanceFromHome >= 10) & WorkLifeBalance ==2 ~ "Monitor Business Travel",
                    # WorkLifeBalance = 2 to differentiate from the case_when above

                    EnvironmentSatisfaction ==1 & YearsInCurrentRole >=2 ~ "Review Job Assignment",

                    JobInvolvement <=2 ~ "Promote Job Involvement",
                    
                    # Catch All
                    TRUE ~ "Retain and Maintain"
               )
          )%>% 
          select(EmployeeNumber, personal_development_strategy, professional_development_strategy, work_environment_strategy)
}

# Validate FRecommend Function

train_readable_tbl %>% select(EmployeeNumber)

train_readable_tbl %>% recommend_function(1)
train_readable_tbl %>% recommend_function(2)
train_readable_tbl %>% recommend_function(4)

train_readable_tbl %>% recommend_function(3)# There is no employee3 - need to provide error capture

test_readable_tbl %>% select(EmployeeNumber)
test_readable_tbl %>% recommend_function(1767)



