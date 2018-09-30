# DATA UNDERSTANDING ----

# Libraries

pkgs <- c(
     "tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr
     "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
     "GGally",     # Data understanding - visualizations
     "skimr",      # Data understanding - summary information
     "readxl"      # Reading excel files
)

if(!require(easypackages)){install.packages("easypackages")}
library(easypackages)
packages(pkgs, prompt = TRUE)

# Load Data
path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

glimpse(train_raw_tbl)

# Descriptive Features:  Age, DistanceFromHome, Gender, MaritalStatus, NumCompaniesWorked, Over18
## Describes the individual

# Employment Features:  Department, EmployeeCount, EmployeeNumber, JobInvolvement, JobLevel, JobRole, JobSatisfaction
## Relate to employment

# Compensation Features:  DailyRate, HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel
## Linked to compensation

# Survey Results:  EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance
## From Actual Surveys

# Performance Data:  JobInvolvement, PerformanceRating
## Performrance Data

# Work-Life Features:  BusinessTravel, Overtime

# Training and Education:  Education, EducationField, TrainingTimesLastYear

# Time-Based Features:  TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrentManager

# Breakdown data collection activities into strategic areas

# EDA ----

# Step 1: Data Summarization ----

skim(train_raw_tbl)

# Character Data Type

train_raw_tbl %>% select_if(is.character) %>% glimpse()

train_raw_tbl %>% select_if(is.character) %>% map(unique)

train_raw_tbl %>% select_if(is.character) %>% map(table)

# Anonymous function with ~  
train_raw_tbl %>% select_if(is.character) %>% map(~ table(.) %>% prop.table()) 

# Numeric Data

train_raw_tbl %>% select_if(is.numeric) %>% map(~ unique(.) %>% length())

train_raw_tbl %>% select_if(is.numeric) %>% map_df(~ unique(.) %>% length()) %>% 
     gather() %>% arrange(value) %>% filter(value <=10)# might be nonessential or categorical

# Step2: Data Visualization ----

# Note below all the vaitables are the Descriptinve Feature identified earlier
train_raw_tbl %>% select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, 
                         DistanceFromHome) %>% 
     ggpairs(aes(color = Attrition), lower = "blank", legend = 1, 
             diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
     theme(legend.position = "bottom")

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5){
     
     color_expr <- enquo(color)
     
     if(rlang::quo_is_null(color_expr)){
          gdata %>% ggpairs(lowe = "blank")
     } else {
          color_name <- quo_name(color_expr)
          
          g <- data %>%
               ggpairs(mapping = aes_string(color = color_name),
                       lower = "blank", legend = 1,
                       diag = list(continuous = wrap("densityDiag",
                                                   alpha = density_alpha))) +
              theme(legend.position = "bottom")
     }
     return(g)
}     
# NOTE:  enquo works inside a function.  At the console, must use quo, not enquo 

# Test new function
train_raw_tbl %>% select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, 
                         DistanceFromHome) %>% 
     plot_ggpairs(color = Attrition)
  
# Explore Features by Category

#  1. Decriptive Features: age, gender, marital status, etc

train_raw_tbl  %>% select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, 
                        DistanceFromHome) %>% 
     plot_ggpairs(Attrition)
# Observations:  
#  AgeYounger people seem to leave more thna older workers.  
#  DistanceFromHome appears skewed - furhter away leave more frequently
#  Num Companies Worked appears more skewed for workerers that have had more jobs
#  Marital Status interesting:  No goes small large medium where Yes goes small medium large (height of histogram bars)

#  2. Employment Features

train_raw_tbl %>% select(Attrition, contains("employee"), contains("department"), contains("job")) %>% 
     plot_ggpairs(Attrition)
# contains is NOT case sentitive

# Observations:
#  EmployeeCount and EmployeeNumber are not useful - will not be used in model
#  Department:  Higher proportion evident in Dept-Dept histogram in far right histogram bar
#  JobInvolvement:  Density Plot shows higher 3rd spike and 4th to less extent - more likely to stay
#  JobLevel:  Density plot shows deviation
#  JobRoles - some roles appear to have higher attrition rate by evaluating the histogram
#  JobStatisfaction:  1 - higher density of attrition than others

#  3. Compensation Features

train_raw_tbl %>% select(Attrition, contains("income"), contains("rate"), contains("salary"), 
                         contains("stock")) %>% 
     plot_ggpairs(Attrition)

# Observations:
#  MonthlyRate: Those that are leaving have a lower Monthly Income
#  PercentSalaryHike:  It's difficult to deduce anything based on the visualization
#  StockOptionLevel:  Those that are staying have a higher stock option level 


#  4. Survey Results

train_raw_tbl %>% select(Attrition, contains("satisfaction"), contains("life")) %>% 
     plot_ggpairs(Attrition)

# Observations:
#  EnvironmentSatisfaction:  A higher proportion of those leaving have a low environment satisfaction level 
#  WorkLifeBalance:  Those that are staying have a higher density of 2's and 3's

#  5. Performance Data

train_raw_tbl %>% select(Attrition, contains("performance"), contains("involvement")) %>% 
     plot_ggpairs(Attrition)

# Observations:
#  JobInvolvement:  Those that are leaving have a lower density of 3's and 4's

#  6. Work-Life Features

train_raw_tbl %>% select(Attrition, contains("overtime"), contains("travel")) %>% 
     plot_ggpairs(Attrition)

# Observations:
#  Overtime:  The proportion of those leaving that are working Over Time are high compared to those that are not leaving

#  7. Training and Education

train_raw_tbl %>% select(Attrition, contains("training"), contains("education")) %>% 
     plot_ggpairs(Attrition)

# Observations:
#  TrainingTimesLastYear:  People that leave tend to have less annual trainings

#  8. Time-Based Features

train_raw_tbl %>% select(Attrition, contains("years")) %>% plot_ggpairs(Attrition)

# Observations:
#  YearsAtCompany:  People that leave tend to have less working years at the company
#  YearsSinceLastPromotion:  It's difficult to deduce anything based on the visualization 
