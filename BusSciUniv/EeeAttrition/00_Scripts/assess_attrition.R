assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct){
     
     attrition_col_expr <- enquo(attrition_col)
     
     data %>% filter((!! attrition_col_expr) %in% attrition_value) %>% 
          arrange(desc(pct)) %>% 
          mutate(above_industry_avg = case_when(
               pct > baseline_pct ~ "Yes", 
               TRUE ~ "No"))
}

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

count_to_pct <- function(data, ..., col = n){
     
     grouping_vars_expr <- quos(...)
     col_expr <- enquo(col)
     
     ret <- data %>% group_by(!!! grouping_vars_expr) %>% 
          mutate(pct = (!! col_expr)/sum(!! col_expr)) %>% ungroup()
     
     return(ret)     
}

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