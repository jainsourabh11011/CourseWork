# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----


library(tidyverse)
library(readxl)

bikes_tbl           <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bikes_tbl
orderlines_tbl
bike_orderlines_tbl %>% glimpse()

# 1.0 Selecting Columns with select() ----

# Basic select
bike_orderlines_tbl %>% select(order_date, order_id, order_line)

bike_orderlines_tbl %>% select(1:3)

bike_orderlines_tbl %>% select(starts_with("order_"))

# Reduce Columns

bike_orderlines_tbl %>% select(order_date, total_price, category_1, category_2)

# Rearrange

bike_orderlines_tbl %>% select(bikeshop_name:state, everything())

# Pull - Gives all the values of a variable

bike_orderlines_tbl %>% pull(total_price) %>% mean()

## This does not work:

bike_orderlines_tbl %>% select(total_price) %>% mean()

# select_if

bike_orderlines_tbl %>% select_if(is.character)

bike_orderlines_tbl %>% select_if(is.numeric)

bike_orderlines_tbl %>% select_if(~ is.numeric(.)) # the same as the one above (an anonymous function)

# 2.0 Arranging with arrange() and desc() ----

bikes_tbl %>% select(model, price) %>% arrange(desc(price)) %>% View()

# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----

bikes_tbl %>% select(model, price) %>% filter(price > mean(price))
bikes_tbl %>% select(model, price) %>% filter(price > 5000 | price < 1000) %>%  arrange(desc(price))

bikes_tbl %>% select(model, price) %>% filter(price> 6000, model %>% str_detect("Supersix"))

bike_orderlines_tbl %>% filter(category_2 %in% c("Over Mountain", "Trail"))

bike_orderlines_tbl %>% filter(category_2 == "Over Mountain")

bike_orderlines_tbl %>% filter(category_2 != "Over Mountain")

bike_orderlines_tbl %>% filter(!(category_2 %in% c("Over Mountain", "Trail", "Endurance Road")))  ##############

# 3.2 slice(): filtering with row number(s) ----

bikes_tbl %>% arrange(desc(price)) %>% slice(1:5)

bikes_tbl %>% arrange(price) %>%  slice(1:5) # lowest price

bikes_tbl %>% arrange(desc(price)) %>% slice((nrow(.)-4):nrow(.))

bikes_tbl %>% arrange(desc(price)) %>% slice((n()-4):n()) ######################

# distinct() values

bike_orderlines_tbl %>% distinct(category_2)

bike_orderlines_tbl %>% distinct(category_1, category_2)

bike_orderlines_tbl %>% distinct(bikeshop_name, city, state)

# 4.0 Adding Columns with mutate() ----

# Add a Col
 bike_orderlines_prices <-  bike_orderlines_tbl %>% select(order_date, model, quantity, price) %>% 
     mutate(total_price = quantity * price)

## Overwrite

bike_orderlines_prices %>% mutate(total_price = log(total_price))

## Adding a flag

bike_orderlines_prices %>% mutate(is_supersix = model %>% str_to_lower() %>% str_detect("supersix")) %>% 
     filter(is_supersix)                                                                                       ###############

## Binning & If-Then

bike_orderlines_prices %>% mutate(total_price_binned = ntile(total_price, 3))  #####################

### More flexible binning case_when

bike_orderlines_prices %>% mutate(total_price_binned = ntile(total_price, 3)) %>% 
     mutate(total_price_binned2 = case_when(total_price > quantile(total_price, 0.66) ~ "High", 
                                            total_price > quantile(total_price > 0.33) ~ "Medium",
                                            TRUE ~ "Low"))
## Change the cut points
bike_orderlines_prices %>% mutate(total_price_binned = ntile(total_price, 3)) %>% 
     mutate(total_price_binned2 = case_when(total_price > quantile(total_price, 0.75) ~ "High", 
                                            total_price > quantile(total_price > 0.25) ~ "Medium",
                                            TRUE ~ "Low"))

bike_orderlines_prices %>% 
     mutate(bike_type = case_when(
          model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix",
          model %>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll",
          TRUE ~ "Not Supersix or Jekyll")
          )

# 5.0 Grouping & Summarizing with group_by() and summarize() ----

## Bascis
bike_orderlines_tbl %>% summarise(revenue = sum(total_price))

bike_orderlines_tbl %>% group_by(category_1) %>% summarise(revenue = sum(total_price))

bike_orderlines_tbl %>% group_by(category_1, category_2) %>% 
     summarise(revenue = sum(total_price)) %>% ungroup() %>% arrange(desc(revenue))

bike_orderlines_tbl %>% group_by(category_1, category_2, frame_material) %>% 
     summarise(revenue = sum(total_price)) %>% ungroup() %>% arrange(desc(revenue))

## Summary Functions

bike_orderlines_tbl %>% group_by(category_1, category_2) %>% summarise(
     count = n(),
     avg = mean(total_price),
     med = median(total_price),
     sd = sd(total_price),
     min = min(total_price),
     max = max(total_price)
     ) %>% 
     ungroup() %>% arrange(desc(count))

## summarise all - across all columns

bike_orderlines_missing <- bike_orderlines_tbl %>% mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))
bike_orderlines_missing

bike_orderlines_missing %>% summarise_all(~ sum(is.na(.)))

bike_orderlines_missing %>% summarise_all(~ sum(is.na(.))/length(.))

## Below, can also use fill or replace_na

bike_orderlines_missing %>% filter(!is.na(total_price))


# 6.0 Renaming columns with rename() and set_names() ----

# 6.1 rename: One column at a time ----

bikeshop_revenue_tbl <-  bike_orderlines_tbl %>% select(bikeshop_name, category_1, total_price) %>% 
     group_by(bikeshop_name, category_1) %>% summarise(sales = sum(total_price)) %>% 
     ungroup() %>% arrange(desc(sales))

bikeshop_revenue_tbl

bikeshop_revenue_tbl %>% rename("Bikeshop Name" = bikeshop_name, "Primary Category" = category_1, Sales = sales)

# 6.2 set_names: All columns at once ---

bikeshop_revenue_tbl %>% set_names("Bikeshop Name", "Primary Category", "Sales")

bikeshop_revenue_tbl %>% set_names(names(.) %>% str_replace("_", " ") %>% str_to_title()) ######################


# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----

# 7.1 spread(): Long to Wide ----

bikeshop_revenue_tbl

bikeshop_revenue_tbl %>% spread(key = category_1, sales) # key is column name

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>% spread(key = category_1, sales) %>% arrange(desc(Mountain)) %>% 
        mutate(Mountain = scales::dollar(Mountain),
               Road = scales::dollar(Road))  

# 7.2 gather(): Wide to Long ----
# This gets data to the tidy format

bikeshop_revenue_formatted_tbl

bikeshop_revenue_formatted_tbl %>% gather(key = "catergory_1", value = "Sales", Mountain, Road)

bikeshop_revenue_formatted_tbl %>% gather(key = "catergory_1", value = "Sales", -bikeshop_name)

bikeshop_revenue_formatted_tbl %>% gather(key = "catergory_1", value = "Sales", Mountain, Road) %>% ################################
        mutate(Sales = Sales %>% str_remove_all("\\$|,") %>% as.double()) %>% 
        arrange(desc(Sales))

# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----




# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----

# 9.1 bind_cols() ----




# 9.2 bind_rows() ----



