# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----


# 1.0 Load libraries ----

# Contians readr
library(tidyverse)   

# Excel Connection
library(readxl)
library(writexl)

# Database Connection
library(odbc)
library(RSQLite)

# 2.0 readr ----

# 2.1 CSV ----

bike_order_csv_tabl <-  read_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")

problems(bike_order_csv_tabl) #none were returned but nice function to know. Use slice to look at specific errors
# e3 problems are probably from 1e3 then an order number was 1000 or greater. Below is how to fix it if it occurs
read_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv", col_types = cols(order_id = col_double()))

# 2.2 RDS ----

bike_orders_rds_tbl <- read_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")

# 3.0 Excel ----

excel_sheets("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx")

bike_orders_excel_tbl <- read_excel("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx", sheet = "Sheet1")

# 4.0 Databases  ----

con <- dbConnect(drv = SQLite(), dbname = "00_data/chinook/Chinook_Sqlite.sqlite")

dbListTables(con)

album_tbl <- tbl(con, "Album") %>% collect() # tbl and connect (pulls data into memory) are from dplyr

# Disconnect

artist_tbl <- tbl(con, "Artist") %>%  collect()

dbDisconnect(con)

