# INSTACART MARKET BASKET ANALYSIS -----
# Load data 

library(tidyverse)
library(fs)

read_directory_to_list <- function(path, .f = read_csv, ...) {
    
    names_vec <- dir_ls(path, ...) %>%
        str_split("/", simplify = T) %>%
        .[,ncol(.)] %>%
        path_ext_remove()
    
    ret_list <- dir_ls(path) %>%
        map(.f) %>%
        set_names(names_vec)
    
    return(ret_list)
    
}

