
basename_reg <-c("Northeast", "Midwest", "South", "West")
pop_reg <- c(3.21, 3.94, 14.29, 13.84)
barplot(pop_reg, names.arg = basename_reg, ylab="Population Growth %", col="lightgreen", border = "lightblue")


basename_div <- c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", 
                  "East South Central", "West South Central", "Mountain", "Pacific")
pop_div <-c(3.75, 3.03, 2.80, 6.59, 15.47, 8.28, 15.59, 21.42, 10.78)
barplot(pop_div, names.arg = basename_div, ylab="Population Growth %", col="lightgreen", border = "lightblue", las=2)
