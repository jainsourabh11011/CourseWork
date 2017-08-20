# Load ToothGrowth data
data(ToothGrowth)

# load packages, hidden
library(ggplot2)

#  Dose transformation to factor and data peek:

data <- ToothGrowth
data$dose <- factor(data$dose)
levels(data$supp) <- c("Orange Juice", "Ascorbic Acid")
str(data)
summary(data)

# Dosage testing: 0.5 mg vs 1 mg.

data_05_1 <- subset(data, dose %in% c(0.5,1))
t.test(len ~ dose, paired=F, var.equal=F, data=data_05_1)

# Dosage testing: 1 mg vs 2 mg.

data_1_2 <- subset(data, dose %in% c(1,2))
t.test(len ~ dose, paired=F, var.equal=F, data=data_1_2)

# Delivery method testing

t.test(len ~ supp, paired=F, var.equal=F, data=data[data$dose==0.5,])
t.test(len ~ supp, paired=F, var.equal=F, data=data[data$dose==1,])
t.test(len ~ supp, paired=F, var.equal=F, data=data[data$dose==2,])
