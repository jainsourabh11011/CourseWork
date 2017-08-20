library(ggplot2)
library(plyr)#Used for the revalue function
library(dplyr)
library(RColorBrewer)
library(statsr)


load("./Inferential/project/gss.Rdata")

varsOfInterest <- c("year", "age", "sex", "race", "degree", "incom16", "relig", "attend", "class", "natspac")
modData <- select(gss, one_of(varsOfInterest))

countNAs <- apply(modData, 2, function(x) sum(is.na(x)))
countNAs#25834

modData2 <- select(modData, attend, natspac)
modData2 <- modData2 %>% filter(attend != "NA", natspac != "NA")

table(modData2$attend, modData2$natspac)

n_distinct(modData$attend)

levels(modData2$attend)
#to drop factor see http://www.r-bloggers.com/r-drop-factor-levels-in-a-dataset/
modData2$attend <- modData2$attend[modData2$attend !="Never"]
modData2$attend <- factor(modData2$attend)
levels(modData2$attend)


plot(gss$attend,  col=brewer.pal(3, "Accent"))
ggplot(gss, aes(x=attend, fill=factor(attend))) + geom_bar() + theme(axis.text.x=element_text(angle=90))
ggplot(gss, aes(x=natspac, fill=factor(natspac))) + geom_bar() + theme(axis.text.x=element_text(angle=90))

modData2$attend <- revalue(modData2$attend, c("Never"="Rarely", "Lt Once A Year"="Rarely", "Once A Year"="Rarely", "Sevrl Times A Yr"="Rarely"))
modData2$attend <- revalue(modData2$attend, c("Once A Month"="Frequently", "2-3X A Month"="Frequently", "Nrly Every Week"="Frequently", 
                                              "Every Week"="Frequently", "More Thn Once Wk"="Frequently"))
#table(modData2)
#tmpTable <- table()
tmpTable <- addmargins(table(modData2))
tmpTable

plot(modData2$attend, modData2$natspac, col=brewer.pal(3, "Accent"))
library(vcd)
P10 <- mosaic(~attend + natspac, data = modData2, shade=TRUE)

#Overall frequent attendance rate in the sample
overall_frq <- tmpTable[2,4]/tmpTable[3,4]
overall_rare <- tmpTable[1,4]/tmpTable[3,4]
#calculate expected
rare_little <- round(tmpTable[3,1] * overall_rare)
rare_right <- round(tmpTable[3,2] * overall_rare)
rare_much <- round(tmpTable[3,3] * overall_rare)

rare_total <- rare_little + rare_right + rare_much

frq_little <- round(tmpTable[3,1] * overall_frq)
frq_right <- round(tmpTable[3,2] * overall_frq)
frq_much <- round(tmpTable[3,3] * overall_frq)

frq_total <- frq_little + frq_right + frq_much

rarely <- c(1591, 4358, 4671)
frequently <- c(1486, 6248, 7700)
religion_space <- as.data.frame(rbind(rarely, frequently))
names(religion_space) <- c("Too Little", "About Right", "Too Much")
chisq.test(religion_space)
