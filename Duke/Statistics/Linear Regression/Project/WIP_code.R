#See http://www.r-bloggers.com/regression-on-categorical-variables/

library(ggplot2)
library(dplyr)
library(statsr)
library(knitr)
library(scales)
library(formattable)
library(car)
library(MASS)
library(psych)

#http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

unique1 <- lapply(movies_filtered, unique)
unique2 <- sapply(unique1, length)
unique2 <- as.data.frame(unique2)
colnames(unique2) <- "Count_Distinct"
unique3<-cbind(Variables=rownames(unique2), unique2)
rownames(unique3) <- NULL
unique4 <- unique3 %>% mutate(Percent_Unique = 1-Count_Distinct/nrow(movies_filtered))
unique4$Percent_Unique <- percent(unique4$Percent_Unique)
format_table(unique4,list(Variables=formatter("span", style="color:blue")), align = "c")
kable(unique4)
ggplot(unique4, aes(x=Variables, y=Percent_Unique)) + geom_bar(stat = "identity")

#Good to know
library(plyr)
count(movies_filtered, "genre")

ggplot(movies_filtered, aes(x = title_type, y = audience_score)) + geom_boxplot() + 
     xlab("Title Type") + ylab("Audience Score") + ggtitle("Audience Score vs. Title Type")

movies_filtered %>% group_by(title_type) %>% summarise(median_score = median(audience_score), 
                                                       iqr_score = IQR(audience_score))

by_directors <- movies_filtered %>% group_by(director) %>% summarize(n())
colnames(by_directors) <- c("Director_Name", "Count_Movies")
by_directors <- by_directors[order(-by_directors$Count_Movies),]
range(by_directors$Count_Movies)
by_dir_cnt <- by_directors %>% group_by(Count_Movies) %>% summarize(n())
colnames(by_dir_cnt) <- c("Num_Movies", "Num_Directors")
ggplot(by_dir_cnt, aes(x=Num_Movies, y=Num_Directors)) +
     geom_bar(stat = "identity", fill="lightgreen") +
     geom_text(aes(label=Num_Directors), vjust=-.5) +
     ggtitle("How Many Movies Did Directors Produce?") +
     labs(x="No. of Movies Produced", y="No. of Directors")

tmpMLR <- lm(audience_score ~., data=movies_filtered)
pairs.panels(movies_filtered)
plot(movieMLR, pch=16, which=1)
#Note how the residuals plot of this last model shows some important points still lying 
#far away from the middle area of the graph.

movies_filtered2 <- select(movies_filtered, genre:audience_score)
tmpMLR <- lm(audience_score ~., data=movies_filtered2)

summary(tmpMLR)

VARIABLE=c("",gsub("[-^0-9]", "", names(unlist(tmpMLR$xlevels))))
MODALITY=c("",as.character(unlist(tmpMLR$xlevels)))
names=data.frame(VARIABLE,MODALITY,NOMVAR=c("(Intercept)",paste(VARIABLE,MODALITY,sep="")[-1]))
regression=data.frame(NOMVAR=names(coefficients(tmpMLR)), COEF=as.numeric(coefficients(tmpMLR)))
merge(names,regression,all.x=TRUE)

merge(names,regression)

library(xtable)
htlmtable <- xtable(merge(names,regression))
print(htlmtable,type="html")

#See http://www.statmethods.net/stats/rdiagnostics.html
outlierTest(tmpMLR)

qqPlot(tmpMLR)

sresid <- studres(tmpMLR) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(tmpMLR)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(tmpMLR)

# Evaluate Collinearity
vif(tmpMLR) # variance inflation factors 
sqrt(vif(tmpMLR)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(tmpMLR)
# Ceres plots 
ceresPlots(MLR)


#See this:  https://rpubs.com/FelipeRego/MultipleLinearRegressionInRFirstSteps
#See this https://rpubs.com/FelipeRego - really good stuff
#See http://www.r-bloggers.com/the-relative-importance-of-predictors-let-the-games-begin/
# http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression


#Create a new varaible where best_anything = Yes if any best* = Yes
movies_new <- mutate(movies, best_any=ifelse(best_pic_nom=="yes", 1, 
               ifelse(best_pic_win=="yes", 1,
               ifelse(best_actor_win=="yes", 1,
               ifelse(best_actress_win=="yes", 1,
               ifelse(best_dir_win=="yes", 1,
               ifelse(top200_box=="yes", 1, 0)))))))

#Way to manage the exlanatory variables in a ln function - see ?lm
#see http://www.r-bloggers.com/using-the-update-function-during-variable-selection/
MLR <- lm(audience_score ~., data=movies_filtered)#full model
MLR2 <- update(MLR, . ~  . -mpaa_rating)

MLR_Step2 <- update(MLR2, .~. -runtime)

