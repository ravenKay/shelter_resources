#Import Libaries 
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(gridExtra)
library(corrplot)
library(gmodels)
library(leaps)
library(ISLR)
library(ResourceSelection)
library(leaps)
library(car)
library(caret)
library(e1071)
library(DataExplorer)
library(randomForest)

#import dataset
homeless_services <- read_xlsx("homeless_services.xlsx")

#PART A: REVIEW DATA STRUCTURE, MISSING VALUES, ETC. 
#check dimensions and structure
names(homeless_services)
str(homeless_services)
dim(homeless_services) 

#check for missing values 
plot_missing(homeless_services)

#check for duplicates 
cat("The number of duplicated rows are", nrow(homeless_services) - nrow(unique(homeless_services)))

#split the numerical and categorical variables for analysis
categorical_variables <- names(homeless_services)[which(sapply(homeless_services, is.character))]
categorical_car <- c(categorical_variables, 'Currently housed in transitional shelter?', ' Shelter Exit Date','Obtained permanent housing in Alexandria?', 'Utilizes shelter mental health services?', 'History of mental health-related challenges?', 'Utilizes shelter substance abuse services?')
numeric_variables <- names(homeless_services)[which(sapply(homeless_services, is.numeric))]

#set data frame to data table
hs_table <- setDT(homeless_services)

#summarize numerical values and structure 
summary(homeless_services[,.SD, .SDcols = numeric_variables])

#create factors and check data with categorical variables
homeless_services[,(categorical_variables) := lapply(.SD, as.factor), .SDcols = categorical_variables]
train_cat <- homeless_services[,.SD, .SDcols = categorical_variables]
train_cont <- homeless_services[,.SD,.SDcols = numeric_variables]
str(homeless_services)

#check number of residents not utilizing relevant shelter services 
total_mental <- sum(homeless_services$`History of mental health-related challenges?` == "y")
total_substance <- sum(homeless_services$`History of substance abuse-related challenges?` == "y")

no_use_mental <- sum(homeless_services[,"History of mental health-related challenges?", with = FALSE] != homeless_services[, "Utilizes shelter mental health services?" , with = FALSE])

no_use_substance <- sum(homeless_services[,"History of substance abuse-related challenges?", with = FALSE]!= homeless_services[, "Utilizes shelter substance abuse services?" , with = FALSE])

#Percentage not taking advantage of relevant services
no_use_mental/total_mental
no_use_substance/total_substance

#create graphs using ggplot (credit: plot loops and styling based off https://www.kaggle.com/emrearslan/detailed-exploratory-data-analysis-using-r )
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

#plot out to view
doPlots(train_cat, fun = plotHist, ii = 7:9, ncol = 4)
hist(homeless_services$`Monthly Income`)

#Conduct initial analysis via correlation 
correlations <- cor(train_cont)
corrplot(correlations, method="square")

#frequencies of variables occuring 
freq_cat <- table(train_cat$`Currently housed in transitional shelter?`)
relfreq <- freq_cat/299
cbind(freq_cat, relfreq)

joint <- CrossTable(train_cat$`Utilizes shelter mental health services?`, train_cat$`History of mental health-related challenges?`, prop.chisq = FALSE)
joint.counts <- joint$t
barplot(joint.counts, beside = TRUE, col = rainbow(2), ylab = 'Frequency', xlab = 'variable')

#Rename given the many factors and normalize numeric variables (if necessary)
homeless_services$`Currently housed in transitional shelter?` <- factor(homeless_services$`Currently housed in transitional shelter?`, levels = c("y", "n"), labels = c("Housed in shelter", "No longer housed in shelter"))

#Variable Selection
hs <- as.data.frame(homeless_services)
regsubsets.out <- 
  regsubsets(hs$`Monthly Income` ~ hs$`Shelter Entry Date` + hs$`Utilizes shelter mental health services?`+ hs$`Utilizes shelter mental health services?` + hs$`History of mental health-related challenges?` + hs$`Utilizes shelter substance abuse services?` + hs$`History of substance abuse-related challenges?` + hs$`Has utilized employment assistance services?` + hs$`Currently employed?` + hs$`Obtained permanent housing in Alexandria?`,
             data = homeless_services,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
regsubsets.out
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
layout(matrix(1:2, ncol = 2))
## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")

abline(a = 1, b = 1, lty = 2)
res.legend
which.max(summary.out$adjr2)
summary.out$which[3,]

#Part D: Modeling

#Basic 'best fit' linear model 
m1 <- lm(hs$`Monthly Income` ~ hs$`Obtained permanent housing in Alexandria?` + hs$`Currently employed?` + hs$`Utilizes shelter substance abuse services?`, data = homeless_services)
summary(m1)
plot(m1)


#Basic logistic regression model
housing_step_model <- step(lm(hs$`Monthly Income`~ hs$`Shelter Entry Date` + hs$`Utilizes shelter mental health services?`+ hs$`Utilizes shelter mental health services?` + hs$`History of mental health-related challenges?` + hs$`Utilizes shelter substance abuse services?` + hs$`History of substance abuse-related challenges?` + hs$`Has utilized employment assistance services?` + hs$`Currently employed?` + hs$`Obtained permanent housing in Alexandria?`, data = hs), direction = "forward")

housing_logit <- glm(hs$`Obtained permanent housing in Alexandria?`~ hs$`Shelter Entry Date` + hs$`Utilizes shelter mental health services?`+ hs$`Utilizes shelter mental health services?` + hs$`History of mental health-related challenges?` + hs$`Utilizes shelter substance abuse services?` + hs$`History of substance abuse-related challenges?` + hs$`Has utilized employment assistance services?` + hs$`Currently employed?` + hs$`Monthly Income`, family = binomial(link = "logit"), data = hs)
summary(housing_logit)
#Because the output is in log terms we need to convert to % using exp
exp(coef(housing_logit))

#Test our models goodness of fit 
hoslem.test(hs$`Monthly Income`, fitted(housing_logit))


#perform bagging 
sample(1:12, replace= TRUE)

set.seed(203)
forest_fit <- randomForest(as.factor(homeless_services$`Obtained permanent housing in Alexandria?`) ~ homeless_services$`Utilizes shelter mental health services?` + homeless_services$`Shelter Entry Date` + homeless_services$`History of mental health-related challenges?`+homeless_services$`Utilizes shelter substance abuse services?` + homeless_services$`History of substance abuse-related challenges?` + homeless_services$`Has utilized employment assistance services?` + homeless_services$`Currently employed?` + homeless_services$`Monthly Income`, data = homeless_services, importance = TRUE, nTree = 500)

plot(forest_fit)
varImpPlot(forest_fit)
varUsed(forest_fit)
prediction <- predict(forest_fit)
summary(prediction)
conf <- forest_fit$confusion
print(conf)

