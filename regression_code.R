library(nnet)
library(ggplot2)
library(caret)
library(ROCR)
library(leaps)
library(dplyr)
library(hrbrthemes)
library(ggcorrplot)
library(tidyverse)


setwd("C:/Users/micha/Documents/STAT 6021/Project/Project 2/Stats-6021-Project-2")

########################### exploratory analysis #####################################

data <-read.csv(file = "wineQualityWhites_Reds_combind.csv")
datared <-read.csv(file = "wineQualityReds.csv")
datawhite <-read.csv(file = "wineQualityWhites.csv")

datared$X <- NULL
datawhite$X <- NULL

attach(data)

#force categorical columns to factors
wine_type <- factor(wine_type)
levels(wine_type) <- c("white", "red") 
contrasts(wine_type)

#data exploration
#create quality histogram 

ggplot(data, aes(quality, fill = wine_type)) + 
  geom_histogram(alpha = .4, position = 'identity', binwidth = 1)+
  scale_x_continuous(breaks = seq(0,10,1), lim = c(0,10)) +labs(title = "Count of Wine by Quality") 

#correlation 
#white
data2 <- datawhite
data2$wine_type <- NULL
m <- cor(data2)
ggcorrplot(m, hc.order = TRUE, type = "lower",
           lab = TRUE) +labs(title = "White Wine Correlation") 
#reds
data2 <- datared
data2$wine_type <- NULL
m <- cor(data2)
ggcorrplot(m, hc.order = TRUE, type = "lower",
           lab = TRUE) +labs(title = "Red Wine Correlation") 

#quality vs alcohol 
#reds
boxplot(datared$alcohol~datared$quality,
        data=datawhite,
        main="Quality vs Alcohol Content Red Wine",
        xlab="Quality",
        ylab="Alcohol Content",
        col="red",
        border="black"
)

#White
boxplot(datawhite$alcohol~datawhite$quality,
        data=datawhite,
        main="Quality vs Alcohol Content White Wine",
        xlab="Quality",
        ylab="Alcohol Content",
        col="white",
        border="black"
)

par(mfrow=c(2,2))
# need to check for outliers for the red and white data sets
#red wine
n<-length(datared$quality)
p<-11
# resultred <- lm(datared$quality~datared$fixed.acidity +
#                datared$volatile.acidity + datared$citric.acid +
#                datared$residual.sugar + datared$chlorides +
#                datared$free.sulfur.dioxide + datared$total.sulfur.dioxide +
#                datared$density + datared$pH + datared$sulphates +datared$alcohol)

resultred <- lm(datared$quality ~ datared$alcohol + datared$density + datared$sulphates + datared$volatile.acidity)


#leverage 
lev<-lm.influence(resultred)$hat 

sort(lev)
2*p/n

plot(lev, main="Red Wine Leverages", ylim=c(0,0.2))
abline(h=2*p/n, col="red")
lev[lev>2*p/n]

# Are there any influential observations based on DFITs?
##influential observations
DFFITS<-dffits(resultred)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]


#1f Are there any influential observations based on Cooks 
COOKS<-cooks.distance(resultred)
COOKS[COOKS>qf(0.5,p,n-p)]


plot(resultred, main="Red Wine", pch = 18, col='red' , which =c(4))


#white Wine outliers
n<-length(datawhite$quality)
p<-11
# resultwhite <- lm(datawhite$quality~datawhite$fixed.acidity + 
#                   datawhite$volatile.acidity + datawhite$citric.acid + 
#                   datawhite$residual.sugar + datawhite$chlorides + 
#                   datawhite$free.sulfur.dioxide + datawhite$total.sulfur.dioxide + 
#                   datawhite$density + datawhite$pH + datawhite$sulphates +datawhite$alcohol)

resultwhite <- lm(datawhite$quality ~ datawhite$alcohol + datawhite$pH + datawhite$sulphates + datawhite$density + datawhite$volatile.acidity + datawhite$residual.sugar )


#leverage 
lev<-lm.influence(resultwhite)$hat 

sort(lev)
2*p/n

plot(lev, main="White Wine Leverages", ylim=c(0,0.6))
abline(h=2*p/n, col="blue")
lev[lev>2*p/n]

# Are there any influential observations based on DFITs?
##influential observations
DFFITS<-dffits(resultwhite)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]


#Are there any influential observations based on Cooks 
COOKS<-cooks.distance(resultwhite)
COOKS[COOKS>qf(0.5,p,n-p)]


plot(resultwhite, main="White Wine", pch = 18, col='red' , which =c(4))



#outliers for wine type 
par(mfrow=c(1,2))
resultType <- lm(data$quality ~ data$density + data$residual.sugar + data$total.sulfur.dioxide + data$volatile.acidity + 
                   data$chlorides + data$sulphates + data$alcohol + data$free.sulfur.dioxide)

n<-length(data$quality)
p<-11

#leverage 
lev<-lm.influence(resultType)$hat 

sort(lev)
2*p/n

plot(lev, main="Wine Type Leverages", ylim=c(0,0.6))
abline(h=2*p/n, col="blue")
lev[lev>2*p/n]

# Are there any influential observations based on DFITs?
##influential observations
DFFITS<-dffits(resultType)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]


# Are there any influential observations based on Cooks 
COOKS<-cooks.distance(resultType)
COOKS[COOKS>qf(0.5,p,n-p)]


plot(resultType, main="White Wine", pch = 18, col='red' , which =c(4))








################## Red Wine Analysis of Quality  ###################
wines_red <- read.csv('wineQualityReds.csv', header=TRUE, row.names=1)
wines_white <- read.csv('wineQualityWhites.csv', header=TRUE, row.names=1)


# Create a binomial class for quality (1 for Good and 0 for Bad)
wines_red$quality_class [wines_red$quality >= 0] <- 0 # Bad
wines_red$quality_class [wines_red$quality >= 6] <- 1 # Good

# Add levels and convert quality class to factor
levels(wines_red$quality_class) <- c('Good', 'Bad')
wines_red$quality_class <- as.factor(wines_red$quality_class)
contrasts(wines_red$quality_class)

# Evaluate model using validation 
set.seed(111)
index <- sample.int(nrow(wines_red), floor(.70*nrow(wines_red)), replace = F)
train <-wines_red[index, ]
test <-wines_red[-index, ]

# Produce a logistic regression for quality class on training set
result_red <- glm(quality_class~alcohol+volatile.acidity+fixed.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+free.sulfur.dioxide+density+pH+sulphates, data=train, family="binomial")
summary(result_red) 


## The summary tells us that some of the predictors are  not significant in the presence of all the predictors. 
## After validation of this model, we may try a reduced model

# Predictions on test set using logistic regression
preds<-round(predict(result_red, test, type="response"))

# Confusion matrix for predictions
confusion_table <- table(preds, test$quality_class) 

n <- sum(confusion_table) # number of instances
diag <- diag(confusion_table) 

accuracy <- sum(diag) / n

## Accuracy of the model is 0.7554859

TP <- confusion_table[2,2]
FP <- confusion_table[1,2]
FN <- confusion_table[2,1]
TN <- confusion_table[1,1]
false_pos <- FP/TN
false_negative <- FN/TP
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)

## We will assess the predictors using box plots to decide which predictors 
## should stay in the model

# Produce Box Plots
p1<-ggplot(aes(y = fixed.acidity, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p2<-ggplot(aes(y = volatile.acidity, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p3<-ggplot(aes(y = citric.acid, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p4<-ggplot(aes(y = residual.sugar, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p5<-ggplot(aes(y = chlorides, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p6<-ggplot(aes(y = free.sulfur.dioxide, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p7<-ggplot(aes(y = density, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p8<-ggplot(aes(y = total.sulfur.dioxide, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p9<-ggplot(aes(y = pH, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p10<-ggplot(aes(y = sulphates, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()
p11<-ggplot(aes(y = alcohol, x = factor(quality_class), fill = factor(quality_class)), data = wines_red) + geom_boxplot()

require(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2) # volatile acidity, citric acid
grid.arrange(p5, p6, p7, p8,  ncol=2, nrow=2) # density, total.sulfur.dioxide
grid.arrange(p9, p10, p11, ncol=2, nrow=2) # alcohol, sulphates

grid.arrange(p2 ,p3, p7, p8, ncol=2, nrow=2)
grid.arrange(p10, p11, nrow=2)

## It looks like alcohol, density, sulphates, volatile acidity, citric acid and total sulfur dioxide may be significant. 
## We will try a reduced model using only these predictors.

# Check VIF
library(faraway)
vif(train[,c(1:11)])

# Reduced model
result_red_reduced <- glm(quality_class~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+free.sulfur.dioxide+sulphates, data=train, family="binomial")
summary(result_red_reduced)

# Is this better than the full model?
deltaG2_partial<-result_red_reduced$deviance-result_red$deviance ##take difference in residual deviances between the two models
deltaG2_partial
1-pchisq(deltaG2_partial,5) ##df is 5 since we have 5 additional parameters in the bigger model

# Is this model useful?
deltaG2<-result_white_reduced$null.deviance-result_white_reduced$deviance ##take difference between the null and residual deviance for our model
1-pchisq(deltaG2,5) ##df is 5 since our model has 5 additional parameters other than the intercept

# Predict new values on test set using reduced model
preds<-round(predict(result_red_reduced, test, type='response'))

# Confusion matrix
confusion_table <- table(preds, test$quality_class)

n <- sum(confusion_table) # number of instances
diag <- diag(confusion_table) 

accuracy <- sum(diag) / n # accuracy calculation 

## Accuracy of the model is 0.7711599, slightly better than first

TP <- confusion_table[2,2]
FP <- confusion_table[1,2]
FN <- confusion_table[2,1]
TN <- confusion_table[1,1]
false_pos <- FP/TN
false_negative <- FN/TP
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)

## The overall error is 0.2288401.

# Produce ROC Curve  
rates<-prediction(preds, test$quality_class)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

# Plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Quality Class")
lines(x = c(0,1), y = c(0,1), col="red")

# Compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values[[1]]



################## White Wine Analysis of Quality ###################

# Create a binomial class for quality (1 for Good and 0 for Bad)
wines_white$quality_class [wines_white$quality >= 1] <- 0 # Bad
wines_white$quality_class [wines_white$quality >= 6] <- 1 # Good

# Add levels and convert quality class to factor
levels(wines_white$quality_class) <- c('Good', 'Bad')
wines_white$quality_class <- as.factor(wines_white$quality_class)
contrasts(wines_white$quality_class)

## The summary tells us that none of the predictors are significant in the presence of all the predictors. 
## After validation of this model, we may try a reduced model

# Evaluate model using validation 
set.seed(111)
index <- sample.int(nrow(wines_white), floor(.70*nrow(wines_white)), replace = F)
train <-wines_white[index, ]
test <-wines_white[-index, ]

# Produce a logistic regression for quality class regressed against all other variables
result_white <- glm(quality_class~volatile.acidity+fixed.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+free.sulfur.dioxide+density+pH+sulphates+alcohol, data=train, family="binomial")
summary(result_white)

# Predictions on test set using logistic regression
preds<-round(predict(result_white,test, type='response'))

# Confusion matrix
confusion_table <- table(preds, test$quality_class)

n <- sum(confusion_table) # number of instances
diag <- diag(confusion_table) 

accuracy <- sum(diag) / n # accuracy calculation 

## Accuracy of the model is 0.752809

TP <- confusion_table[2,2]
FP <- confusion_table[1,2]
FN <- confusion_table[2,1]
TN <- confusion_table[1,1]
false_pos <- FP/TN
false_negative <- FN/TP
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)


# Produce Box Plots
p1<-ggplot(aes(y = fixed.acidity, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p2<-ggplot(aes(y = volatile.acidity, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p3<-ggplot(aes(y = citric.acid, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p4<-ggplot(aes(y = residual.sugar, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p5<-ggplot(aes(y = chlorides, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p6<-ggplot(aes(y = free.sulfur.dioxide, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p7<-ggplot(aes(y = density, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p8<-ggplot(aes(y = total.sulfur.dioxide, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p9<-ggplot(aes(y = pH, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p10<-ggplot(aes(y = sulphates, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()
p11<-ggplot(aes(y = alcohol, x = factor(quality_class), fill = factor(quality_class)), data = wines_white) + geom_boxplot()


require(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2) # volatile acidity, residual.sugar
grid.arrange(p5, p6, p7, p8,  ncol=2, nrow=2) # density
grid.arrange(p9, p10, p11, ncol=2, nrow=2) #pH, sulphates, alchohol

grid.arrange(p2, p4, p7, p9, ncol=2, nrow=2)
grid.arrange(p10, p11, ncol=2)


# Reduced model
result_white_reduced <- glm(quality_class~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol, data=train, family="binomial")
summary(result_white_reduced)

# Is this better than the full model?
deltaG2_partial2<-result_white_reduced$deviance-result_white$deviance ##take difference in residual deviances between the two models
deltaG2_partial2
1-pchisq(deltaG2_partial2,5) ##df is 5 since we have 5 additional parameters in the bigger model

# Is this model useful?
deltaG22<-result_white_reduced$null.deviance-result_white_reduced$deviance ##take difference between the null and residual deviance for our model
1-pchisq(deltaG22,7) ##df is 7 since our model has 7 additional parameters other than the intercept


## All of the predictors are significant in the presence of the other predictors.

# Predictions on test set using reduced model
preds<-round(predict(result_white_reduced,test, type='response'))

# Confusion matrix
confusion_table <- table(preds, test$quality_class)

n <- sum(confusion_table) # number of instances
diag <- diag(confusion_table) 

accuracy <- sum(diag) / n

## Accuracy of the model is 0.7487232, not much change in accuracy from full model
## so choose simpler model.

TP <- confusion_table[2,2]
FP <- confusion_table[1,2]
FN <- confusion_table[2,1]
TN <- confusion_table[1,1]
false_pos <- FP/TN
false_negative <- FN/TP
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)


# Produce the numbers associated with classification table
rates<-prediction(preds, test$quality_class)
# Produce ROC Curve
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

# Plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Quality Class")
lines(x = c(0,1), y = c(0,1), col="red")

# Compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values[[1]]

#######################################################################################


#predict probability of "Good" quality for red wine
wineRed <- data.frame(alcohol = 13.1, volatile.acidity = .54, chlorides = 0.076, total.sulfur.dioxide=17, free.sulfur.dioxide=8, sulphates = .72  )
prob<-predict(result_red_reduced, wineRed, type="response")
prob

#predict probability of "Good" quality fo for white wine
wineWhite <- data.frame(volatile.acidity = .26, residual.sugar = 1.5, free.sulfur.dioxide=48, density = .9912, alcohol = 12.4 , pH = 3.54 , sulphates = .52)

prob2<-predict(result_white_reduced ,wineWhite, type="response")
prob2


################################## Plots by Type and Quality and Wine Type prediction #######################

data <- read.table("wineQualityWhites_Reds_combind.csv", header=TRUE, sep=",")

# Drop index column
data <- subset(data, select = -c(X))

# Dummy code wine_type as boolean, 1 = red, 0 = white
data$wine_type_orig <- as.factor(data$wine_type)
data$wine_type <- ifelse(data$wine_type == 'red', 1, 0)

# Create a binomial class for quality (1 for Good and 0 for Bad)
data$quality_class[data$quality >= 1] <- 0 # Bad
data$quality_class[data$quality >= 6] <- 1 # Good
data$quality_class_words <- ifelse(data$quality_class, 'Good', 'Bad')

# Add levels and convert quality class to factor
levels(data$quality_class) <- c('Good', 'Bad')
data$quality_class <- as.factor(data$quality_class)
contrasts(data$quality_class)

# Sample random 70% set
set.seed(111)
data.red <- data[data$wine_type == 1,]
data.white <- data[data$wine_type == 0,]
data.red.sampled <- sample.int(nrow(data.red), floor(.70*nrow(data.red)), replace = F)
data.white.sampled <- sample.int(nrow(data.white), floor(.70*nrow(data.white)), replace = F)

# Split test and train
data.train <- rbind(data.red[data.red.sampled,], data.white[data.white.sampled,])
data.test <- rbind(data.red[-data.red.sampled,], data.white[-data.white.sampled,])
attach(data.train)

# Measure correlations
cor(cbind(total.sulfur.dioxide,volatile.acidity,chlorides,fixed.acidity,sulphates,free.sulfur.dioxide,density,residual.sugar,pH,citric.acid,quality,alcohol))
#pairs(data.train, pch = 19, lower.panel=NULL)


library(ggplot2)
require(gridExtra)


# Plots by Type and Quality

ggplot(aes(y = total.sulfur.dioxide, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot() +
  ggtitle("Total Sulfur Dioxide by Type and Quality") + xlab("Quality") + ylab("Total Sulfur Dioxide")    +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y = volatile.acidity, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot() +
  ggtitle("Volatile Acidity by Type and Quality") + xlab("Quality") + ylab("Volatile Acidity")   +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y = fixed.acidity, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot() +
  ggtitle("Fixed Acidity by Type and Quality") + xlab("Quality") + ylab("Fixed Acidity")  +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y = pH, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot() +
  ggtitle("pH by Type and Quality") + xlab("Quality") + ylab("pH")    +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y = citric.acid, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot() +
  ggtitle("Citric Acid by Type and Quality") + xlab("Quality") + ylab("Citric Acid")    +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y = chlorides, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot() +
  ggtitle("Chlorides by Type and Quality") + xlab("Quality") + ylab("Chlorides")    +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))


ggplot(aes(y = sulphates, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot()  +
  ggtitle("Sulphates by Type and Quality") + xlab("Quality") + ylab("Sulphates") +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y = free.sulfur.dioxide, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot()  +
  ggtitle("Free Sulfur Dioxide by Type and Quality") + xlab("Quality") + ylab("Free Sulphur Dioxide")    +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y =alcohol, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot()   +
  ggtitle("Alcohol by Type and Quality") + xlab("Quality") + ylab("Alcohol Content (%)")   +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y =density, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot()  +
  ggtitle("Density by Type and Quality") + xlab("Quality") + ylab("Density (g/mL)")    +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))

ggplot(aes(y = residual.sugar, x = quality_class_words, fill = wine_type_orig), data = data) + geom_boxplot() +
  ggtitle("Residual Sugar by Type and Quality") + xlab("Quality") + ylab("Residual Sugar") +
  scale_fill_manual(values=c("#E66868", "#E3DD64"), name="Wine Type", breaks=c('red', 'white'), labels=c("Red", "White"))


# Create predictive model
result <- glm(wine_type~density+residual.sugar+total.sulfur.dioxide+volatile.acidity+chlorides+sulphates+alcohol+free.sulfur.dioxide+pH+citric.acid+fixed.acidity+quality, family=binomial, data=data.train)
summary(result)

library(faraway)
vif(data.train[,c(1:12)])

# Reduced Model - remove quality,fixed.acidity, citric.acid, pH
reduced <- glm(wine_type~density+residual.sugar+total.sulfur.dioxide+volatile.acidity+chlorides+sulphates+alcohol+free.sulfur.dioxide, family=binomial, data=data.train)
summary(reduced)

# Is this better than the full model?
deltaG2_partial2<-reduced$deviance-result$deviance ##take difference in residual deviances between the two models
deltaG2_partial2
1-pchisq(deltaG2_partial2,4) ##df is 4 since we have 4 additional parameters in the bigger model

# Is this model useful?
deltaG2<-reduced$null.deviance-reduced$deviance ##take difference between the null and residual deviance for our model
1-pchisq(deltaG2,8) ##df is 8 since our model has 8 additional parameters other than the intercept


vif(data.train[,c(2,4:8,10:11)])

### R_squared ####

r_2 <- 1-(reduced$deviance/reduced$null.deviance)

#### Full Model ####

library(ROCR)
# prediction
preds<-predict(result,newdata=data.test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, data.test$wine_type)

##store the true positive and false positive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Wine Type")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc

##confusion matrix. Actual values in the rows, predicted classification in cols
table(data.test$wine_type, preds>0.5)

table(data.test$wine_type, preds>0.7)


### Reduced Model ####

# prediction
preds2<-predict(reduced,newdata=data.test, type="response")

##produce the numbers associated with classification table
rates2<-prediction(preds2, data.test$wine_type)

##store the true positive and false positive rates
roc_result2<-performance(rates2,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result2, main="ROC Curve for Wine Type")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc2<-performance(rates2, measure = "auc")
auc2

##confusion matrix. Actual values in the rows, predicted classification in cols
table(data.test$wine_type_orig, preds2>0.5)

confusion_table <- table(data.test$wine_type, preds2>0.7)

n <- sum(confusion_table) # number of instances
diag <- diag(confusion_table) 

accuracy <- sum(diag) / n

TP <- confusion_table[2,2]
FP <- confusion_table[1,2]
FN <- confusion_table[2,1]
TN <- confusion_table[1,1]
false_pos <- FP/TN
false_negative <- FN/TP
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)


wineRed <- data.frame(density=0.99235, residual.sugar=2.5, alcohol = 13.1, volatile.acidity = .54, chlorides = 0.076, total.sulfur.dioxide=17, free.sulfur.dioxide=8, sulphates = .72  )

#predict probability of "Red" type fo red wine
prob<-predict(reduced, wineRed, type="response")
prob

wineWhite <- data.frame(volatile.acidity = .26, residual.sugar = 1.5, total.sulfur.dioxide=143, chlorides=0.044, free.sulfur.dioxide=48, density = .9912, alcohol = 12.4, sulphates = .52)

#predict probability of "Red" type for white wine
prob2<-predict(reduced , wineWhite, type="response")
prob2

