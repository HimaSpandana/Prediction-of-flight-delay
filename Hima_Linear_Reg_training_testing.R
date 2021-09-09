getwd()
read.csv("airlines_2.csv", TRUE, ",")
al <- read.csv("airlines_2.csv", TRUE, ",")
al

# Libraries required for this model

library(tidyverse)
library(ggplot2)
library(dplyr)



head(al)
str(al)
colnames(al)


# multiple linear regression model
# Model 1
lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = al)
model1 = lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = al)
summary(model1)


# Removed ARR_DELAY varible 
# Model 2
lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
model2 = lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
summary(model2)


# Removed MONTH variable
# Model 3
lm (formula =  DEP_DELAY_NEW ~ DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
model3 = lm (formula =  DEP_DELAY_NEW ~ DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
summary(model3)


# Removed DAY_OF_MONTH Variable
# Model 4
lm (formula =  DEP_DELAY_NEW ~ DAY_OF_WEEK + DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
model4 = lm (formula =  DEP_DELAY_NEW ~ DAY_OF_WEEK + DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
summary(model4)

## Removed DAY_OF_MONTH Variable
# Model 5
lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
model5 = lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)
summary(model5)



# compare full model with all the varibles in which we are intrested are included vs the reduced model where we have varibles that are significantly contributed

full <- lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = al)

reduced <- lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = al)



#select a subset of numeric varaibles for regresiion modelling 
auto.sel <- subset(al, select = c(DEP_DELAY_NEW, DEP_TIME, DISTANCE, ARR_DELAY_NEW))

## analyse the variables for 
# - normality of distribution 
# - multiple colinearity 
# - extreme values
# - Homoscedasticity(even distribution of residuals)
# all such problems should be fixed here

#Here we will make brief insception of varibles
pairs(auto.sel, col="red")


# Prediction 
predict(model5,al)

P1<- predict(model5,al)
P1

P1- al$DEP_DELAY_NEW

difference <- P1- al$DEP_DELAY_NEW
difference

sqrt(mean(difference^2))


rmse <- sqrt(mean(difference^2))
rmse

# splitting the datset to training and testing dataset


# training and tesing dataset

set.seed(123)
h<- runif(nrow(al))
AL2 <- al[order(h), ]



AL2 <- al[order(h), ]

train <- al[1:9600, ] #  splitting the data into 80% train and 20%
test<- al[9601:12000, ]


str(train)

str(test)


# Performing linear regression and calculating RMSE of Trained dataset 

# Model1#
lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
model11 <- lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
summary(model11)


#Model2#
lm (formula =  DEP_DELAY_NEW ~ DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
model22 <- lm (formula =  DEP_DELAY_NEW ~ DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
summary(model22)


#Model3#
lm (formula =  DEP_DELAY_NEW ~ DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
model33 <- lm (formula =  DEP_DELAY_NEW ~ DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
summary(model33)



# Model4#
lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
model44 <- lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = train)
summary(model44)


#Model5#
lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = train)
model55 <- lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = train)
summary(model55)



# Prediction 
predict(model55,train)

P11<- predict(model55,train)
P11

P11- train$DEP_DELAY_NEW

difference <- P11- train$DEP_DELAY_NEW
difference

sqrt(mean(difference^2))


rmse <- sqrt(mean(difference^2))
rmse


#select a subset of numeric varaibles for regresiion modelling 
auto.sel1 <- subset(train, select = c(DEP_DELAY_NEW, DEP_TIME, DISTANCE, ARR_DELAY_NEW))


#Here we will make brief insception of varibles
pairs(auto.sel1, col="blue")


# Performing linear regression and calculating RMSE of test dataset 

# Model1#
lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)
model111 <- lm (formula =  DEP_DELAY_NEW ~ MONTH +DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)

summary(model111)


#Model2#
lm (formula =  DEP_DELAY_NEW ~ DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)
model222 <- lm (formula =  DEP_DELAY_NEW ~ DAY_OF_MONTH + DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)
summary(model222)


#Model3#
lm (formula =  DEP_DELAY_NEW ~ DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)
model333 <- lm (formula =  DEP_DELAY_NEW ~ DAY_OF_WEEK + DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)
summary(model333)



# Model4#
lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)
model444 <- lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY + ARR_DELAY_NEW + DISTANCE , data = test)
summary(model444)


#Model5#
lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = test)
model555 <- lm (formula =  DEP_DELAY_NEW ~ DEP_TIME + ARR_DELAY_NEW + DISTANCE , data = test)
summary(model555)



# Prediction 
predict(model555,test)

P111<- predict(model55,test)
P111

P11- test$DEP_DELAY_NEW

difference <- P11- test$DEP_DELAY_NEW
difference

sqrt(mean(difference^2))


rmse <- sqrt(mean(difference^2))
rmse


#select a subset of numeric varaibles for regresiion modelling 
auto.sel11 <- subset(test, select = c(DEP_DELAY_NEW, DEP_TIME, DISTANCE, ARR_DELAY_NEW))


#Here we will make brief insception of varibles
pairs(auto.sel11, col="purple")






