#Importing the required libraries
library(readr)
library(caret)
library(ggplot2)
library(car)
library(corrplot)

Ecommerce_Customers <- read_csv("UMass/MIE 697 - Stats/PROJECT/Ecommerce Customers.csv")


#viewing the data set
View(colnames(Ecommerce_Customers))
summary(Ecommerce_Customers)


#Data Cleaning - inspect duplicate and missing values
sum(duplicated(Ecommerce_Customers))
sum(is.na(Ecommerce_Customers))


#EDA
#pair plot and correlation plots, excluding the first three columns
pairs(Ecommerce_Customers[,-1:-3])

corrplot(cor(Ecommerce_Customers[,-1:-3]))




#Plotting Yearly Amt Spent against each of the predictors
plot(Ecommerce_Customers$`Avg. Session Length`, Ecommerce_Customers$`Yearly Amount Spent`, xlab="Session Length", ylab="Expenditure")


plot(Ecommerce_Customers$`Length of Membership`, Ecommerce_Customers$`Yearly Amount Spent`, xlab = "Length", ylab="Expenditure")


plot(Ecommerce_Customers$`Time on Website`, Ecommerce_Customers$`Yearly Amount Spent`, xlab="WebTime", ylab="Expenditure")
  

plot(Ecommerce_Customers$`Time on App`, Ecommerce_Customers$`Yearly Amount Spent`, xlab="AppTime", ylab=("Expenditure"))
     

#Running LM models

#Regressing yearly amount spent on membership length
Model1 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~ Ecommerce_Customers$`Length of Membership`)
summary(Model1)

#Regressing yearly amount spent on time on website
Model2 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Time on Website`)
summary(Model2)

#Regressing yearly amount spent on time on app
Model3 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Time on App`)
summary(Model3)

#Regressing yearly amount spent on Avg session length
Model4 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Avg. Session Length`)
summary(Model4)

#Regressing yearly amount spent on combinations of predictors:

#Combinations of 2
Combo1 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~ Ecommerce_Customers$`Length of Membership`+Ecommerce_Customers$`Time on Website`)
summary(Combo1)

Combo2 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~ Ecommerce_Customers$`Length of Membership`+Ecommerce_Customers$`Time on App`)
summary(Combo2)

Combo3 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~ Ecommerce_Customers$`Length of Membership`+Ecommerce_Customers$`Avg. Session Length`)
summary(Combo3)


Combo4 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~ Ecommerce_Customers$`Avg. Session Length`+Ecommerce_Customers$`Time on App`)
summary(Combo4)

Combo5 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~ Ecommerce_Customers$`Avg. Session Length`+Ecommerce_Customers$`Time on Website`)
summary(Combo5)

Combo6 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~ Ecommerce_Customers$`Time on App`+Ecommerce_Customers$`Time on Website`)
summary(Combo6)


#Combinations of 3:
Combo7 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Avg. Session Length`+Ecommerce_Customers$`Time on App`+Ecommerce_Customers$`Time on Website`)
summary(Combo7)

Combo8 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Avg. Session Length`+Ecommerce_Customers$`Time on App`+Ecommerce_Customers$`Length of Membership`)
summary(Combo8)

Combo9 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Avg. Session Length`+Ecommerce_Customers$`Time on Website`+Ecommerce_Customers$`Length of Membership`)
summary(Combo9)

Combo10 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Time on Website`+Ecommerce_Customers$`Time on App`+Ecommerce_Customers$`Length of Membership`)
summary(Combo10)


#Combination of all 4 predictors:
Combo11 <- lm(Ecommerce_Customers$`Yearly Amount Spent`~Ecommerce_Customers$`Avg. Session Length`+Ecommerce_Customers$`Time on App`+Ecommerce_Customers$`Time on Website`+ Ecommerce_Customers$`Length of Membership`)
summary(Combo11)


Models <- list(Combo1, Combo2, Combo3, Combo4, Combo5, Combo6, Combo7, Combo8, Combo9, Combo10, Combo11, Combo12)


#Model Comparison and Variable selection
for (i in Models) {
  print(vif(i))
  print(AIC(i))
  print(BIC(i))
  
}



#Can the model also be used to predict where more customers would spend most of their time on?
set.seed(10)
sample <- sample(c(TRUE,FALSE), nrow(Ecommerce_Customers), replace=TRUE, prob=c(0.8,0.2))
train <- Ecommerce_Customers[sample,]
test <- Ecommerce_Customers[!sample,]

#Training the model using our best predictors
train.EC <- lm(train$`Yearly Amount Spent`~train$`Avg. Session Length`+train$`Time on App`+train$`Length of Membership`)

#predicting on test dataset
predict.EC <-predict(train.EC, test)

#MSE and MAE
mean((predict.EC-test$`Yearly Amount Spent`[1:length(predict.EC)])^2)
MAE(predict.EC,test$`Yearly Amount Spent`[1:length(predict.EC)])




