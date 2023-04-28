#devtools::install_github("kassambara/datarium")
#data("marketing", package = "datarium")
#head(marketing, 4)
#write.csv(marketing, "C:\\Users\\goofus\\Desktop\\marketing.csv", row.names=TRUE)

#read in marketing.csv file
library(readr)
library(tidyverse)
library(ggpubr)
library(caret)
library(car)
theme_set(theme_pubr())
data = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\marketing(1).csv',header=TRUE)   

#run analysis for sales vs youtube
#make an initial scatter plot of sales vs youtube
ggplot(data, aes(x = YouTube, y = Sales)) +
  geom_point() +
  stat_smooth()

#make an initial scatter plot of sales vs facebook
ggplot(data, aes(x = Facebook, y = Sales)) +
  geom_point() +
  stat_smooth()

#make an initial scatter plot of sales vs newspaper
ggplot(data, aes(x = Newspaper, y = Sales)) +
  geom_point() +
  stat_smooth()

#run correlation analysis for pairs of variables
attach(data)
cor(Sales, YouTube)
cor(Sales, Facebook)
cor(Sales, Newspaper)
cor(data[,2:5])
library(psych)
pairs(data[,2:5], pch = 19) #lower.panel = NULL for upper panels only


#run model for sales and youtube + facebook + newspaper add buys
model1 <- lm(Sales ~ YouTube + Facebook + Newspaper, data = data)
summary(model1)
summary(model1)$coefficients
confint(model1)

#use residuals to check for required conditions for error variable
res1 <- resid(model1)
#check residuals for normal distribution
summary(res1)
hist(res1, xlab="Residuals", main="Residuals for Model 1", breaks=12)
qqnorm(res1)
qqline(res1)
plot(density(res1))
shapiro.test(res1)
#Null hypothesis is that it is normal
#create residual vs fitted plot to check for heteroscedasticity
plot(fitted(model1), res1, ylab="Residuals", xlab="Predicted Sales")
abline(0,0)

#assess quality of model

summary(model1)
confint(model1)
anova(model1)
#calculate the percentage error
sigma(model1)*100/mean(Sales)
#examine residuals and check for outliers
plot(model1)
outlierTest(model1)


#re-run model without newspaper
model2 <- lm(Sales ~ YouTube + Facebook, data = data)
summary(model2)
summary(model2)$coefficients
confint(model2)

#use residuals to check for required conditions for error variable
res2 <- resid(model2)
#check residuals for normal distribution
summary(res2)
hist(res2, xlab="Residuals", main="Residuals for Model 2", breaks=12)
qqnorm(res2)
qqline(res2)
plot(density(res2))
shapiro.test(res2)
#create residual vs fitted plot to check for heteroscedasticity
plot(fitted(model2), res2, ylab="Residuals", xlab="Predicted Sales")
abline(0,0)

#assess quality of model
summary(model2)
confint(model2)
anova(model2)
#calculate the percentage error
sigma(model2)*100/mean(Sales)
#examine residuals and check for outliers
plot(model2)
outlierTest(model2)

#run partial F-test comparing models

anova(model1)  #full model
anova(model2)  #reduced model
anova(model2, model1)  #anova(reduced, full)

# Split the data into training and test set
set.seed(123)
training.samples <- data$Sales %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#make predictions with the additive model using the training and test sets
pred2 <- model2 %>% predict(test.data)
# model performance
# (a) prediction error, RMSE
RMSE(pred2, test.data$Sales)
# (b) R-square
R2(pred2, test.data$Sales)
#predict the prediction interval for a single run
predval <- data.frame(YouTube=100, Facebook=50)
predict(model2, newdata=predval, interval="prediction", level=0.95)
#predict the confidence interval for the average of a series of runs
predict(model2, newdata=predval, interval="confidence", level=0.95)

#re-run the model with YouTube*Facebook interaction term
model3 <- lm(Sales ~ YouTube + Facebook + YouTube:Facebook, data = data)
summary(model3)
summary(model3)$coefficients
confint(model3)

#use residuals to check for required conditions for error variable
res3 <- resid(model3)
#check residuals for normal distribution
summary(res3)
hist(res3, xlab="Residuals", main="Residuals for Model 3", breaks=12)
qqnorm(res3)
qqline(res3)
plot(density(res3))
shapiro.test(res3)
#create residual vs fitted plot to check for heteroscedasticity
plot(fitted(model3), res3, ylab="Residuals", xlab="Predicted Sales")
abline(0,0)

#assess quality of model
summary(model3)
confint(model3)
anova(model3)
#calculate the percentage error
sigma(model3)*100/mean(Sales)
#examine residuals and check for outliers
plot(model3)
outlierTest(model3)

#make predictions with the interaction model using the training and test sets
pred3 <- model3 %>% predict(test.data)
# model performance
# (a) prediction error, RMSE
RMSE(pred3, test.data$Sales)
# (b) R-square
R2(pred3, test.data$Sales)
#predict the prediction interval for a single run
predval <- data.frame(YouTube=100, Facebook=50)
predict(model3, newdata=predval, interval="prediction", level=0.95)
#predict the confidence interval for the average of a series of runs
predict(model3, newdata=predval, interval="confidence", level=0.95)

#run partial F-test comparing models
anova(model3)  #full model
anova(model2)  #reduced model
anova(model2, model3)  #anova(reduced, full)

