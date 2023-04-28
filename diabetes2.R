library(readr)
library(tidyverse)
library(ggpubr)
library(car)
library(caret)
theme_set(theme_pubr())
data("Pima.tr2", package="MASS") 

#define the sigmoid function
sigmoid <- function(t){ 1/(1+exp(-t)) }     
dummy_frame <- data.frame(x=c(-6,6))
#plot the function 
ggplot(data=dummy_frame) +                  
  stat_function(fun=sigmoid) +
  xlim(-6,6)

#split the data into training and test set
set.seed(123)
training.samples <- Pima.tr2$type %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Pima.tr2[training.samples, ]
test.data <- Pima.tr2[-training.samples, ]

#run model for diabetes due to body mass index
model1 <- glm(type ~ glu, data = train.data, family = binomial)
summary(model1)
summary(model1)$coef
confint(model1)

newdata <- data.frame(glu = c(125,150))
probabilities <- model1 %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
predicted.classes

train.data %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(glu, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Body Mass Index",
    y = "Probability of being diabete-pos"
  )

train_preds <- predict(model1,              # Model to use
                       newdata=newdata,      # Data to use for predictions
                       type="response")            # Return predicted probabilities

table(train_preds)
train.data %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(glu, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Body Mass Index",
    y = "Probability of being diabete-pos"
  ) + geom_vline(xintercept=newdata$glu[1], color='Red') +
  geom_hline(yintercept=train_preds[1], color='Red') +
  geom_vline(xintercept=newdata$glu[2], color='Purple') +
  geom_hline(yintercept=train_preds[2], color='Purple')

#run model for diabetes due to age
model2 <- glm(type ~ age, data = train.data, family = binomial)
summary(model2)
summary(model2)$coef
confint(model2)

newdata <- data.frame(age = c(25,55))
probabilities <- model2 %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
predicted.classes

train.data %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(age, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Age",
    y = "Probability of being diabete-pos"
  )

train_preds <- predict(model2,              # Model to use
                       newdata=newdata,      # Data to use for predictions
                       type="response")            # Return predicted probabilities

table(train_preds)
train.data %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(age, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Age",
    y = "Probability of being diabete-pos"
  ) + geom_vline(xintercept=newdata$age[1], color='Red') +
  geom_hline(yintercept=train_preds[1], color='Red') +
  geom_vline(xintercept=newdata$age[2], color='Purple') +
  geom_hline(yintercept=train_preds[2], color='Purple')

#run model for diabetes due to glu+bmi+age
model3 <- glm(type ~ glu + bmi + age, data = train.data, family = binomial)
summary(model3)
summary(model3)$coef
confint(model3)

newdata <- data.frame(glu = c(100,150), bmi = c(25,45), age = c(25,55))
probabilities <- model3 %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
predicted.classes

train.data %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(age, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Age",
    y = "Probability of being diabete-pos"
  )

train_preds <- predict(model3,              # Model to use
                       newdata=newdata,      # Data to use for predictions
                       type="response")            # Return predicted probabilities

table(train_preds)
train.data %>%
  mutate(prob = ifelse(type == "Yes", 1, 0)) %>%
  ggplot(aes(age, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Age",
    y = "Probability of being diabete-pos"
  ) + geom_vline(xintercept=newdata$age[1], color='Red') +
  geom_hline(yintercept=train_preds[1], color='Red') +
  geom_vline(xintercept=newdata$age[2], color='Purple') +
  geom_hline(yintercept=train_preds[2], color='Purple')
