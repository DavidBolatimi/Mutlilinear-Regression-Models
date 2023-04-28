#read in salaries.csv file
library(readr)
library(tidyverse)
library(ggpubr)
library(car)
theme_set(theme_pubr())
data("Salaries", package = "carData")
sample_n(Salaries, 6)

#run a model for salaries with female as the reference level
model_femref <- lm(salary ~ sex, data=Salaries)
summary(model_femref)
Anova(model_femref)
contrasts(Salaries$sex)

#re-run model with male as the reference level
Salaries <- Salaries %>% mutate(sex = relevel(sex, ref = "Male"))
model_malref <- lm(salary ~ sex, data=Salaries)
summary(model_malref)
Anova(model_malref)
contrasts(Salaries$sex)

#run model for Salaries by discipline
modeldisc <- lm(salary ~ discipline, data=Salaries)
summary(modeldisc)
Anova(modeldisc)
contrasts(Salaries$discipline)

#view contrast matrix for rank
matrix <- model.matrix(~rank, data = Salaries)
head(matrix[, -1])
#run model for Salaries by rank
modelrank <- lm(salary ~ rank, data=Salaries)
summary(modelrank)
Anova(modelrank)

model <- lm(salary ~ yrs.service + rank + discipline + sex, data = Salaries)
summary(model)
Anova(model)
