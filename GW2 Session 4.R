library(readr)
library(tidyverse)
library(ggpubr)
library(glue)
library(dplyr)

food = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\Book1(1).csv',header=TRUE) 


levels(food$Restaurant) <- c('Happy Panda', 'Jade Dragon', 'Lotus Leaf')


#run analysis of variance
one.way <- aov(Time ~ Restaurant, data = food)

#summarize the results
summary(one.way)
TukeyHSD(one.way)

#a)
#At a 90%, 95% and 99% confidence level, our p-value = 1.82e-05 is less than all significant levels
#Therefore we reject the null hypothesis, we can conclude that at least two of the restaurants have a significant difference in delivery time

#b)
#calculate summary statistics by group
group_by(food, Restaurant) %>%
  summarise(
    count = n(),
    mean = mean(Time, na.rm = TRUE),
    sd = sd(Time, na.rm = TRUE)
  )

#Lotus Leaf has the fastest average delivery time at 24.4 minutes