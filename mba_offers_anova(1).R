#load in data.csv
library(readr)
library(tidyverse)
library(ggpubr)
library(glue)
library(dplyr)
data = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\mba_offers_anova.csv',header=TRUE)  

#summarize the dataframe
head(data, 10)
summary(data)

#show the levels of the data
levels(data$Degree)
#if the levels need to be added manually (listed as NULL)
levels(data$Degree) <- c("MBAF", "MBAM", "MACC", "MSBA")
levels(data$Degree)

#calculate summary statistics by group
group_by(data, Degree) %>%
  summarise(
    count = n(),
    mean = mean(Offer, na.rm = TRUE),
    sd = sd(Offer, na.rm = TRUE)
  )

#boxplots by group
ggboxplot(data, x="Degree", y="Offer", color="Degree", 
          palette=c("blue", "red", "darkgreen", "darkorange"),
          order=c("MBAF", "MBAM", "MAcc", "MSBA"), ylab="Highest Offer", xlab="Degree")

#mean plot by group
ggline(data, x="Degree", y="Offer", add=c("mean_se", "jitter"),
       order=c("MBAF", "MBAM", "MAcc", "MSBA"), ylab="Highest Offer", xlab="Degree")

#alternative plotting  approaches
#boxplot(Value - Group, data=data, xlab="Group", ylab="Values",
#     frame=FALSE, col=c("blue", "red", "darkgreen", "darkorange"))
#library(gplots)
#plotmeans(Value - Group, data=data, frame=FALSE, xlab="Group",
#     ylab="Value", main="Mean Plot with 95% CI")

#run analysis of variance
one.way <- aov(Offer ~ Degree, data = data)

#summarize the results
summary(one.way)

#conduct Tukey's multiple comparison test for pairs of population means
TukeyHSD(one.way)
