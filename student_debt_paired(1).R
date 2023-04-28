#load in student_debt.cvs and place data into a vector
#Need for exam
library(readr)
library(tidyverse)
library(ggpubr)
library(glue)
debt = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\student_debt_matched(1).csv',header=TRUE)   
priframe <- debt[,c(1:3)]
priframe <- mutate(priframe, Private.Ratio = Private.Aid/Private.Cost)
prisort <- priframe[order(priframe$Private.Ratio),]
pri <- prisort$Private.Debt
pubframe <- debt[,c(4:6)]
pubframe <- mutate(pubframe, Public.Ratio = Public.Aid/Public.Cost)
pubsort <- pubframe[order(pubframe$Public.Ratio),]
pub <- pubsort$Public.Debt
diff <- pri - pub

#make a histogram of the difference vector
hist(diff,main="Difference of Debt")

#check difference data for normality
#with Q-Q plot
qqnorm(diff,main="Q-Q Plot for Difference")
qqline(diff)
#with Shapiro-Wilk test (null hypothesis is that data is normal)
shapiro.test(diff)
#with Kolmogorov-Smirnov test
#ks.test(data, 'pnorm')

#calculate one-sample t-test for difference vector
t.test(diff, mu=0, alt='two.sided')

#calculate two-sample t-test with matched pairs
t.test(pri, pub, paired=TRUE, alt='two.sided')

#compare with two-sample t-test with independent samples
t.test(pri, pub, alt='two.sided')


#TWO SAMPLE estimate for difference in mean
#uncertainty = 1595.23
#estimate = 779.37 plus/minus 1595.23

#One Sample estimate for difference in 
# =

#Matched Paired Analysis
#SalaryOffer File
sal = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\mba_offers_matched2.csv',header=TRUE) 

fin <- sal[,c(1:2)]
finsort <- fin[order(fin$GPA.Finance.),]
finsal <-finsort$Finance.Offer
market <- sal[,c(3:4)]
marketsort <- market[order(market$GPA.Marketing.),]
marketsal <- marketsort$Marketing.Offer
diff <- finsal - marketsal

 