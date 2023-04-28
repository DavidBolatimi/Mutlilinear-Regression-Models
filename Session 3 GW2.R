library(readr)
library(tidyverse)
library(ggpubr)
library(glue)
debt = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\student_debt.csv',header=TRUE)   
public <- debt$Public
private <- debt$Private


#Run our test
t.test(public, private, var.equal=TRUE)

# At a 95% confidence interval, our p-value - 0.3365 is greater than our level of significance = 0.05 therefore we fail to reject the null hypothesis
#We can conclude there is no significant difference between the average amount of debt students should expect when they graduate
#if they attend a private or public school

#Uncertainty at a 95% confidence interval (815.8601 - -2374.6001)/2 = 1595.23

#difference estimate 29163.94 - 28384.57 = 779.37
#Our estimate with standard error is 779.37 ± 1595.23