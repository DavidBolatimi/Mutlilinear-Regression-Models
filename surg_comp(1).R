#load in surg_comp.cvs
library(readr)
library(tidyverse)
library(ggpubr)
library(glue)
theme_set(theme_pubr())
comp = read.csv(file=file.choose(),header=TRUE)   

#make frequency table
table(comp)
prop.table(table(comp))

#make bar plot
counts <- table(comp)   
barplot(counts,xlab="Surgical Complications",col=c("darkblue","red"),
        main='Surgical Complications by Gender', legend=rownames(counts),
        beside=TRUE)

#test comparing two proportions
#using prop.test
counts <- table(comp)   
n1 <- margin.table(counts, margin=1)[2]
n2 <- margin.table(counts, margin=1)[1]
prop.test(x = c(counts[4], counts[3]), 
          n = c(n1, n2), alt='two.sided', correct=FALSE)

#confidence interval for difference of proportions for two groups
n1 <- margin.table(counts, margin=1)[2]
n2 <- margin.table(counts, margin=1)[1]
p1 <- prop.table(counts, margin=1)[4]
p2 <- prop.table(counts, margin=1)[3]
#assuming population proportions are equal
p <- (counts[3]+counts[4])/(n1+n2)
se <- sqrt(p*(1-p)*(1/n1 + 1/n2))
conf.lev <- 0.95
z.score <- qnorm(1-(1-conf.lev)/2)
me <- z.score*se
lcl <- (p1-p2)-me
ucl <- (p1-p2)+me
point <- formatC(p1-p2, digits=3, format="f")
uncert <- formatC(me, digits=3, format="f")
lclslim <- formatC(lcl, digits=3, format="f")
uclslim <- formatC(ucl, digits=3, format="f")
glue("Confidence interval: {point} ± {uncert}, ({lclslim}, {uclslim})")

#test comparing two proportions
#building the z-statistic
z <- (p1-p2)/se
2*pnorm(z, mean = 0, sd = 1, lower.tail = FALSE)
