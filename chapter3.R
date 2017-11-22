#Analysis
#Step N. 2
library(dplyr); library(ggplot2)
alchm <- read.csv("create_alc.csv")
alchm
glimpse(alchm)

#The data set contains 35 variables and 382 observations. The dataset shows interaction between alcohol consumption and diverse items such as sex, age, address and others

#Step N. 3. 
m <- glm(high_use ~ failures + absences + sex + famsize, data = alc, family = "binomial")
print(m)
summary(m)
#Variables failures, absences, sex and famsize are chosen in the model for understanding high consumption of alcohol. All the variable have statistically significant relationship with the alcohol consumption.

#Exercise N. 4
plot(m)
boxplot(m)