#Álvaro Germán Torres Mora. 14.11.2017. Learning R by means of Open Data Science at the University of Helsinki
#Data Wrangling
#Exercise 1
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header = TRUE)

#Document with 12 columns

#Exercise 2.


library(dplyr)

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")


deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)


surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)


strategic_columns <-  select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(lrn14, one_of(keep_columns))


str(learning2014)
learning2014 <- filter(learning2014, Points > 0)
learning2014

#Exercise 3
getwd()
write.csv(learning2014, file = "Learning2014.csv")
learning2014 <- read.csv("Learning2014.csv")
str(learning2014)
head(learning2014)

#Analysis
#Exercise 1
str(learning2014)
dim(learning2014)

#The file learning 2014 contains 166 observations of 7 variables (gender, Age, Attitude, deep, stra, surf, Points)

#Exercise 2
install.packages("ggplot2")
library(ggplot2)
p1 <- plot(learning2014)
p1

#or alternatively,
p11 <- ggplot(learning2014, aes(x = Attitude, y = Points, col = gender))
p11
# Learning2014 shows a statistics whose purpose is to measure the target variable Points, using variables such as gender, age, attitude and approaches such as deep, surface and strategic

#Exercise 3 
p2 <- plot(learning2014, aes(x = Attitude, y = Points, col = gender))
p2

#or alternatively,
p21 <- ggplot(learning2014, aes(x = Attitude, y = Points, col = gender))
p21

p21 <- p11 + geom_point()
p21

p31 <- p21 + geom_smooth(method = "lm")
p31

summary(p31)
my_model <- lm(Points ~ Attitude + gender, data = learning2014)
my_model

summary(my_model)
#Residuals or errors oscilate between 2 and -3 and are closely overrepresented between 0.01 and 0.03 of the x axis
#Exercise 4
summary(my_model)
#It seems that variables gender attitude and Points are correlated to the target points. Standard errors are equal to 0.0595 (Attitude) and 0.9157 (gender). Since Pr value is very low for attitude (8.21e-09) there is a strong correlation with the target. 
#Exercise 5
plot(my_model)

#Residuals or errors oscilate between -20 and 10 which. In order to make the model such residuals should be minimized
