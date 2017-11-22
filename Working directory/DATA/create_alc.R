# Álvaro Germán Torres Mora. November 20, 2017. Logistic Regression. https://archive.ics.uci.edu/ml/datasets/Student+Performance

#Wrangling
#Exercise N. 1
library(readr)
math <- read.csv("student-mat.csv", sep = ";")
math
str(math)
dim(math)
por <- read.csv("student-por.csv", sep = ";")
str(por)
dim(por)

#Exercise N. 2
library(dplyr)
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
join_by
math_por <- inner_join(math, por, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))
colnames(math_por)
alc <- select(math_por, one_of("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet"))
str(alc)
dim(alc)

# Exercise N. 3
alc <- select(math_por, one_of(join_by))


notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]


notjoined_columns


for(column_name in notjoined_columns) {
  
  two_columns <- select(math_por, starts_with(column_name))
  
  first_column <- select(two_columns, 1)[[1]]
  

  if(is.numeric(first_column)) {
    
    alc[column_name] <- round(rowMeans(two_columns))
  } else { 
    alc[column_name] <- first_column
  }
}


glimpse(alc)

#Exercise 4
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
alc <- mutate(alc, high_use = alc_use > 2)
#Exercise 5
glimpse(alc)

write.csv(alc, file = "create_alc.csv")

