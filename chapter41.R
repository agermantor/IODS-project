#Analysis exercises
#Step 2. Loading the data and exploring it
library(MASS)
data("Boston")

# exploring the dataset
str(Boston)
dim(Boston)


#Boston dataset describes data on Housing Values in Suburbs of Boston. It has 506 observations and 14 variables (crim, zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)

#Step 3. Graphical overview of the data
install.packages("corrplot")
library(corrplot)
library(dplyr)
pairs(Boston)
cor_matrix<-cor(Boston) %>% round(digits = 2)
cor_matrix
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)
#The picture shows the correlation between the different variables. It seems that proportion of owner-occupied units built prior to 1940 is (srongly = close to -1) negatively correlated to the weighted mean of distances to Boston employment centres.It also seems that proportion of non-retail business acres is (strongly = close to 1) correlated to nitrogen oxides concentration (parts per 10 million). By looking, index of accessibility to radial highways and full-value property-tax rate per \$10,000 are the variables the most correlated.

#Step 4
summary(Boston)
boston_scaled <- scale(Boston)
summary(boston_scaled)

# In Boston there are no negative values and there are mean values. In boston scaled there are negative values and means are substracted from the corresponding column. The difference is divided with the standard deviation of the variable.

boston_scaled <- as.data.frame(boston_scaled)
boston_scaled
summary(boston_scaled$crim)
bins <- quantile(boston_scaled$crim)
bins
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
table(crime)
boston_scaled <- dplyr::select(boston_scaled, -crim)
boston_scaled <- data.frame(boston_scaled, crime)
n <- nrow(boston_scaled)
ind <- sample(n,  size = n * 0.8)
train <- boston_scaled[ind,]
test <- boston_scaled[-ind,]

#Step 5
library(MASS)
lda.fit <- lda(crime ~ ., data = train)
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "orange", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

classes <- as.numeric(train$crime)
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)

#Step 6

correct_classes <- test$crime
test <- dplyr::select(test, -crime)
lda.pred <- predict(lda.fit, newdata = test)
lda.pred
table(correct = correct_classes, predicted = lda.pred$class)

#Since we have defined the categories of crimes we have the variables the highest aiming for both th correct and the predicted in high. This means that the prediction based on the model works quite well.

#Step 7

data(Boston)
boston_scaled <- scale(Boston)
dist_eu <- dist(boston_scaled)
dist_eu

km <-kmeans(boston_scaled, centers = 3)
pairs(boston_scaled, col = km$cluster)
set.seed(123)
k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans(boston_scaled, k)$tot.withinss})
install.packages("ggplot2")
library("ggplot2")
# According to Datacamp K-means must include the number of clusters as an argument. Looking at the total cluster sum of squares (WCSS) when their number changes is a good method, paying attention to the drop of such a number. It seems that this is the case when we use:
qplot(x = 1:k_max, y = twcss, geom = 'line')
km <-kmeans(boston_scaled, centers = 2)
pairs(boston_scaled, col = km$cluster)





