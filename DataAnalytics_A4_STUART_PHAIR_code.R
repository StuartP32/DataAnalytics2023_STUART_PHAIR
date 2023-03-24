NYC_data <- read.csv(file.choose(), header = TRUE)
NYC_data

attach(NYC_data)
head(NYC_data)
str(NYC_data)
tail(NYC_data)
summary(NYC_data)

#Check null values
sapply(NYC_data, function(x) sum(is.na(x)))

#Drop columns with mostly null values
NYC_data1 <- subset(NYC_data, select = -c(COMMERCIAL.UNITS, RESIDENTIAL.UNITS, TOTAL.UNITS, EASE.MENT))
NYC_data1

#Drop rows with null values
NYC_data2 <- na.omit(NYC_data1)
NYC_data2

summary(NYC_data2)

#identify hiw many instances have a sales price of 0
length(which(NYC_data2$SALE.PRICE==0))

#Drop rows with sales price less than 100,000
NYC_data3 <- subset(NYC_data2, SALE.PRICE > 100000)
NYC_data3
summary(NYC_data3)

#Histogram of sales price
library(ggplot2)
ggplot(data=NYC_data3, aes(x=SALE.PRICE)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of Sales Price")

#Histogram of sales price closer
NYC_data4 <- subset(NYC_data3, SALE.PRICE < 10000000)
ggplot(data=NYC_data4, aes(x=SALE.PRICE)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of Sales Price")

#Plot Year Built
boxplot(NYC_data3$YEAR.BUILT)
title('Year Built')

#calculate IQR
IQR(NYC_data3$YEAR.BUILT)
#62

#Regression
#Generate 3 random samples
sample1 <- NYC_data3[sample(nrow(NYC_data3), size=8000), ]
sample1
sample2 <- NYC_data3[sample(nrow(NYC_data3), size=8000), ]
sample2
sample3 <- NYC_data3[sample(nrow(NYC_data3), size=8000), ]
sample3

#Multivariate regression model
model1 <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data = sample1)
summary(model1)
model2 <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data = sample2)
summary(model2)
model3 <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET+LAND.SQUARE.FEET, data = sample3)
summary(model3)


#Decision trees
library(rpart)
library(rpart.plot)
treeModel = rpart(SALE.PRICE~YEAR.BUILT+LAND.SQUARE.FEET+GROSS.SQUARE.FEET, method = "anova", data = NYC_data3)
print(treeModel)
summary(treeModel)
rpart.plot(treeModel)
plot(treeModel)

#Random Forest
library(randomForest)
rfModel <- randomForest(SALE.PRICE~SALE.DATE+YEAR.BUILT+GROSS.SQUARE.FEET+NTA, data = NYC_data3)
rfModel
summary(rfModel)
which.min(rfModel$mse) #min MSE at 169 trees
sqrt(rfModel$mse[which.min(rfModel$mse)]) #rmse 15349576
plot(rfModel)
varImpPlot(rfModel)


#Predictions
pred1 <- data.frame(SALE.DATE=2010, YEAR.BUILT= 1900, GROSS.SQUARE.FEET= 5000,NTA='Lower East Side')
as =predict(rfModel, newdata=pred1)
#31467095

pred2 <- data.frame(SALE.DATE=1930, YEAR.BUILT= 1920, GROSS.SQUARE.FEET= 3000,NTA='Lower East Side')
predict(rfModel, newdata=pred2)
#37089469

pred3 <- data.frame(SALE.DATE=2020, YEAR.BUILT= 2020, GROSS.SQUARE.FEET= 10000,NTA='Lower East Side')
predict(rfModel, newdata=pred3)
#79154845



library(rfUtilities)
rf.significance(rfModel, pred1a)
                

