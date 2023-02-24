EPI2010_data <- read.csv(file.choose(), header = TRUE)
EPI2010_data

head(EPI2010_data)
str (EPI2010_data)
tail(EPI2010_data)
summary(EPI2010_data)

library(modeest)
mlv(AIR_E, method = 'mfv')

#Central Tendencies for AIR_E variable
mean(AIR_E)
median(AIR_E)
mlv(AIR_E, method = 'mfv')

#Central Tendency for WATER_E variable
mean(WATER_E)
median(WATER_E)
mlv(WATER_E, method = 'mfv')

#Generate the Boxplots for AIR_E variable
boxplot(AIR_E)

#Generate the Boxplots for WATER_E variable
boxplot(WATER_E)

#Generate Central Tendency values for NOX_pt variable
mean(NOX_pt)
median(NOX_pt)
mlv(NOX_pt, method = 'mfv')

#Generate Central Tendency values for SO2_pt variable
mean(SO2_pt)
median(SO2_pt)
mlv(SO2_pt, method = 'mfv')

#Generate the Boxplots for OZONE_pt variable
boxplot(OZONE_pt)

#Generate the Boxplots for WQI_pt variable
numeric_WQI_pt <- as.numeric(WQI_pt)
boxplot(numeric_WQI_pt)

#Generate Central Tendency values for CLIMATE variable
mean(CLIMATE)
median(CLIMATE)
mlv(CLIMATE, method = 'mfv')

#Generate Central Tendency values for AGRICULTURE variable
mean(AGRICULTURE)
median(AGRICULTURE)
mlv(AGRICULTURE, method = 'mfv')

#Generate the Boxplots for FISHERIES variable
numeric_FISHERIES <- as.numeric(FISHERIES)
boxplot(numeric_FISHERIES) 

#Generate the Boxplots for NMVOC_pt variable
boxplot(NMVOC_pt)

#Boxplot
boxplot(ENVHEALTH,ECOSYSTEM)

#qqplot
qqplot(ENVHEALTH,ECOSYSTEM)

#Part1b
EPI_data <- read.csv(file.choose(), header = TRUE)
EPI_data

attach(EPI_data)
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH = lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval='prediction')
cENV<- predict(lmENVH,NEW,interval='confidence')

# using the response variable as: AIR_E
boxplot(AIR_E, DALY, AIR_H, WATER_H)
Model1 <- lm(AIR_E ~DALY+AIR_H+WATER_H)
Model1
summary(Model1)
coModel1<-coef(Model1)
DALYNEW1<-c(seq(5,95,5))
AIR_HNEW1<-c(seq(5,95,5))
WATER_HNEW1<-c(seq(5,95,5))
NEW1<-data.frame(DALYNEW1,AIR_HNEW1,WATER_HNEW1)
pModel1<- predict(Model1,NEW1,interval='prediction')
cModel1<- predict(Model1,NEW1,interval='confidence')


# using the response variable as: CLIMATE
boxplot(CLIMATE, DALY, AIR_H, WATER_H)
Model2 <- lm(CLIMATE ~DALY+AIR_H+WATER_H)
summary(Model2)
coModel2<-coef(Model2)
DALYNEW2<-c(seq(5,95,5))
AIR_HNEW2<-c(seq(5,95,5))
WATER_HNEW2<-c(seq(5,95,5))
NEW2<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pModel2<- predict(Model2,NEW2,interval='prediction')
cModel2<- predict(Model2,NEW2,interval='confidence')

#Part1c
shapiro.test(ENVHEALTH)
#Shapiro-Wilk normality test
#data:  ENVHEALTH
#W = 0.91613, p-value = 1.083e-08
#Reject 
shapiro.test(DALY)
#Shapiro-Wilk normality test
#data:  DALY
#W = 0.93654, p-value = 1.891e-07
#Reject
shapiro.test(AIR_H)
#Shapiro-Wilk normality test
#data:  AIR_H
#W = 0.92138, p-value = 8.994e-09
#Reject
shapiro.test(WATER_H)
#Shapiro-Wilk normality test
#data:  WATER_H
#W = 0.8597, p-value = 1.679e-12
#Reject


#Lab 2 part 2
#Regression
data2 <- read.csv(file.choose(), header = TRUE)
data2
summary(data2)
dim(data2)
attach(data2)

regModel = lm(ROLL~UNEM+HGRAD)
summary(regModel)

plot(UNEM~HGRAD)
plot(regModel)

help(abline)
abline(regModel)

newData2 <- data.frame(UNEM = 7, HGRAD = 90000)
newData2
predictModel = predict.lm(regModel, newdata = newData2)
predictModel
# Prediction of 81437 fall enrollment with 90,000 high school grads and unemployment is 7%

regModel1 = lm(ROLL~UNEM+HGRAD+INC)
summary(regModel1)

plot(regModel1)

newData3 <- data.frame(UNEM = 7, HGRAD = 90000, INC = 25000)
newData3
predictModel = predict.lm(regModel1, newdata = newData3)
predictModel
# Prediction of 137452 fall enrollment with 90,000 high school grads and unemployment is 7% and income = $25,000

#Exercise 2: Classification
# abalone dataset from UCI repository
# reading the dataset from UCI repository URL
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
# Column names
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                       'rings' )
# summary on abalone
summary(abalone)
# structure of the abalone data
str(abalone)
# summary of the abalone rings column
summary(abalone$rings)

# As shown above, the “rings” variable has a range between 1-29.
# This is the variable that we want to predict, and predicting this many levels
# might not give us the insight we’re looking for.
# For now, we’ll break the rings variable
# into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11,
# and “old” for abalones older than 11.
abalone$rings = as.numeric(abalone$rings)
abalone$rings = cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings = as.factor(abalone$rings)
summary(abalone$rings)
# remove the "sex" variable in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba = abalone
aba$sex = NULL
dim(abalone)
# normalize the data using min max normalization
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] = as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
# After Normalization, each variable has a min of 0 and a max of 1.
# in other words, values are in the range from 0 to 1.
# We’ll now split the data into training and testing sets.
ind = sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain = aba[ind==1,]
KNNtest = aba[ind==2,]
sqrt(2918)
# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852 round it to 55 and use k = 55 # We usually take an Odd number for k value,
# knn model
# knn() is in the "class" library. Make sure to install it first on your RStudio.
library(class)
help("knn") # Read the knn documentation on RStudio.
KNNpred = knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)
# Predicted 403 young, 699 adult, 146 old

#Exercise 3: Clustering
iris
str(iris)

iris2 = iris
iris2$Species = NULL
iris2
dim(iris2)

k.max <- 12

wss<- sapply(1:k.max,function(k){kmeans(iris2[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)
