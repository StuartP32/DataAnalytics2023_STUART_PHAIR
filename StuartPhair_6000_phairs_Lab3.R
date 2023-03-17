data(Titanic)
df <- as.data.frame(Titanic, stringsAsFactors = FALSE)

head(df)
summary(df)



sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

dim(titanic_train)
dim(titanic_test)

detectionTreeModel = rpart(Survived~., titanic_train, method = "class")
detectionTreeModel

rpart.plot(detectionTreeModel)

