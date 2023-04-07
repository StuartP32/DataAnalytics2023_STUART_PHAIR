cancer_df <- read.csv(file.choose(), header = TRUE)
cancer_df

attach(cancer_df)
head(cancer_df)
str (cancer_df)
tail(cancer_df)
summary(cancer_df)

sapply(cancer_df, function(x) sum(is.na(x)))

hist(ca_cervix)

#Wine data
redwine_df <- read.csv(file.choose(), header = TRUE)
redwine_df

sapply(redwine_df, function(x) sum(is.na(x)))

redwine_df$wine.type <- 1

whitewine_df <- read.csv(file.choose(), header = TRUE)
whitewine_df

sapply(whitewine_df, function(x) sum(is.na(x)))

whitewine_df$wine.type <- 0

#final wine df
wine_df <- rbind(whitewine_df, redwine_df)

attach(wine_df)
head(wine_df)
str (wine_df)
tail(wine_df)
summary(wine_df)

boxplot(quality)

Q <- quantile(wine_df$quality, probs=c(.25, .75), na.rm = FALSE)
iqr = IQR(wine_df$quality)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

eliminated_df<- subset(wine_df, wine_df$quality > (Q[1] - 1.5*iqr) & wine_df$quality < (Q[2]+1.5*iqr))
boxplot(eliminated_df$quality)

#decision tree classification

library(ctree)

output.tree <- ctree(ca_cervix ~ ., data = cancer_df)

