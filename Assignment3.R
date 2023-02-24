#Assignment 3

#read data
nyt10 <- read.csv(file.choose(), header = TRUE)
nyt10

nyt12 <- read.csv(file.choose(), header = TRUE)
nyt12

nyt13 <- read.csv(file.choose(), header = TRUE)
nyt13

nyt18 <- read.csv(file.choose(), header = TRUE)
nyt18

nyt19 <- read.csv(file.choose(), header = TRUE)
nyt19

nyt20 <- read.csv(file.choose(), header = TRUE)
nyt20

nyt21 <- read.csv(file.choose(), header = TRUE)
nyt21


# a) Create boxplots

#nyt10
boxplot(nyt10$Impressions)
boxplot(nyt10$Clicks)

summary(nyt10$Impressions)
summary(nyt10$Clicks)


#nyt12
boxplot(nyt12$Impressions)
boxplot(nyt12$Clicks)

summary(nyt12$Impressions)
summary(nyt12$Clicks)


#nyt13
boxplot(nyt13$Impressions)
boxplot(nyt13$Clicks)

summary(nyt13$Impressions)
summary(nyt13$Clicks)


#nyt18
boxplot(nyt18$Impressions)
boxplot(nyt18$Clicks)

summary(nyt18$Impressions)
summary(nyt18$Clicks)


#nyt19
boxplot(nyt19$Impressions)
boxplot(nyt19$Clicks)

summary(nyt19$Impressions)
summary(nyt19$Clicks)


#nyt20
boxplot(nyt20$Impressions)
boxplot(nyt20$Clicks)

summary(nyt20$Impressions)
summary(nyt20$Clicks)


#nyt21
boxplot(nyt21$Impressions)
boxplot(nyt21$Clicks)

summary(nyt21$Impressions)
summary(nyt21$Clicks)


#B) normality test
library(nortest)

#nyt10
ad.test(nyt10$Impressions)
ad.test(nyt10$Clicks)
hist(nyt10$Impressions, seq(0,20,2))
hist(nyt10$Clicks, seq(0,6,1))

#nyt12
ad.test(nyt12$Impressions)
ad.test(nyt12$Clicks)
hist(nyt12$Impressions, seq(0,20,2))
hist(nyt12$Clicks, seq(0,6,1))

#nyt13
ad.test(nyt13$Impressions)
ad.test(nyt13$Clicks)
hist(nyt13$Impressions, seq(0,20,2))
hist(nyt13$Clicks, seq(0,6,1))

#nyt18
ad.test(nyt18$Impressions)
ad.test(nyt18$Clicks)
hist(nyt18$Impressions, seq(0,20,2))
hist(nyt18$Clicks, seq(0,6,1))

#nyt19
ad.test(nyt19$Impressions)
ad.test(nyt19$Clicks)
hist(nyt19$Impressions, seq(0,20,2))
hist(nyt19$Clicks, seq(0,6,1))

#nyt20
ad.test(nyt20$Impressions)
ad.test(nyt20$Clicks)
hist(nyt20$Impressions, seq(0,20,2))
hist(nyt20$Clicks, seq(0,6,1))

#nyt21
ad.test(nyt21$Impressions)
ad.test(nyt21$Clicks)
hist(nyt21$Impressions, seq(0,20,2))
hist(nyt21$Clicks, seq(0,6,1))

# C) ecdf and qq plots

#nyt10
plot(ecdf(nyt10$Impressions))
plot(ecdf(nyt10$Clicks))
qqnorm(nyt10$Impressions, main = 'nyt10$Impressions Normal Q-Q Plot')
qqnorm(nyt10$Clicks, main = 'nyt10$Clicks Normal Q-Q Plot')

#nyt12
plot(ecdf(nyt12$Impressions))
plot(ecdf(nyt12$Clicks))
qqnorm(nyt12$Impressions, main = 'nyt12$Impressions Normal Q-Q Plot')
qqnorm(nyt12$Clicks, main = 'nyt12$Clicks Normal Q-Q Plot')

#nyt13
plot(ecdf(nyt13$Impressions))
plot(ecdf(nyt13$Clicks))
qqnorm(nyt13$Impressions, main = 'nyt13$Impressions Normal Q-Q Plot')
qqnorm(nyt13$Clicks, main = 'nyt13$Clicks Normal Q-Q Plot')

#nyt18
plot(ecdf(nyt18$Impressions))
plot(ecdf(nyt18$Clicks))
qqnorm(nyt18$Impressions, main = 'nyt18$Impressions Normal Q-Q Plot')
qqnorm(nyt18$Clicks, main = 'nyt18$Clicks Normal Q-Q Plot')

#nyt19
plot(ecdf(nyt19$Impressions))
plot(ecdf(nyt19$Clicks))
qqnorm(nyt19$Impressions, main = 'nyt19$Impressions Normal Q-Q Plot')
qqnorm(nyt19$Clicks, main = 'nyt19$Clicks Normal Q-Q Plot')

#nyt20
plot(ecdf(nyt20$Impressions))
plot(ecdf(nyt20$Clicks))
qqnorm(nyt20$Impressions, main = 'nyt20$Impressions Normal Q-Q Plot')
qqnorm(nyt20$Clicks, main = 'nyt20$Clicks Normal Q-Q Plot')

#nyt21
plot(ecdf(nyt21$Impressions))
plot(ecdf(nyt21$Clicks))
qqnorm(nyt21$Impressions, main = 'nyt10$Impressions Normal Q-Q Plot')
qqnorm(nyt21$Clicks, main = 'nyt21$Clicks Normal Q-Q Plot')

# D) Significance test

#nyt10
plot(Impressions~Clicks, data = nyt10)
abline(lm(Impressions~Clicks, data = nyt10))
summary(lm(Impressions~Clicks, data = nyt10))

#nyt12
plot(Impressions~Clicks, data = nyt12)
abline(lm(Impressions~Clicks, data = nyt12))
summary(lm(Impressions~Clicks, data = nyt12))

#nyt13
plot(Impressions~Clicks, data = nyt13)
abline(lm(Impressions~Clicks, data = nyt13))
summary(lm(Impressions~Clicks, data = nyt13))

#nyt18
plot(Impressions~Clicks, data = nyt18)
abline(lm(Impressions~Clicks, data = nyt18))
summary(lm(Impressions~Clicks, data = nyt18))

#nyt19
plot(Impressions~Clicks, data = nyt19)
abline(lm(Impressions~Clicks, data = nyt19))
summary(lm(Impressions~Clicks, data = nyt19))

#nyt20
plot(Impressions~Clicks, data = nyt20)
abline(lm(Impressions~Clicks, data = nyt20))
summary(lm(Impressions~Clicks, data = nyt20))

#nyt21
plot(Impressions~Clicks, data = nyt21)
abline(lm(Impressions~Clicks, data = nyt21))
summary(lm(Impressions~Clicks, data = nyt21))

#2
#Normality test and histogram

#nyt18
smallernyt18 = nyt18 %>% filter(Age > 30)
ggplot(data = smallernyt18, mapping = aes(x = Age)) + geom_histogram(binwidth = 10)
ad.test(smallernyt18$Age)

#nyt19
smallernyt19 = nyt19 %>% filter(Age > 30)
ggplot(data = smallernyt19, mapping = aes(x = Age)) + geom_histogram(binwidth = 10)
ad.test(smallernyt19$Age)

#nyt20
smallernyt20 = nyt20 %>% filter(Age > 30)
ggplot(data = smallernyt20, mapping = aes(x = Age)) + geom_histogram(binwidth = 10)
ad.test(smallernyt20$Age)

#nyt21
smallernyt21 = nyt21 %>% filter(Age > 30)
ggplot(data = smallernyt21, mapping = aes(x = Age)) + geom_histogram(binwidth = 10)
ad.test(smallernyt21$Age)

#ECDF and qq plot

#nyt18
plot(ecdf(smallernyt18$Age))
qqnorm(smallernyt18$Age, main = 'smallernyt18$Age Normal Q-Q Plot')

#nyt19
plot(ecdf(smallernyt19$Age))
qqnorm(smallernyt19$Age, main = 'smallernyt19$Age Normal Q-Q Plot')

#nyt20
plot(ecdf(smallernyt20$Age))
qqnorm(smallernyt20$Age, main = 'smallernyt20$Age Normal Q-Q Plot')

#nyt21
plot(ecdf(smallernyt21$Age))
qqnorm(smallernyt21$Age, main = 'smallernyt21$Age Normal Q-Q Plot')

#Significance test

#nyt18
plot(Age~Impressions, data = smallernyt18)
abline(lm(Age~Impressions, data = smallernyt18))
summary(lm(Age~Impressions, data = smallernyt18))

#nyt19
plot(Age~Impressions, data = smallernyt19)
abline(lm(Age~Impressions, data = smallernyt19))
summary(lm(Age~Impressions, data = smallernyt19))

#nyt20
plot(Age~Impressions, data = smallernyt20)
abline(lm(Age~Impressions, data = smallernyt20))
summary(lm(Age~Impressions, data = smallernyt20))

#nyt21
plot(Age~Impressions, data = smallernyt21)
abline(lm(Age~Impressions, data = smallernyt21))
summary(lm(Age~Impressions, data = smallernyt21))



