EPI_Data = read.csv(file.choose(), header = TRUE)
EPI_Data
head(EPI_Data)
str(EPI_Data)
dim(EPI_Data)
View(EPI_Data)
attach(EPI_Data)
EPI
tf = is.na(EPI)
E = EPI[!tf] 

summary(EPI) 

fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(EPI)


plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")

boxplot(EPI,ECOSYSTEM) 

qqplot(EPI,DALY)

EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

