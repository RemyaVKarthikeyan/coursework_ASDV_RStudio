#Week 1 Workshop 
#Basic Statistics and Data Visualization with R
x=c(1,2,3,4,5,6)
mean(x)
median(x)
length(x)
range(x)
sd(x)
var(x)
summary(x)
pulse=c(120,136,152)
exercise_level=c('light','intense','light')
time_in_mins=c(30,20,20)
exercise_dataframe=data.frame(pulse,exercise_level,time_in_mins)
print(exercise_dataframe)
summary(exercise_dataframe)
exercise_dataframe$exercise_level=as.factor(exercise_dataframe$exercise_level)
summary(exercise_dataframe)
exercise_dataframe[1,3]
exercise_dataframe[,3]
exercise_dataframe[,-3]
exercise_dataframe$pulse>130
exercise_dataframe[exercise_dataframe$pulse>130,]
completed_training=c(1,0,1)
exercise_dataframe[completed_training==1,]

#Part 2: Exploring data graphically
data()
cars
summary(cars)
plot(cars)
hist(cars$speed)
hist(cars$dist)
plot(density(cars$speed))
plot(density(cars$dist))
Thai_tourist=read.csv("Thaitourism1.csv",header=TRUE)
names(Thai_tourist)
head(Thai_tourist)
tail(Thai_tourist)
str(Thai_tourist)
summary(Thai_tourist)
Thai_tourist_full=read.csv("Thaitourism2.csv",header=TRUE)
names(Thai_tourist_full)
head(Thai_tourist_full)
tail(Thai_tourist_full)
str(Thai_tourist_full)
summary(Thai_tourist_full)
Thai_2016=Thai_tourist[Thai_tourist$Year==2016,]
Thai_UK=Thai_tourist_full[Thai_tourist_full$nationality=='UnitedKingdom',]
barplot(Thai_2016$Tourists_1000s)
barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region)
barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region,horiz = TRUE)
barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region,col=c('red','blue'))
barplot(Thai_2016$Tourists_1000s,names.arg = Thai_2016$Region,col=rainbow(8))
hist(Thai_UK$tourists)
hist(Thai_UK$tourists,breaks=16)
hist(Thai_UK$tourists,freq=FALSE)
hist(Thai_UK$tourists,labels=TRUE)
hist(Thai_UK$tourists,xlab='no of customers',col=rainbow(8))
Thai_Europe=droplevels(subset(Thai_tourist_full,region=='Europe'))
boxplot(Thai_Europe$tourists,data=Thai_Europe)
boxplot(tourists~nationality,data=Thai_Europe,col=rainbow(8))
boxplot(tourists~nationality,data=Thai_Europe,col=rainbow(8),horizontal = TRUE)
boxplot(tourists~nationality,data=Thai_Europe,col=rainbow(8),subset = nationality %in% c("France","Russia","Germany"))
pie(Thai_2016$Tourists_1000s)
pie(Thai_2016$Tourists_1000s,labels=Thai_2016$Region,col=rainbow(8))
percent=round(100*Thai_2016$Tourists_1000s/sum(Thai_2016$Tourists_1000s),1)
percent=paste(Thai_2016$Region,"=",percent,"%")
pie(Thai_2016$Tourists_1000s,labels=percent,col=rainbow(8))
install.packages('ggplot2')
library(ggplot2)
qplot(Region,Tourists_1000s,data=Thai_tourist)
qplot(Region,Tourists_1000s,data=Thai_tourist,color=Year)
qplot(Region,Tourists_1000s,data=Thai_tourist,color=Year,size=I(10))
qplot(Region,Tourists_1000s,data=Thai_tourist,geom="boxplot",fill=I("red"))
qplot(tourists,data=Thai_UK,geom="histogram",,fill=I("red"))
qplot(tourists,data=Thai_UK,geom="density",xlab="tourists", ylab="density",fill=I("red"))
?qplot()
----------------------------------------------------------------------------------------------

#Week 2
# Probability Distribution
install.packages('mosaic')
library(mosaic)
plotDist('norm',mean=10,sd=2,kind='density')
plotDist('norm',mean=0,sd=1,kind='density')
plotDist('norm',mean=10,sd=2,kind='cdf')
plotDist('norm',mean=0,sd=1,kind='cdf')
plotDist('binom',size=20,prob=0.7)
plotDist('binom',size=20,prob=0.2)
plotDist('binom',size=100,prob=0.4)
plotDist('chisq',df=2,kind='density')
plotDist('chisq',df=4,kind='density')
plotDist('chisq',df=6,kind='density')
plotDist('chisq',df=8,kind='density')
plotDist('chisq',df=299,kind='density')
plotDist('f',df1=19,df2=29)
plotDist('f',df1=10,df2=20)
plotDist('f',df1=499,df2=499)
plotDist('t',df=4,kind='density')
plotDist('t',df=4,kind='density',col='red')
plotDist('norm',mean=0,sd=1,kind='density',col='blue',add=TRUE)
plotDist('t',df=299,kind='density',col='red')
plotDist('norm',mean=0,sd=1,kind='density',col='blue',add=TRUE)
#genertaing 500 random numbers between 2 and 5 (increasing exponentially plot)
y=runif(500,2,5)
plot(y,exp(y))

#genertaing 500 random numbers between 2 and 5 (decreasing exponentially plot)
y=runif(500,2,5)
plot(y,exp(-y))

plotDist('exp',params=list(1),col='blue',kind='density')
plotDist('exp',params=list(2),col='green',kind='density',add=TRUE)
plotDist('exp',params=list(3),col='orange',kind='density',add=TRUE)
plotDist('exp',params=list(4),col='red',kind='density',add=TRUE)

plotDist('exp',params=list(1),col='blue',kind='cdf')
plotDist('exp',params=list(2),col='green',kind='cdf',add=TRUE)
plotDist('exp',params=list(3),col='orange',kind='cdf',add=TRUE)
plotDist('exp',params=list(4),col='red',kind='cdf',add=TRUE)

#Part 2: Statistical Inference with Standard Normal Distribution
pnorm(-1)
xpnorm(-1)
xpnorm(2.15)
xpnorm(1.65)
xpnorm(1.35)
xpnorm(1.2839)
xpnorm(1.64)
xpnorm(1.96)
xpnorm(-1.65)
xpnorm(c(-1.96,1.96))

#Challenge 1
xpnorm(c(-1.283,1.283))

#Challenge 2
xpnorm(-.3855)

----------------------------------------------------------------------------------------
  
#Week 3
#Correlation Analysis & Confidence Interval Estimations
install.packages('datarium')
install.packages('tidyverse')
install.packages('rcompanion')
install.packages('corrplot')
library(datarium)
library(tidyverse)
library(rcompanion)
library(corrplot)
names(mtcars)
mtcars
plot(mtcars$hp,mtcars$mpg)
cor(mtcars$hp,mtcars$mpg)
cor(mtcars$hp,mtcars$mpg,method='spearman')
discrete_mtcars <- mtcars%>%select(gear , cyl) 
cor(discrete_mtcars, method = "spearman")
continuous_mtcars <- mtcars %>% select(-vs, -am, -gear, -cyl, -carb)
head(continuous_mtcars, 5)
round(cor(continuous_mtcars), digits = 2)
corrplot(cor(continuous_mtcars), method = "number", type = "upper")
continuous_discrete_mtcars <- mtcars %>% select(-vs, -am) 
head(continuous_discrete_mtcars,5)
round(cor(continuous_discrete_mtcars, method = "spearman"), digit=2)
discrete_mtcars <- mtcars%>%select(gear , cyl, carb) 
discrete_mtcars
cor(discrete_mtcars, method = "spearman")

#Section 2: Correlation Between Categorical Variables
mtcars
cramerV(mtcars$am , mtcars$vs)
titanic=read.csv('titanic.csv')
head(as.data.frame(titanic))
str(titanic)
cramerV(titanic$Survived , titanic$Pclass)
cramerV(titanic$Survived , titanic$Sex)

#Challenge 1

cramerV(titanic$Survived,titanic$Fare)
cramerV(titanic$Pclass,titanic$Fare)

cor.test(mtcars$hp , mtcars$vs)
plot(mtcars$vs,mtcars$hp)


set.seed(99) 
n <- 100 
choc_weights <- rnorm(n,20,2)

mean(choc_weights)
sd(choc_weights)


t_score <- qt(p=(1-0.95)/2, df=n-1, lower.tail=FALSE)
margin_error <- t_score*sd(choc_weights)/sqrt(n)

sprintf("The 95 per cent confidence interval for the mean population weight is from %.02f grams to %.02f grams", mean(choc_weights)-margin_error,mean(choc_weights)+margin_error)

data(iris)
head(iris)
dim(iris)
summary(iris)
dim(iris)

mean_setosa = mean(iris[iris$Species=='setosa',4]) 

mean_virginica = mean(iris[iris$Species=='virginica',4]) 
sd_setosa = sd(iris[iris$Species=='setosa',4]) 
sd_virginica = sd(iris[iris$Species=='virginica',4])

margin_error = t_score*sqrt((49*sd_setosa^2 + 49*sd_virginica^2)/98)
mean_diff = mean_virginica - mean_setosa 
sprintf("The 95 per cent confidence interval for the difference in mean petal width between setosa and virginica is from %.02f cm to %.02f cm", mean_diff-margin_error,mean_diff+margin_error)

#Week 4
#Hypothesis Testing in R
install.packages("datarium") 
install.packages("qqplotr") 
install.packages("ggplot2") 
library(ggplot2) 
library(datarium) 
library(qqplotr)
mice2

ggplot(mapping = aes(sample=mice2$before)) + stat_qq_point(size = 2,color = "blue") + stat_qq_line(color="orange") + xlab("Theoretical") + ylab("Sample")
set.seed(499)
n=500
random_sample=rnorm(n,0,1)
ggplot(mapping=aes(sample=random_sample)) +stat_qq_point(size=2,color='blue')+stat_qq_line(color='orange')+xlab("Theoretical")+ylab('Sample')
random_sample2=rexp(500)
ggplot(mapping=aes(sample=random_sample2)) +stat_qq_point(size=2,color='blue')+stat_qq_line(color='orange')+xlab("Theoretical")+ylab('Sample')


set.seed(10)
normal_data=rnorm(200)
shapiro.test(normal_data)
non_normal_data=rexp(200, rate=3)
shapiro.test(non_normal_data)


set.seed(10)
normal_data=rnorm(200)
ks.test(normal_data,'pnorm')

non_normal_data=rexp(200, rate=3)
ks.test(non_normal_data,'pnorm')


#create data frame
df <- data.frame(y=c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8),
                 x1=c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8),
                 x2=c(3, 3, 6, 6, 8, 9, 9, 8, 8, 7, 4, 3, 3, 2, 7))

#perform log transformation
log_y <- log10(y)
hist(log_y,col='coral',main='log_transformed')
hist(y, col='steelblue', main='Original')


choc_weight <-read.csv("chocolate_weights.csv",header=TRUE, row.names = 1)
head(choc_weight)
summary(choc_weight)
dim(choc_weight)
hist(choc_weight$chocolate_data)
mean(choc_weight$chocolate_data)
sd(choc_weight$chocolate_data)

#One sample T-test
t.test(choc_weight$chocolate_data, mu=18, alternative="less")

#Independent Two-sample T-test
data('mtcars')
head(mtcars)
summary(mtcars)

# convert am into factor
mtcars$am <- as.factor(mtcars$am) 
# Use box plot to compare Automatic and Manual 
boxplot(mpg ~ am, data=mtcars, names=c("Automatic", "Manual"), xlab="Automatic or Manual",
        ylab="Miles per Gallon", main="Miles per Gallon for Automatic and Manual Cars")
t.test(mpg ~ am, mtcars)
t.test(mpg ~ am, mtcars, alternative="less")


data("mice2")
head(mice2)
summary(mice2)
boxplot(mice2$before,mice2$after,names=c("Before","After"), 
        xlab="Before & After Treatment", ylab="Weight", main="Mice Weights Before & After Treatment")
t.test(mice2$before,mice2$after,paired=TRUE)
set.seed(42) 
colour <- sample(c("Red","Green","Blue"),replace=TRUE,50) 
size <- sample(c("Big","Small"),replace=TRUE,50) 
colour <- as.factor(colour)
size <- as.factor(size)

table(colour, size)

chisq.test(colour, size)

data("properties") 
head(properties)
contingency_table <- table(properties$property_type, properties$buyer_type) 
print(contingency_table)

ggplot(properties) + aes(x = buyer_type, fill = property_type) + geom_bar() + labs(title ="Number of Properties Purchased by Buyer and Property Type", x = "Type of Buyer", 
                                                                                   y = "Count of Properties by Type")
chisq.test(contingency_table)

education_level <- read.csv("education_level.csv",header=TRUE, row.names = 1) 
head(education_level)
summary(education_level)

education_level$Education <- as.factor(education_level$Education)
summary(education_level)

# Lets visually check for normality 
college_grads=education_level$Wages[education_level$Education=='College']
ggplot(mapping=aes(sample=college_grads)) +
  stat_qq_point(size=2,color='blue')+
  stat_qq_line(color='orange')+
  xlab("Theoretical")+ylab("Sample")

options(scipen=999) 
hist(college_grads)     

uni_grads <-education_level$Wages[education_level$Education=="University"]
ggplot(mapping = aes(sample = uni_grads)) + stat_qq_point(size = 2,color = "blue") + stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")

hist(uni_grads)
wilcox.test(Wages ~ Education, data=education_level)
