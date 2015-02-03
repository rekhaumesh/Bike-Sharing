bike_train<-read.csv("train.csv",stringsAsFactors=FALSE,header=TRUE)
nrow(bike_train)
str(bike_train)
bike_train$datetime <- strptime(bike_train$datetime,"%Y-%m-%d %H:%M:%S")
class(bike_train$datetime)
nrow(bike_train)


###Findout whether there are any Missing Values or NAs in the dataset
## You can use any of the following commands
sapply(bike_train,function(x) sum(is.na(x)))
apply(is.na(bike_train),2,sum)
colSums(is.na(bike_train))

###Plotting scatter plot of temp vs atemp:
lm.fit<-lm(registered ~ temp + humidity + windspeed,data=bike_train)
summary(lm.fit)
plot(bike_train$temp,bike_train$atemp)
vcov(lm.fit)
coefficients(lm.fit)
anova(lm.fit)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.fit)
dev.off()

###Feature engineering
##Explorin other variables to improve predictive power

bike_train$weekday <- as.POSIXlt(bike_train$date)$wday
bike_train$date <- "0"
bike_train$time <- "0"
bike_train$date<-format(bike_train$datetime,"%Y-%m-%d")
bike_train$time<-format(bike_train$datetime,"%H:%M:%S")
str(bike_train)
layout(matrix(c(1,2,3,4),2,2))
table(bike_train$holiday)
barplot(table(bike_train$holiday), main="Holiday status")
table(bike_train$workingday)
barplot(table(bike_train$workingday),main="Working day status")
table(bike_train$weekday)
barplot(table(bike_train$weekday),main="Weekdays status")
dev.off()
###Rental activities during the year
layout(matrix(c(1,2),1,2))
bike_train$date<-as.Date(bike_train$date)


plot(bike_train$date,bike_train$registered,col="blue")
plot(bike_train$date,bike_train$casual,col="blue")
dev.off()


##break down to only YEAR. Add one more column
bike_train$year<-as.integer(format(bike_train$date,"%Y"))
bike_train$year<- as.factor(bike_train$year)
layout(matrix(c(1,2),1,2))
plot(bike_train$year,bike_train$registered)

###Breaking down Hours
bike_train$hour<- as.integer(substr(bike_train$time,1,2))
bike_train$hour<- as.factor(bike_train$hour)
plot(bike_train$hour,bike_train$registered,col="red")

####Seasonal activities and weekdays activities
layout(matrix(c(1,2),1,2))
plot(bike_train$weekday,bike_train$registered,col="red")
plot(bike_train$season,bike_train$registered,col="blue")
dev.off()

##Linear regression Model 2 for REGISTERED users
lm.fit2<-lm(registered ~ temp + humidity + windspeed + season + weekday + year + hour, data=bike_train)
summary(lm.fit2)
vcov(lm.fit2)
coefficients(lm.fit2)
anova(lm.fit2)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.fit2)
dev.off()

##Linear regression Model 2 for CASUAL users

lm.fit3<-lm(casual ~ temp + humidity + windspeed + season + weekday + year + hour, data=bike_train)
summary(lm.fit3)
vcov(lm.fit3)
coefficients(lm.fit3)
anova(lm.fit3)
layout(matrix(c(1,2,3,4),2,2))
plot(lm.fit3)
dev.off()
