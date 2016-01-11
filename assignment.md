# Reproducible-Research-Assignment-1
Assignment 1
Rmarkdown file for Assignment 1
===============================
```{r}
setwd("E:/R/Reproducible Research/repdata-data-activity")
```

```{r}
acty <- read.csv("activity.csv", colClass=c('integer', 'Date', 'integer'))
str(acty)

sum(is.na(acty$steps))
```
#What is mean total number of steps taken per day?
```{r}
perday<-tapply(acty$steps, acty$date, sum)
hist(perday,10, main = "Total number of steps taken per day", xlab = "")
```

# Remove NA

```{r}
activity_rm<-acty[which(!is.na(acty$steps)),]
perday<-tapply(activity_rm$steps, activity_rm$date, sum)

mean(perday)
median(perday)
```
# What is the average daily activity pattern?
```{r}
dailyA <-tapply(activity_rm$steps, activity_rm$interval, mean)
plot(y = dailyA, x = names(dailyA), type = "l", xlab = "5-Minute-Interval", 
     main = "Daily Pattern", ylab = "Average steps")
dailyA[dailyA==max(dailyA)]
```

# Input Missing Values

```{r}
sum(is.na(acty$steps))
sum(is.na(acty))

new_act <- acty
new_act[which(is.na(new_act$steps)),1]<-
  dailyA[as.character(new_act[which(is.na(new_act$steps)),3])]
sum(is.na(new_act))
```
# new histogram
```{r}
perday2<-tapply(new_act$steps, new_act$date, sum)

par(mfrow=c(1,2))
hist(perday,10, main = "Total steps taken per day", xlab = "Steps"
     , ylim =c(0, 25))
abline(v = median(perday), col = 4, lwd = 4)
hist(perday2,10, main = "Total steps taken per day  
     (missing values replaced with mean of interval)", xlab = "Steps",
     ylim =c(0, 25))
abline(v = median(perday2), col = 4, lwd = 4)

mean(perday2)
median(perday2)
```

#Are there differences in activity patterns between 
#weekdays and weekends?
```{r}

new_act$wd<-weekdays(new_act$date)
new_act$fwd<- as.factor(c("weekend", "weekday"))
new_act[new_act$wd == "Sunday" | new_act$wd == "Saturday" ,5]<- factor("weekend")
new_act[!(new_act$wd == "Sunday" | new_act$wd == "Saturday"),5 ]<- factor("weekday")

```
# Plot
```{r}
new_act_we <- subset(new_act, fwd == "weekend") 
new_act_wd <- subset(new_act, fwd == "weekday") 
dailyact_we<-tapply(new_act_we$steps, new_act_we$interval, mean)
dailyact_wd<-tapply(new_act_wd$steps, new_act_wd$interval, mean)
par(mfrow=c(2,1))
plot(y = dailyact_wd, x = names(dailyact_wd), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps", 
     ylim =c(0, 250))
plot(y = dailyact_we, x = names(dailyact_we), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekends", ylab = "Average number of steps", 
     ylim =c(0, 250))
```
