activity <- read.csv("./activity.csv")
valid_data <- activity[which(activity$steps !="NA"),]

total_steps <- aggregate(steps~date, valid_data, sum)
mean(total_steps$steps)
median(total_steps$steps)
hist(total_steps$steps, main="Number of Steps", xlab= "Total number of steps taken each day", col = "gray", breaks=30)

dayly_average <- aggregate(steps~interval, valid_data, mean)
#names(dayly_average)[2] <- "AverageSteps" 
plot(dayly_average$interval, dayly_average$steps, type = "l", col = "gray", main = "Average daily activity pattern", xlab="5-minute interval", ylab="Average number of steps taken")
dayly_average[dayly_average$steps == max(dayly_average$steps),]

nrow(activity)-nrow(valid_data)
install.packages("gam")
library(gam)
activity1 <- na.gam.replace(activity)

####  the same can be achieved with:
####  activity$steps[is.na(activity$steps)] <- dayly_average$AverageSteps

AllSteps <- aggregate(steps~date, activity1, sum)
hist(AllSteps$steps, main="Number of Steps", xlab = "Total number of steps taken each day", col = "gray", break = 30)
mean(AllSteps$steps)
median(AllSteps$steps)
sum(AllSteps$steps) - sum(total_steps$steps)

library(lattice)
days <- weekdays(as.Date(activity1$date))
activity1$days <- ifelse(days == "Saturday" | days == "Sunday", "Weekend", "Weekday")
NewSteps <- aggregate(steps~interval+days, activity1, mean)
xyplot(steps~interval | days,NewSteps, type="l", layout=c(1,2),xlab="Interval",ylab = "Number of steps")






