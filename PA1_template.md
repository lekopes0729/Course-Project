Reproducible Research: Peer Assessment 1
================================================================

## Loading and preprocessing the data  
1. Load the date
```{r}
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day and make a histogram of the total number of steps taken each day
```{r}
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps per Day"), col="green",xlab="Number of Steps")
```

2. Calculate and report the mean and median of the total number of steps taken per day
- Mean
```{r}
rmean <- mean(steps_by_day$steps)
rmean
```

- Median
```{r}
rmedian <- median(steps_by_day$steps)
rmedian
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Daily Activity Pattern")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
- Max Interval
```{r}
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
- Total number of missing value
```{r}
NATotal <- sum(!complete.cases(data))
NATotal
```

2. Devise a strategy for filling in all of the missing values in the dataset.Use Mean for that day 
```{r}
StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
    obs <- data[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
new_activity <- data
new_activity$steps <- fillNA
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```{r}
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)
```

- Mean
```{r}
rmeantotal <- mean(StepsTotalUnion$steps)
rmeantotal
```

- Median
```{r}
rmediantotal <- median(StepsTotalUnion$steps)
rmediantotal
```

The mean is the same, the median has a little difference.

## Are there differences in activity patterns between weekdays and weekends?
```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```


