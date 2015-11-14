# PA1_template
Jorge Rodriguez  
13 de noviembre de 2015  
##Code to download and unzip the date and then load it into a dataframe 

```r
library(lattice)
library(ggplot2)
setwd("/Users/rodrigblanco/Documents/Formacion/Coursera/05_ReproducibleResearch/Project1")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,"./repdata_data_activity.zip", method = "curl")
unzip("./repdata_data_activity.zip") 
activity <- read.csv("./activity.csv",header = TRUE)
activity$date <- as.Date(activity$date)
```

##What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

```r
stepsDate <- aggregate(activity$steps, list(date = activity$date), sum, na.rm=TRUE)
names(stepsDate) <- c("date","steps")
```

Make a histogram of the total number of steps taken each day

```r
hist(stepsDate$steps, main=" ", breaks=10, xlab="Daily steps")
```

![](PA1_template_files/figure-html/HistogramOfStepsByDate-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsDate$steps)
```

```
## [1] 9354.23
```

```r
median(stepsDate$steps)
```

```
## [1] 10395
```

##What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsInterval <- aggregate(activity$steps, list(interval = activity$interval), mean, na.rm=TRUE)
names(stepsInterval) <- c("interval","steps")
with(stepsInterval,plot(interval, steps, type = "l",xlab = "Interval", ylab = "Steps mean"))
```

![](PA1_template_files/figure-html/PlotMeansByInterval-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsInterval[which.max(stepsInterval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values
Calculate and report the total number of missing values in the dataset.

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. Use the mean for that date.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
actWoNA <- activity
meanDate <- aggregate(actWoNA$steps, list(date = actWoNA$date), mean, na.rm=TRUE)
names(meanDate) <- c("date","steps")
for (i in 1:length(actWoNA$steps)) {
    if (is.na(actWoNA$steps[i])) {
        dateNA <- actWoNA$date[i]
        dateMean <- subset(meanDate, date == dateNA)
        if (is.nan(dateMean$steps)) {
            dateMean$steps <- 0
        }
        actWoNA$steps[i] <- dateMean$steps
    }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsDateNA <- aggregate(actWoNA$steps, list(date = actWoNA$date), sum)
names(stepsDateNA) <- c("date","steps")
hist(stepsDateNA$steps, main=" ", breaks=10, xlab="Total Daily Steps")
```

![](PA1_template_files/figure-html/StatisticsWithoutNA-1.png) 

```r
mean(stepsDateNA$steps)
```

```
## [1] 9354.23
```

```r
median(stepsDateNA$steps)
```

```
## [1] 10395
```
As can be seen, There is no impact in the Mean and Median after replacing all NA with the mean for the date.

##Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity$daytype <- "weekday"

for (j in 1:length(activity$date)) {
    if (weekdays(activity$date[j]) =="sábado") {
        activity$daytype[j] <- "weekend"
    }
    else if (weekdays(activity$date[j]) == "domingo") {
        activity$daytype[j] <- "weekend"
    }
}

activity$daytype <- as.factor(activity$daytype)
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
weekInterval <- aggregate(activity$steps, list(interval = activity$interval, daytype = activity$daytype), mean, na.rm = TRUE)
names(weekInterval) <- c("interval", "daytype","steps")

p <- xyplot(steps ~ interval | daytype, data = weekInterval, layout = c(1,2),type = 'l')
print (p) 
```

![](PA1_template_files/figure-html/PlotMeansByDayType-1.png) 


