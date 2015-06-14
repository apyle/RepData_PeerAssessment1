# Reproducible Research: Peer Assessment 1


```r
require(knitr)
```

```
## Loading required package: knitr
```

```r
opts_chunk$set(echo = TRUE, cache = TRUE, cache.path = "cache/", fig.path="figure/")
```

## Loading and preprocessing the data

Load the data downloaded from the GitHub repo

1. Load the data (i.e. `read.csv()`)

```r
df <- read.csv("activity.csv", head=TRUE)
#summary(df)
```

2. Process/transform the data into a suitable analytical format 

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
by_day <- group_by(df, date)
day_step_by_int <- summarize(by_day, total_int = sum(steps, na.rm=T))
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day 

2. Make a histogram of the total number of steps taken each day



```r
hist(day_step_by_int$total_int, 10, main="Histogram of Frequency of Daily Steps", 
     xlab="Number of Daily Steps", ylab="Frequency")
```

![](figure/unnamed-chunk-3-1.png) 

```r
my_mean <- mean(day_step_by_int$total_int)
my_median <- median(day_step_by_int$total_int)
```

3. Calculate and report the mean and median of the total number of steps taken 
per day

The mean is 9354.2295082 and the median is 10395.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)


```r
by_interval <- group_by(df, interval)
int_mean <- summarize(by_interval, mean(steps, na.rm=T))
colnames(int_mean) <- c("Interval", "Mean.Steps")
plot(int_mean$Interval, int_mean$Mean.Steps, type="l", 
     main="Average Daily Activity Pattern", xlab="Interval", ylab="Average Steps")
```

![](figure/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?


```r
max_interval <- int_mean$Interval[which.max(int_mean$Mean.Steps)]
```

The interval with the maximum average steps across the entire day is 835

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. 
the total number of rows with NAs)


```r
missing_values <- sum(is.na(by_day$steps))
```

The total number of missing values in the dataset is 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
int_mean <- mutate(int_mean, Step.Missing = round(Mean.Steps))
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
adj_interval <- merge(by_interval, int_mean, by.x = "interval", by.y = "Interval")
adj_interval <- mutate(adj_interval, 
                       imputted = ifelse(is.na(adj_interval$steps), 
                                         adj_interval$Step.Missing, 
                                         adj_interval$steps))
adj_day <- group_by(adj_interval, date)
adj_step_by_day <- summarize(adj_day, adj_steps = sum(imputted))
hist(adj_step_by_day$adj_steps, 10)
```

![](figure/unnamed-chunk-8-1.png) 
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#plot(adj_interval$Interval, adj_interval$Mean.Steps, type="l", 
#     main="Average Daily Activity Pattern with Imputed Values", xlab="Interval", 
#     ylab="Average Steps")
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


