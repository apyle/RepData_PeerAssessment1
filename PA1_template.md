# Reproducible Research: Peer Assessment 1
apyle@github.com  
2015-09-13  

<br />
<hr />
Reproducible Research Project 1 Writeup
=======================================

## Introduction
The first Peer Assessment project in the September, 2015 [Coursera][1] 
[Reproducible Research Class][2] calls for an analysis of data gathered from a 
personal fitness device measuring the wearer's steps in five-minute intervals 
over the period of two months. Details about the assignment can be found in the 
instructor's [GitHub source repository][3]. It was completed using [RStudio][4] 
and various R libraries and packages.

To begin, we will take care of some housekeeping items to make sure the knitr 
library can properly generate this document. It needs to show all R code 
executed for this analysis. We also want to reference the directory where 
generated plots will be stored.


```r
# keep the chatter down so the write-up stays clean
options(warn = -1)

# make sure we have access to the knitr library for setup and housekeeping
# suppressWarnings should not be necessary since we have used the warn option
# but using it to reinforce the idea that we want to keep the write-up clean.
suppressWarnings(library(knitr))

# set knitr default option values, specifically echo, turning on the cache and
# setting its location, and specifying the figure directory
opts_chunk$set(echo = TRUE, cache = TRUE, cache.path = "cache/", fig.path="figure/")
```

<hr />
## The Data

As stated in the assignment, the data to be analyzed is orginates "from a 
personal activity monitoring device. This device collects data at 5 minute 
intervals through out the day. The data consists of two months of data from an 
anonymous individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day."

The data is stored in a file, `activity.csv` which is included from the 
assignment GitHub repo. With this in mind we'll load this file directly into R 
rather than download it from GitHub. Then we'll transform it into a format to 
make it easier to analyze than its raw format.


```r
# Part 1: Load the data downloaded from the GitHub repo

# The data is in the activity.csv file and contains 17,568 observations. Each 
# observation includes:

# steps: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-minute interval in which measurement was taken

#Step 1. Load the data (i.e. `read.csv()`)
df <- read.csv("activity.csv", head=TRUE)

#Step 2. Process/transform the data into a suitable analytical format 
suppressWarnings(library(dplyr))

# create a grouping for the data by date. We'll use this for summarizations
by_day <- group_by(df, date)
```

<hr />
## What is mean total number of steps taken per day?

For the first part of the data analysis we will use the data as is, with several 
unknown, or NA, values. We will ignore these values for now.

We are going to summarize the data by date and then create a histogram of the 
total number of steps taken per day.


```r
# Part 2: Analyze the data as it exists ignoring NA values

# Step 1. Calculate the total number of steps taken per day 
day_step_by_int <- summarize(by_day, total_int = sum(steps, na.rm=T))

# Step 2. Make a histogram of the total number of steps taken each day
hist(day_step_by_int$total_int, 10, main="Histogram of Frequency of Daily Steps", 
     xlab="Number of Daily Steps", ylab="Frequency")
```

![](figure/part2_2-1.png) 

Now we'll calculate the mean and median of the total number of steps taken per day.


```r
# Step 3. Calculate and report the mean and median of the total number of steps taken per day
my_mean <- round(mean(day_step_by_int$total_int), 0)
my_median <- median(day_step_by_int$total_int)
```

For the dataset where we are ignoring empty values, the mean is 9354 steps 
taken per day and the median is 10395 steps taken per day.

<hr>
## What is the average daily activity pattern?

Again, we are still analyzing the given data as it exists. We'll plot the five-minute intervals on a time series plot of the averabe number steps taken averaged across all days.
###Analyze the data as it exists



```r
# Part 3: Analyze the average daily activity pattern

# Step 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

# First, organize the data by the interval 
by_interval <- group_by(df, interval)

# and put that in a handy data structure with the mean and meaningful names
int_mean <- summarize(by_interval, mean(steps, na.rm=T))
colnames(int_mean) <- c("Interval", "Mean.Steps")

# using the data we're also going to find which interval has the most steps
max_interval <- int_mean$Interval[which.max(int_mean$Mean.Steps)]

# Now we'll plot out the intervals and add a line for the most steps
plot(int_mean$Interval, int_mean$Mean.Steps, type="l", 
     main="Average Daily Activity Pattern", xlab="Interval", ylab="Average Steps")
abline(v = max_interval, col = "red", lty = "dashed")
```

![](figure/part3_1-1.png) 

####Step 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval <- int_mean$Interval[which.max(int_mean$Mean.Steps)]
#abline(v = max_interval, col="red", lwd = 2)
```

Response: The interval with the maximum average steps across the entire day is 835

## Imputing missing values

###Impute missing values and analyze the resulting data

####Step 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing_values <- sum(is.na(by_day$steps))
```

Response: The total number of missing values in the dataset is 2304

####Step 2. Devise a strategy for filling in all of the missing values in the dataset. 

Response: The relatively unsophisiticated strategy chosen to impute the missing values is to calculate the mean of each 5-minute interval and subsititue that for the `NA` values.


```r
int_mean <- mutate(int_mean, Step.Missing = round(Mean.Steps))
```

####Step 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
adj_interval <- merge(by_interval, int_mean, by.x = "interval", by.y = "Interval")
adj_interval <- mutate(adj_interval, 
                       imputted = ifelse(is.na(adj_interval$steps), 
                                         adj_interval$Step.Missing, 
                                         adj_interval$steps))
adj_day <- group_by(adj_interval, date)
adj_step_by_day <- summarize(adj_day, adj_steps = sum(imputted))
```

####Step 4a. Make a histogram of the total number of steps taken each day.


```r
hist(adj_step_by_day$adj_steps, 10, main="Histogram of Frequency of Imputed Daily Steps", 
     xlab="Number of Daily Steps", ylab="Frequency")
```

![](figure/part4_4a-1.png) 

####Step 4b. Calculate the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
original_mean <- mean(day_step_by_int$total_int)
original_median <- median(day_step_by_int$total_int)

imputed_mean <- mean(adj_step_by_day$adj_steps)
imputed_median <- median(adj_step_by_day$adj_steps)

diff_mean = imputed_mean - original_mean
diff_median = imputed_median - original_median
```

Response: The mean for the data with imputed values is 1.0765639\times 10^{4} while the
median is 1.0762\times 10^{4}. This compares to the origial values of 9354.2295082
and 10395 respectively.

By introducing the imputed values the mean increased by 1411.4098361 and the median
increased by 367. By using the imputed values we reduced the number
of days that had very few steps taken and increased the number of days that
represent the mean. The number of days in the historgram above the mean did not
change.

## Are there differences in activity patterns between weekdays and weekends?

###Determine if the day of the week makes a difference in the number of steps in the imputed data

####Step 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
adj_step_by_interval <- mutate(adj_interval, day_of_week = weekdays(as.Date(date)))
adj_step_by_interval <- mutate(adj_step_by_interval, day_factor = ifelse(day_of_week == "Saturday" | day_of_week == "Sunday", "weekend", "weekday"))
adj_step_by_interval$day_factor <- as.factor(adj_step_by_interval$day_factor)
adj_step_for_graph <- group_by(adj_step_by_interval, day_factor, interval)
adj_step_for_graph <- summarize(adj_step_for_graph, final_steps = sum(imputted))
```

####Step 2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(ggplot2)

g <- ggplot(adj_step_for_graph, aes(interval, final_steps)) + 
        facet_grid(day_factor ~ .) +
        geom_line() +
        xlab("Interval") +
        ylab("Weekday type") +
        ggtitle("Differences in Steps Taken Based on Weekday v. Weekend")

dev.set(2)
```

```
## pdf 
##   2
```

```r
png(file="figure/part5_2.png", width=672)
print(g)
dev.off()
```

```
## pdf 
##   2
```

![](figure/part5_2.png)

As shown in the graphs above, the weekday activity starts sooner in the day then on the weekends. There is also more steps taken during the weekdays than on the weekends.

## Conclusion


## Acknowlegements

I originally wrote this for the June 2015 class. I improved it for the September 
2015 class based on the feedback from my anonymous peer reviewers. I also 
incorporated ideas from the following classmates after my peer assesment of their 
assignments: born2rununix, Glabenweek, and karthikr77.  
  
  
[1]: https://www.coursera.org
[2]: https://class.coursera.org/repdata-032
[3]: https://github.com/rdpeng/RepData_PeerAssessment1
[4]: https://www.rstudio.com/



