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

Again, we are still analyzing the given data as it exists. We'll plot the 
five-minute intervals on a time series plot of the averabe number steps taken 
averaged across all days.



```r
# Part 3: Analyze the average daily activity pattern

# Step 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

# First, organize the data by the interval 
by_interval <- group_by(df, interval)

# and put that in a handy data structure with the mean and meaningful names
int_mean <- summarize(by_interval, mean(steps, na.rm=T))
colnames(int_mean) <- c("Interval", "Mean.Steps")

#Step 2. Which 5-minute interval, on average across all the days in the dataset, contains 
# the maximum number of steps?
max_interval <- int_mean$Interval[which.max(int_mean$Mean.Steps)]

# Now we'll plot out the intervals and mark which interval has the most steps
plot(int_mean$Interval, int_mean$Mean.Steps, type="l", 
     main="Average Daily Activity Pattern", xlab="Interval", ylab="Average Steps")
abline(v = max_interval, col = "red", lty = "dashed")
```

![](figure/part3_1-1.png) 


As can be seen on the graph above, the interval with the maximum average steps 
across the entire day is 835.

<hr />
## Imputing missing values

Having analyzed the raw data, we will now impute missing values to keep these 
from skewing the results. 


```r
#Step 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing_values <- sum(is.na(by_day$steps))
```

First we'll calculate the total number of missing values in the dataset which is 2304.

In order to get more meaninful results from our data we're going to impute data to 
replace those missing values. The relatively unsophisticated strategy chosen to impute 
the missing values is to calculate the mean of each 5-minute interval and subsititue 
that for the `NA` values.


```r
#Step 2. Devise a strategy for filling in all of the missing values in the dataset.

# Add the interval mean which we'll use to substitute for the NA values in the raw data
int_mean <- mutate(int_mean, Step.Missing = round(Mean.Steps))

#Step 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

# combine the original data organized by the interval with the mean data set.
# TODO: ought to refactor the field names so that we don't have to specify interval
#       and Interval. It would be cleaner if we used the same name for the same value.
adj_interval <- merge(by_interval, int_mean, by.x = "interval", by.y = "Interval")

# now substitute the interval mean where we have an NA value
adj_interval <- mutate(adj_interval, 
                       imputed = ifelse(is.na(adj_interval$steps), 
                                         adj_interval$Step.Missing, 
                                         adj_interval$steps))
```

Having replaced the `NA` values with the mean of the interval, we'll analyze how this has impacted the results.


```r
#Step 4a. Make a histogram of the total number of steps taken each day.

# reorganize the data so we group it by date and not interval 
adj_day <- group_by(adj_interval, date)

# and find the number of steps taken when including the imputted values
adj_step_by_day <- summarize(adj_day, adj_steps = sum(imputed))

# render the histogram of adjusted steps
hist(adj_step_by_day$adj_steps, 10, main="Histogram of Frequency of Imputed Daily Steps", 
     xlab="Number of Daily Steps", ylab="Frequency")
```

![](figure/part4_4a-1.png) 

As the histogram shows, the number of low step days has decreased and we have results that resemble a normal distribution. We'll next want to analyze the imputed values impact.



```r
#Step 4b. Calculate the mean and median total number of steps taken per day. Do these values 
#         differ from the estimates from the first part of the assignment? What is the impact 
#         of imputing missing data on the estimates of the total daily number of steps?

# find the mean and median from our original data
original_mean <- round(mean(day_step_by_int$total_int), 0)
original_median <- round(median(day_step_by_int$total_int), 0)

# and from our adjusted data
imputed_mean <- round(mean(adj_step_by_day$adj_steps), 0)
imputed_median <- round(median(adj_step_by_day$adj_steps), 0)

# and find the difference
diff_mean = imputed_mean - original_mean
diff_median = imputed_median - original_median
```

The mean for the data with imputed values is 10766 
steps per day while the median is 10762 
steps. This compares to the original values of 9354
and 10395 daily steps respectively.

By introducing the imputed values the mean increased by 1412 steps and the median
increased by 367 steps. By using the imputed values we reduced the number
of days that had very few steps taken and increased the number of days that
represent the mean. The number of days in the historgram above the mean did not
change.

<hr />
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



