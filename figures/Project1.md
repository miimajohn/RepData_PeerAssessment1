Intro
-----

This was the first project for the **Reproducible Research** course in Coursera's Data Science specialization track. The purpose of the project was to answer a series of questions using data collected from a [FitBit](http://en.wikipedia.org/wiki/Fitbit).

Synopsis
--------

The purpose of this project was to practice:

-   loading and preprocessing data
-   imputing missing values
-   interpreting data to answer research questions

Data
----

The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment was downloaded from the course web site:

-   Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) \[52K\]

The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)

-   **date**: The date on which the measurement was taken in YYYY-MM-DD format

-   **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. \#\# Loading and preprocessing the data

``` r
setwd("/Users/mjb/datasciencecoursera-master")
activity <- read.csv("activity.csv")
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

Sum steps by day while ignoring the missing values, create Histogram, and calculate mean and median.

``` r
# Calculate the total number of steps taken per day while ignoring the missing values in the dataset.
sum_steps_by_day <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
png("steps.png",width=480,height=480,units="px",bg="transparent")
hist(sum_steps_by_day$steps, 
     main = paste("Total Steps Each Day"),  
     border="red",
     col="blue", 
     xlab="Number of Steps")
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
daily_mean <- mean(sum_steps_by_day$steps)
daily_median <- median(sum_steps_by_day$steps)
```

``` r
daily_mean
```

    ## [1] 10766.19

``` r
daily_median
```

    ## [1] 10765

The `mean` is 1.076618910^{4} and the `median` is 10765.

What is the average daily activity pattern?
-------------------------------------------

-   Calculate average steps for each interval for all days.
-   Plot the Average Number Steps per Day by Interval.
-   Find interval with most average steps.

``` r
ave_steps_by_interval <- aggregate(steps ~ interval, activity, mean)
png("aveSteps.png",width=480,height=480,units="px",bg="transparent")
plot(ave_steps_by_interval$interval,ave_steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
max_interval <- ave_steps_by_interval[which.max(ave_steps_by_interval$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

``` r
max_interval
```

    ## [1] 835

Imputing missing values.
------------------------

Note some of the computations have biases due to a number of days/intervals where there are missing values (coded as 𝙽𝙰)

``` r
incomplete <- sum (!complete.cases(activity))
imputed_data <- transform(activity, steps = ifelse(is.na(activity$steps), ave_steps_by_interval$steps[match(activity$interval, ave_steps_by_interval$interval)], activity$steps))
```

Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data.

``` r
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram.

``` r
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
png("impuSteps.png",width=480,height=480,units="px",bg="transparent")
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), border="red",col="green", xlab="Number of Steps")

#Create Histogram to show difference.

hist(sum_steps_by_day$steps, main = paste("Total Steps Each Day"), border="red", col="blue", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("green", "blue"), lwd=10)

dev.off( )
```

    ## quartz_off_screen 
    ##                 2

Calculate new mean and median for imputed data.

``` r
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```

Calculate difference between imputed and non-imputed data.

``` r
mean_diff <- rmean.i - daily_mean
med_diff <- rmedian.i - daily_median
```

Calculate total difference.

``` r
total_diff <- sum(steps_by_day_i$steps) - sum(sum_steps_by_day$steps)
```

-   The imputed data mean is

``` r
rmean.i
```

    ## [1] 10589.69

-   The imputed data median is

``` r
rmedian.i
```

    ## [1] 10766.19

-   The difference between the non-imputed mean and imputed mean is

``` r
mean_diff
```

    ## [1] -176.4949

-   The difference between the non-imputed mean and imputed mean is

``` r
med_diff
```

    ## [1] 1.188679

-   The difference between total number of steps between imputed and non-imputed data is

``` r
total_diff
```

    ## [1] 75363.32

Thus, there were 7.536332110^{4} more steps in the imputed data.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)
png("diffactivity.png",width=480,height=480,units="px",bg="transparent")
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
dev.off()
```

    ## quartz_off_screen 
    ##                 2
