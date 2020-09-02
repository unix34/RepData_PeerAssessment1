---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity <- read.csv("./activity.csv")
```
## What is mean total number of steps taken per day?


```r
stepsByDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```


```r
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
stepsByDayMean
```

```
## [1] 9354.23
```

```r
stepsByDayMedian
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
averageStepsPerTimeBlock[which.max(averageStepsPerTimeBlock$meanSteps),]
```

```
##     interval meanSteps
## 104      835  206.1698
```

## Imputing missing values

```r
# subset dataset where there are no NAs
    activity_no_NA <- activity[which(!is.na(activity$steps)),]
  
  # calculate the mean steps for each interval
    interval_only <- activity_no_NA %>% group_by(interval) %>% summarise(average=mean(steps))

    # convert the average to integer
    interval_only$average <- as.integer(interval_only$average)
    
    #subset dataset where steps have NAs
    activity_na <- activity[which(is.na(activity$steps)),]
    
    # fill NAs with average steps based on interval
    activity_na$steps <- ifelse(activity_na$interval==interval_only$interval,interval_only$average)
    
    # row bind the datasets that do not have NAs and the dataset where NAs are replaced with
    # mean values
    activity_imput <- rbind(activity_no_NA,activity_na)
nrow(activity_na)
```

```
## [1] 2304
```

```r
 # Compute the total number of steps per day
  stepsByDay_imput <- activity_imput %>% group_by(date) %>% summarise(stepsperday = sum(steps))
    qplot(stepsperday,data=stepsByDay_imput,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
totalstepsperday_imput <- activity_imput %>% group_by(date) %>% summarise(stepsperday = sum(steps))
  mean_n_median <- totalstepsperday_imput %>% summarise(average=mean(stepsperday),median=median(stepsperday))
  mean_n_median
```

```
## # A tibble: 1 x 2
##   average median
##     <dbl>  <int>
## 1  10750.  10641
```
## Are there differences in activity patterns between weekdays and weekends?

```r
activity_imput$dateType <-  ifelse(as.POSIXlt(activity_imput$date)$wday %in% c(0,6), 'weekend', 'weekday')
averagedActivityImput <- aggregate(steps ~ interval + dateType, data=activity_imput, mean)
ggplot(averagedActivityImput, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
