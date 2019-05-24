---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


 Loading and preprocessing the data

```r
activityData <- read.csv ("activity.csv", header = T, sep = ",", stringsAsFactors = F)
```
Now we convert the date column to the appropriate format:

```r
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## What is mean total number of steps taken per day?
the following lines calculate the total number of steps per day and the mean number of daily steps:

```r
library (dplyr)
AvgDay <- activityData %>% group_by(date) %>%
  summarize(total.steps = sum(steps, na.rm = T), 
            mean.steps = mean(steps, na.rm = T))
```
 we can construct the histogram of the total steps:

```r
echo = TRUE
library(ggplot2)
g <- ggplot(AvgDay, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),  
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
The histogram shows the largest count around the 10000-12500 step class thus we can infer that the median will be in this interval, the data is symmetrically distributed around the center of the distribution, except for one class at the extreme left

a summary of the data, which will include the mean and the median:

```r
summary(AvgDay$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
summary (AvgDay$mean.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.1424 30.6979 37.3785 37.3826 46.1597 73.5903       8
```

## What is the average daily activity pattern?
 the data isgrouped by interval this time and then calculate the mean of each interval goup:

```r
echo=TRUE
AvgInterval <- activityData %>% group_by(interval) %>%
  summarize(mean.steps = mean(steps, na.rm = T))

g <- ggplot(AvgInterval, aes(x = interval, y = mean.steps))
g + geom_line() + theme(axis.text = element_text(size = 12), 
                        axis.title = element_text(size = 14, face = "bold")) + 
  labs(y = "Mean number of steps") + labs(x = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
it is observed that the largest amount of steps occurs between time intervals 500 and 1000. The maximum average number of steps is: 206 and occurs in time interval #835


## Imputing missing values
calculating the percentage of missing data as well as the number of rows that contain an NA.

```r
mean(is.na(activityData$steps))
```

```
## [1] 0.1311475
```

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```
to evaluate the effect of filling in NAs with estimated values we will create a new dataset and then perform a comparison.
There are several alternatives we can use to fill the NAs, for example:
  
 1.Using the average steps during the day to fill in NAs within the same day. The drawbacks of this method are that we have seen there is a large variation thoughout the day  and more importantly we observed in the summary of the AvgDay that there are 8 days when no data was recorded so in those cases we would not have an estimator.
 2.Using the average steps per interval. We will use this metric as our first attempt to fill in the NAs.
First, we will check for missing values in the interval column within AvgInterval, where we stored the mean number of steps for each 5 min interval:

```r
sum(is.na(AvgInterval$mean.steps))
```

```
## [1] 0
```
Since there are no missing values in this variable we will use it to fill in for NAs. Next we create a duplicate of the original data named newData and we will draw the appropriate values AvgInterval:

```r
newData <- activityData
```
to fill in missing values we check at each row if the column interval is NA, when the condition is true we look for the corresponding interval (index), we search for this particular interval in the AvgInterval data and extract it to a temporary variable values. Last we choose only the column of interest from values, which is the mean.steps and assign this number to the corresponding position in the newData set. We use a for loop to run through all the rows

```r
for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    index <- newData$interval[i]
    value <- subset(AvgInterval, interval==index)
    newData$steps[i] <- value$mean.steps
  }
}
head(newData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
We use a similar method as before to group the data by date and calculate daily totals

```r
newAvg <- newData %>% group_by(date) %>%
  summarize(total.steps = sum(steps, na.rm = T))
```
And we can construct the histogram:

```r
echo=TRUE
  g <- ggplot(newAvg, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
This figure shows, similarly to the first histogram, symmetrically distributed data around the maximum without the column in the extreme left (which contained the days with missing data). One must notice that filling values with the interval means increases the frequencies in the 10000-12500 class, which contains the median
For a more quantitative comparison lets review the 5 number summaries and standard deviations of the original data AvgDay vs the data with the imputed values newData

```r
summary (AvgDay$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
sd(AvgDay$total.steps, na.rm=T)
```

```
## [1] 5405.895
```

```r
sd(newAvg$total.steps, na.rm=T)
```

```
## [1] 3974.391
```
The mean and the median stay the same, however the 1st quantile of the new data slides closer to the mean. When we look at the standard deviation values, we can also observe that the new data has a smaller standard deviation, thus the effect of imputing NAs with the mean values for the time intervals is a decrease in the spread, we obtained a distribution that is more concentrated around the center of gravity.



## Are there differences in activity patterns between weekdays and weekends?

```r
newData$day <- ifelse(weekdays(newData$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
```
And we can construct the histogram:

```r
echo=TRUE
g <- ggplot(newAvg, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),
                                            axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
Next we create two subsets, one containing the weekend and one containing the weekday data:

```r
wkend <- filter(newData, day == "weekend")
wkday <- filter(newData, day == "weekday")
```
Then, similarly to section 2, we group by the intervals and calculate the mean number of steps for each time interval. Since the day column is lots during the grouping, we add it again to the wkend and wday dataframes. Lastly, we merge both data sets into one named newInterval

```r
wkend <- wkend %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
wkend$day <- "weekend"

wkday <- wkday %>%
  group_by(interval) %>%
  summarize(mean.steps = mean(steps)) 
wkday$day <- "weekday"

newInterval <- rbind(wkend, wkday)
newInterval$day <- as.factor(newInterval$day)
newInterval$day <- relevel(newInterval$day, "weekend")
```
The two panel plot is now created, using the day column as a factor to spearate the weekday from the weekend timeseries.

```r
echo=TRUE
g <- ggplot (newInterval, aes (interval, mean.steps))
g + geom_line() + facet_grid (day~.) + theme(axis.text = element_text(size = 12), 
                                             axis.title = element_text(size = 14)) + labs(y = "Number of Steps") + labs(x = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
it is seen that the activity profiles between weekdays and weekends greatly differ. During the weekdays, activity peaks in the morning between 7 and 9 and then the activity remains below ~100 steps. In contrast, the weekend data does not show a period with particularly high level of activity, but the activity remains higher than the weekday activity at most times and in several instances it surpases the 100 steps mark and it is overall more evenly distributed throughout the day.
                                                                                         
