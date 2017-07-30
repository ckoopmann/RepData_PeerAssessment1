# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data
Load in data, convert it to data.table and show a summary:

```r
library(data.table)
df <- read.csv('activity.csv')
dt <- as.data.table(df)
summary(dt)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?
To get the mean number of steps taken per day, we first calculate the daily sums and use this data to create the histogram as well as the mean.

```r
daily_steps = dt[,.(DailySum = sum(steps, na.rm = TRUE)), by = date]
hist(daily_steps$DailySum, breaks = 10)
```

![](figuresdaily_steps-1.png)<!-- -->

```r
mean(daily_steps$DailySum, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(daily_steps$DailySum, na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
We see a lot of activity in the morning hours.

```r
interval_means = dt[,.(IntervalMean= mean(steps, na.rm = TRUE)), by = interval]
plot(x = interval_means$interval, y = interval_means$IntervalMean, type = 'l')
```

![](figuresactivity_pattern-1.png)<!-- -->

```r
interval_means[which.max(IntervalMean)]
```

```
##    interval IntervalMean
## 1:      835     206.1698
```


## Imputing missing values
Number of rows with at least one NA:

```r
sum(!complete.cases(dt))
```

```
## [1] 2304
```
Fill in all nas with the interval mean and check that none are left afterwards.

```r
dt_replaced = dt
dt_replaced[,IntervalMean := mean(steps, na.rm = TRUE), by = interval]
dt_replaced[is.na(steps),steps := IntervalMean]
sum(!complete.cases(dt_replaced))
```

```
## [1] 0
```
Repeat Analysis per day with the imputed data:

```r
daily_steps = dt_replaced[,.(DailySum = sum(steps, na.rm = TRUE)), by = date]
hist(daily_steps$DailySum, breaks = 10)
```

![](figuresmissing_values_analysis-1.png)<!-- -->

```r
mean(daily_steps$DailySum, na.rm = TRUE)
```

```
## [1] 10749.77
```

```r
median(daily_steps$DailySum, na.rm = TRUE)
```

```
## [1] 10641
```
Both median and mean have increased after the replacement.

## Are there differences in activity patterns between weekdays and weekends?
To plot the different activity patterns I use the wday function from lubridate to avoid problems with different weekday names depending on system language. I use ggplot2 here for easy faceting.

```r
library(lubridate)
library(ggplot2)
dt[,date := as.Date(date)]
dt[,weekday := wday(date)]
weekend = c(1,7)
interval_means_weekend = dt[weekday %in% weekend,.(IntervalMean= mean(steps, na.rm = TRUE)), by = interval]
interval_means_weekend$type = "weekend"
interval_means_weekday = dt[!(weekday %in% weekend),.(IntervalMean= mean(steps, na.rm = TRUE)), by = interval]
interval_means_weekday$type = "weekday"
interval_means = rbind(interval_means_weekend, interval_means_weekday)

ggplot(data = interval_means, aes(x = interval, y = IntervalMean)) + geom_line() + facet_grid(type ~.)
```

![](figuresweekday_analysis-1.png)<!-- -->
One can see a difference with more activity in the morning hours on weekdays, maybe because of the morning commute.
