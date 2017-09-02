# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
# file
data_file <- "activity.zip"
# load the data
activities <- read_csv(data_file)
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```


## What is mean total number of steps taken per day?

```r
# Calculate the total number of steps taken per day
steps_per_day <- activities[!is.na(activities$steps),] %>% 
                  group_by(date)  %>% 
                  summarise(total_per_day = sum(steps), mean=mean(steps))

# Make a histogram of the total number of steps taken each day
ggplot(steps_per_day, aes(date, total_per_day)) + geom_bar(stat = "identity") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Calculate and report the mean and median of the total number of steps taken per day
mean(steps_per_day$total_per_day)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$total_per_day)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
steps_per_interval <- activities[!is.na(activities$steps),] %>% 
                  group_by(interval)  %>% 
                  summarise(total_per_interval = sum(steps), mean = mean(steps), median=median(steps))

ggplot(steps_per_interval, aes(interval, mean)) + geom_line() + xlab("5-minute") + ylab("Mean Steps Taken Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
steps_per_interval %>% top_n(total_per_interval, n=1)
```

```
## # A tibble: 1 x 4
##   interval total_per_interval     mean median
##      <int>              <int>    <dbl>  <int>
## 1      835              10927 206.1698     19
```

## Imputing missing values

```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activities$steps))
```

```
## [1] 2304
```

```r
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
imputed_activities <- inner_join(activities, steps_per_interval) %>%
                        mutate(steps = ifelse(is.na(steps), mean, steps))
```

```
## Joining, by = "interval"
```

```r
# confirm there is no na
sum(is.na(imputed_activities$steps))
```

```
## [1] 0
```

```r
#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
steps_per_day_imputed <- imputed_activities %>%
                  group_by(date)  %>% 
                  summarise(total_per_day = sum(steps), mean=mean(steps))

ggplot(steps_per_day_imputed, aes(date, total_per_day)) + geom_bar(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#Calculate and report the mean and median of the total number of steps taken per day
mean(steps_per_day_imputed$total_per_day)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_imputed$total_per_day)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
imputed_activities = imputed_activities %>% 
                        mutate( day = if_else( weekdays(date) %in% c("Saturday","Sunday"), "Weekend", "Weekday") )
#steps_per_day_imputed$day = as.factor(steps_per_day_imputed$day)


#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
steps_per_interval_weekday <- imputed_activities %>% 
                  group_by(interval, day)  %>% 
                  summarise(total_per_interval = mean(steps))

## Plot using ggplot2
ggplot(steps_per_interval_weekday, aes(interval, total_per_interval)) + geom_line(color = "steelblue4", lwd = 1) + 
    facet_wrap(~day, ncol = 1) + labs(title = expression("Fig 4"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
