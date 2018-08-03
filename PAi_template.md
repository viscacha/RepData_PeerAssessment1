---
title: "Peer-Graded-Assignment-Course-Project-1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Load and read the data to understand the dataset, remove NA

data_row <- read.csv('activity.csv')
data <- data_row[ with (data_row, { !(is.na(steps)) } ), ]
head(data,20)

#Average daily activity pattern
# 1. Calculate the number of steps taken per day (ignore NA)

by_day <- group_by(data, date)
steps_by_day <- summarise(by_day, total = sum(steps))
steps_by_day

# 2. Make a histogram of the total number of steps taken each day

hist(steps_by_day$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")



# 3. Calculate and report the mean and median of the total number of steps taken per day

summary(steps_by_day)

# Mean is 10766, Median is 10765

# What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


steps_5min_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_5min_interval$interval, steps_5min_interval$steps, type='l', 
+      main="Average number of steps over all days", xlab="Interval", 
+      ylab="Average number of steps")

row_with_max_steps <- which.max(steps_5min_interval$steps)

steps_5min_interval[row_with_max_steps, ]

# The interval 835 has the maximum average value of steps 206.1698.

# Imputing missing values

# Calculate and report the total number of missing values in the dataset

sum(is.na(data_row))

# Total number of rows with NA’s is 2304.

# Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in. 

data_imputed <- data_row
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- steps_5min_interval[
      steps_5min_interval$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}

df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
head(df_imputed_steps_by_day)

# New data set data_no_na created. NA’s were replaced with mean of 5-minute interval.

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")

# get mean and median of imputed data

mean(df_imputed_steps_by_day$steps)
median(df_imputed_steps_by_day$steps)
mean(steps_by_day$total)
median(steps_by_day$total)

# Mean and median data with NA is 10766.19. Mean with NA is the same- 10766.19. While median of data without NA is 10765. 

# Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

# 2. Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"
# convert type_of_day from character to factor
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)
# calculate average steps by interval across all days
df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)
 

qplot(interval, 
      steps, 
      data = df_imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)
