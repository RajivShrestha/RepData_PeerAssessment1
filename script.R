## include various libraries
library(knitr)
opts_chunk$set(echo = TRUE)

library(dplyr)
library(lubridate) #for date 
library(ggplot2)   #for plotting

#getting data
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
setwd("./RepData_PeerAssessment1/")
zipName <- "activity.zip"

if(!file.exists(zipName)){
  download.file(fileUrl,zipName)
}  

fName <- unzip(zipName, files= NULL, list = FALSE)
data <- read.csv(fName, header=TRUE, sep=",",colClasses = c("numeric", "character",
                                                                "integer"))

#tidying the data. Change the date using lubridate
data$date <- ymd(data$date)

#check the data
str(data)
head(data)

##What is mean total number of steps taken per day?
steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print


# use ggplot for making the histogram:
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")

## mean and median of steps per day
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)

mean_steps
# [1] 10766.19
median_steps
# [1] 10765

#average daily activity pattern
interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))

ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")

interval[which.max(interval$steps),]

## Imputing missing values
#total missing values in dataset
sum(is.na(data$steps))


#filling missing data by using mean of same 5-minute interval
data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]

#check for no missing values
sum(is.na(data_full$steps))
## [1] 0

## average with missing value filled in
steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

## histogram of steps taken if 5 minute interval with missing data filled in
ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")

## Calculate the mean and median steps with the filled in values:
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)

mean_steps_full
# [1] 10766.19
median_steps_full
# [1] 10766.19

## Difference in activity pattern between weekday and weekend
# factor variable to indicate weekday or weekend
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)

#average steps in 5 minute interval for weekdays and weekends
interval_full <- data_full %>%
  group_by(interval, weektype) %>%
  summarize(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)