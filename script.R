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
x <- "File Exists"
if(!file.exists(zipName)){
  download.file(fileUrl,zipName)
  x <- "No file"
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
median_steps

#average daily activity pattern
