
library(lubridate)
library(RColorBrewer)


##Load the data, filter out missing values, and calculate the total number of steps per day


###change to the working directory of project1
setwd("~/RepData_PeerAssessment1")

#Validate we have the activity file
if(!file.exists("activity.csv") )
    unzip('activity.zip')

org_data <- read.csv("activity.csv", header=TRUE, na.strings = "NA")

#format dates to POSIXct
org_data$date <- ymd(org_data$date)

#Add a column with the day of the week for the observations given date
org_data$dayofweek <- weekdays.POSIXt(org_data$date)

#Filter out missing data
filteredData <- org_data[which(complete.cases(org_data)),]

#We are already using filtered data without NAs
#Calculate the total number of steps taken per day
agg_sum <- aggregate(filteredData$steps ~ filteredData$date , FUN = sum)
names(agg_sum) <- c("date", "steps")



###Histogram of the total number of steps taken each day

png("plot1_totalSteps.png")
par(mfrow = c(1,1))

hist(agg_sum$steps, 
     breaks = 10, 
     col = brewer.pal(n = 10, name = "Set3"),
     xlab = "Number of Steps Per Day", 
     ylab = "Number of Days", 
     main = "Total Number Of Steps per Day",
     xlim = c(0,25000),
     ylim = c(0,20)
)
dev.off()

##Calculate and report the mean and median of the total number of steps taken per day
###What is mean total number of steps taken per day?

mean(agg_sum$steps)


###What is the median number of steps?
median(agg_sum$steps)



###What is the average daily activity pattern?
#This is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

avg_steps_interval <- aggregate(filteredData$steps ~ filteredData$interval, FUN=mean)
names(avg_steps_interval) <- c("interval", "steps")

png("plot2_avgStepsForAllDays.png")
par(mfrow=c(1,1))

plot(x = avg_steps_interval$interval, y = avg_steps_interval$steps, type = "l",
     xlab="Interval (Measured Every 5 Minutes)", ylab="Average Number of Steps",
     main = "Average Number of Steps per 5 Minute Time Interval" 
)
dev.off()
###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxInterval <- avg_steps_interval[avg_steps_interval$steps == max(avg_steps_interval$steps),]
maxInterval
strftime( as.Date(Sys.time()) + minutes(maxInterval$interval), "%H:%M"  )


###Missing Data Analysis and Comparisons

totalObservations <- nrow(org_data)
missingValues <- nrow(org_data[is.na(org_data),])
missingValues

#For missing data, I have imputed the data for missing values by using the mean number of the steps for a given interval from the known data set

new_data <- org_data
 
for(i in 1:nrow(new_data))  # for each row in completed data set with NA's
{
    if (  is.na(new_data$steps[i]) )
        new_data$steps[i] <- as.integer( avg_steps_interval$steps[avg_steps_interval$interval == new_data$interval[i]] )
} 
agg_sum_imputed <- aggregate(new_data$steps ~ new_data$date, FUN=sum)
names(agg_sum_imputed) <- c("date", "steps")

###Side by side comparsion of the data with NAs removed vs NAs being imputed
png("plot3_NAsVsImputed.png")
par(mfrow = c(1,2))

 

#Histograms showing both Missing data removed vs. Missing Data Imputed
hist(agg_sum$steps, 
  breaks = 10, 
  col = brewer.pal(n = 10, name = "Set3"),
  xlab = "Number of Steps Per Day", 
  ylab = "Number of Days", 
  main = "Steps per Day - Excluding NAs",
  xlim = c(0,25000),
  ylim = c(0,25)
)

#Imputed Data
hist(agg_sum_imputed$steps, 
  breaks = 10,
  col = brewer.pal(n = 10, name = "Set3"),
  xlab = "Number of Steps Per Day", 
  ylab = "Number of Days", 
  main = "Steps per Day - Imputed Values",
  xlim = c(0,25000),
  ylim = c(0,25)
)
dev.off()
##The range with imputed data
range(agg_sum_imputed$steps)
###Mean of the imputed data
mean(agg_sum_imputed$steps)

###Median number of steps of the imputed data
median(agg_sum_imputed$steps)

 

###Are there differences in activity patterns between weekdays and weekends?
#Created a factor variable for calculating differences between the weekdays and weekends.
Weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
Weekend <- c("Saturday", "Sunday")

###Assign the correct weekday/weekend factor based-off the defined weekday in the data frame
new_data$day <- ifelse(new_data$dayofweek %in% Weekday,"Weekday", "Weekend")

weekend_avg_steps_interval <- aggregate(steps ~ interval ,data = new_data, subset = day == "Weekend", FUN=mean)
names(avg_steps_interval) <- c("interval", "steps")

weekday_avg_steps_interval <- aggregate(steps ~ interval ,data = new_data, subset = day == "Weekday", FUN=mean)
names(avg_steps_interval) <- c("interval", "steps")


###This is a panel plot showing the average number of steps taken, averaged across all weekday days or weekend days.

png("plot4_WeekdayVsWeekend.png")
par(mfrow = c(2, 1))
plot(x = weekend_avg_steps_interval$interval, 
  y = weekend_avg_steps_interval$steps, 
  xlab = "Interval", 
  ylab="Avg Number of Steps", 
  main = "Weekend - Average Steps per Interval" , 
  type = "l"
)
plot(x = weekday_avg_steps_interval$interval, 
  y = weekday_avg_steps_interval$steps, xlab = "Interval", 
  ylab="Avg Number of Steps", 
  main = "Weekday - Average Steps per Interval" ,
  type = "l"
)
dev.off()
                                 