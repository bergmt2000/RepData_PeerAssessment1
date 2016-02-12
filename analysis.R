library(lubridate)
library(RColorBrewer)
echo=TRUE
#change to the working directory of project1
setwd("~/project1")

org_data <- read.csv("activity.csv", header=TRUE, na.strings = "NA")

#format dates to POSIXct
org_data$date <- ymd(org_data$date)

#Add a column with the day of the week for the observations given date
org_data$dayofweek <- weekdays.POSIXt(org_data$date)

#Filter out missing data
filteredData <- org_data[which(complete.cases(org_data)),]

##we are already using filtered out missing values
#Calculate the total number of steps taken per day
agg_sum <- aggregate(filteredData$steps ~ filteredData$date , FUN = sum)
names(agg_sum) <- c("date", "steps")



#What is mean total number of steps taken per day?

##Here's the range of the data
range(agg_sum$steps)

##Make a histogram of the total number of steps taken each day

par(mfrow = c(1,1))
png("plot1.png", width=480, height=480)

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
#Plot line of the most frequent bucket
#abline(h = 16)



##Calculate and report the mean and median of the total number of steps taken per day

##What is mean total number of steps taken per day?
mean(agg_sum$steps)


##What is the median number of steps
median(agg_sum$steps)



#What is the average daily activity pattern?

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avg_steps_interval <- aggregate(filteredData$steps ~ filteredData$interval, FUN=mean)
names(avg_steps_interval) <- c("interval", "steps")
par(mfrow=c(1,1))
png("plot2.png", width=480, height=480)

plot(x = avg_steps_interval$interval, y = avg_steps_interval$steps, type = "l",
     xlab="Interval (Measured Every 5 Minutes)", ylab="Average Number of Steps",
     main = "Average Number of Steps per 5 Minute Time Interval" )

dev.off()

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxInterval <- avg_steps_interval[avg_steps_interval$steps == max(avg_steps_interval$steps),]
maxInterval

#The conversion of minutes to time for the max value is as follows:  (assumes the device's clock 0 is equal to midnight)
strftime( as.Date(Sys.time()) + minutes(maxInterval$interval), "%H:%M"  )


#This shows all the datapoints for the max interval
filteredData[filteredData$interval == maxInterval$interval,]


##Note that there are a number of days/intervals where there are missing values (coded as NA). 
##The presence of missing days may introduce bias into some calculations or summaries of the data.

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalObservations <- nrow(org_data)

missingValues <- nrow(org_data[is.na(org_data),])


#Just some other quick validation on missing values on all three columns
sum(is.na(org_data$steps))

#should be zero
sum(is.na(org_data$interval))

#should be zero
sum(is.na(org_data$date))




#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.


##Now we'll derive a new dataset imputing the values of the missing data with the average for those interval calculated in avg_sum_interval.
##TO use whole numbers (integer), I'll just add 1/2 to the value and call as.integer on the value.. This allows us to round-up if the value is > 1/2

new_data <- org_data

for(i in 1:nrow(new_data))  # for each row in completed data set with NA's
{
    if (  is.na(new_data$steps[i]) )
        new_data$steps[i] <- as.integer( avg_steps_interval$steps[avg_steps_interval$interval == new_data$interval[i]] )
        #new_data$steps[i] <- as.integer( avg_steps_interval$steps[avg_steps_interval$interval == new_data$interval[i]] + .5) #this is with rounding
}


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
###just a review of the new data to see if its any different
agg_sum_imputed <- aggregate(new_data$steps ~ new_data$date, FUN=sum)
names(agg_sum_imputed) <- c("date", "steps")

#Show the side by side comparsion of the data with NAs removed vs NAs being imputed
png("plot3.png", width=480, height=480)
par(mfrow = c(1,2))

#Histograms showing both Missing data removed vs. Missing Data Imputed
hist(agg_sum$steps, 
     breaks = 10, 
     col = brewer.pal(n = 10, name = "Set3"),
     xlab = "Number of Steps Per Day", 
     ylab = "Number of Days", 
     main = "Total Steps per Day",
     xlim = c(0,25000),
     ylim = c(0,25)
)

#Imputed Data
hist(agg_sum_imputed$steps, 
     breaks = 10,
     col = brewer.pal(n = 10, name = "Set3"),
     xlab = "Number of Steps Per Day", 
     ylab = "Number of Days", 
     main = "Total Steps per Day",
     xlim = c(0,25000),
     ylim = c(0,25)
)


#Plot mean and median values
#abline(v = median(agg_sum_imputed$steps), col = "red", lwd = 2)
#abline(v = mean(agg_sum_imputed$steps), col = "green", lwd = 2)
dev.off()

##The range with imputed data
range(agg_sum_imputed$steps)

##The mean of the imputed data
mean(agg_sum_imputed$steps)

##What is the median number of steps
median(agg_sum_imputed$steps)


##Do these values differ from the estimates from the first part of the assignment? 
##Both the mean and median are impacted.
###Missing data mean -> 10766.19 - Imputed Data mean -> 10749.77 == 16.41951
###Missing data median -> 10765  - Imputed Data median -> 10641  == 124

##What is the impact of imputing missing data on the estimates of the total daily number of steps?
###The impact of imputing the data reduced both the average and middle values.  
###For the mean, its   .1525099 % change
###For the median, its 1.151881 % change



#Are there differences in activity patterns between weekdays and weekends?
###Created a factor variable for calculating differences between the weekdays and weekends.
Weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
Weekend <- c("Saturday", "Sunday")

##Assign the correct weekday/weekend factor based-off the defined weekday in the data frame
new_data$day <- ifelse(new_data$dayofweek %in% Weekday,"Weekday", "Weekend")

weekend_avg_steps_interval <- aggregate(steps ~ interval ,data = new_data, subset = day == "Weekend", FUN=mean)
names(avg_steps_interval) <- c("interval", "steps")

weekday_avg_steps_interval <- aggregate(steps ~ interval ,data = new_data, subset = day == "Weekday", FUN=mean)
names(avg_steps_interval) <- c("interval", "steps")

#This is a panel plot showing the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

par(mfrow = c(2, 1))
png("plot4.png", width=480, height=480)
plot(x = weekend_avg_steps_interval$interval, y = weekend_avg_steps_interval$steps, xlab = "Interval", ylab="Avg Number of Steps", main = "Weekend - Average Steps per Interval" , type = "l")
plot(x = weekday_avg_steps_interval$interval, y = weekday_avg_steps_interval$steps, xlab = "Interval", ylab="Avg Number of Steps", main = "Weekday - Average Steps per Interval" , type = "l")

dev.off()