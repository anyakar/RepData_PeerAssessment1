##{r data-loading}
unzip("activity.zip")
activity <- read.csv(file="activity.csv")

## Transforming the date into POSIXlt date format.
##{r dataformatting}
activity$date <- strptime(activity$date, format="%Y-%m-%d")
##

### What is mean total number of steps taken per day?
##Computing a histogram of the total number of steps taken each day.

##{r sumsteps}
require(plyr)
StepsSummary <- ddply(activity, .(date), summarize, 
                      meanSteps = mean(steps), 
                      medianSteps = median(steps, na.rm = TRUE), 
                      sumSteps = sum(steps))

hist(StepsSummary$sumSteps, col = "grey", main = c("Histogram of Number of Steps"), 
     xlab = c("Number of Steps Taken per Day"))

#tapply(x, list(group[row(x)], col(x)), sum)
#t(sapply(split(as.data.frame(x), group), colSums))
#aggregate(x, list(group), sum)[-1]


##{r meanmedian}
Mean = round(mean(StepsSummary$sumSteps, na.rm = TRUE), 0)
Median = median(StepsSummary$sumSteps, na.rm = TRUE)
##
#For the total number of steps taken each day, the mean is **`r Mean`** and the median is **`r Median`**.

### What is the average daily activity pattern?

#1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

##{r sumdailysteps}
require(plyr)
StepsTimeInt <- ddply(activity, .(interval), summarize, 
                      timeIntSteps = round(mean(steps, na.rm = TRUE), 0))

maxRow <- which.max(StepsTimeInt$timeIntSteps)
tInt <- StepsTimeInt$interval[maxRow]

## Of the 5-minute intervals, on average across all the days in the dataset, the interval **`r iInt`** had the maximum number of steps.


### Imputing missing values
# Note that there are a number of days/intervals where there are missing
# values (coded as `NA`). The presence of missing days may introduce
# bias into some calculations or summaries of the data.

##{r NAvalues}
NAvalues = sum(colSums(is.na(activity)))

# Total number of missing values in the dataset is **`r NAvalues`**.

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Creating an index of missing values; 
# Using previous calculation of steps mean for every given interval, inserting values for missing observations, while populating remaining rows with NAs 
# Replacing all NAs with 0 and summing them up to get new imputed values

##{r imputingNAValues}

activity <- read.csv(file="activity.csv")
activity$date <- strptime(activity$date, format="%Y-%m-%d")
StepsTimeInt <- ddply(activity, .(interval), summarize, 
                      timeIntSteps = round(mean(steps, na.rm = TRUE), 0))

NAindex <- !complete.cases(activity$steps)

stepsMean = StepsTimeInt$timeIntSteps[which(StepsTimeInt$interval == activity$interval)]
activity$stepsMean[NAindex] <- stepsMean[NAindex]
activity[is.na(activity)] <- 0
activity$stepsNoNAs <- activity$steps + activity$stepsMean

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
##{r creating new imputed dataset}
activityImputed <- subset(activity, select = c(stepsNoNAs, date, interval))

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
##{r new histogram}
StepsSummaryImputed <- ddply(activityImputed, .(date), summarize, 
                             sumSteps = sum(stepsNoNAs))

hist(StepsSummaryImputed$sumSteps, col = "grey", main = c("Histogram of Number of Steps With Imputed Missing Values"), 
     xlab = c("Number of Steps Taken per Day"))

##{r computing mean median with Imputed data}
MeanImputed = round(mean(StepsSummaryImputed$sumSteps, na.rm = TRUE), 0)
MedianImputed = median(StepsSummaryImputed$sumSteps, na.rm = TRUE)
##

# Mean and median values are lower with imputed values -- since the distribution has a right-skew, imputed values on average are smaller. The difference between the Mean and the Median increases.

### Are there differences in activity patterns between weekdays and weekends?

# For this part the `weekdays()` function may be of some help here. Use
# the dataset with the filled-in missing values for this part.

##{r weekly pattern}
activityImputed$weekdays <- as.factor(weekdays(activityImputed$date))
labelWeekdays = c("weekday", "weekday", "weekend", "weekend", "weekday", "weekday", "weekday")
levels(activityImputed$weekdays) = as.factor(labelWeekdays)

require(plyr)
#StepsSummaryImputed <- ddply(activityImputed, .(interval, weekday), summarize, 
#      sumSteps = sum(stepsNoNAs))

StepsTimeImputed <- ddply(activityImputed, .(interval, weekdays), summarize, 
                          timeIntSteps = round(mean(stepsNoNAs),0))

library("ggplot2")
ggplot(StepsTimeImputed, aes(group = factor(weekdays), x = interval, y = timeIntSteps, color = weekdays)) + geom_line() 
