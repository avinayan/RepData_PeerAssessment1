# Run this file from thr RepData_PeerAssessment1 directory
# Load the File
ActivityData <- read.csv("activity.csv")

# Quick Exploration of Data
head(ActivityData, 10)
str(ActivityData)
summary(ActivityData)

# Data Processing and Transformation
ActivityData$steps <- as.numeric(ActivityData$steps)
ActivityData$minutes <- ActivityData$interval %% 100
ActivityData$hour <- ActivityData$interval %/% 100
ActivityData$interval <- as.factor(ActivityData$interval)

str(ActivityData)
summary(ActivityData)

# Total Number of Steps per day
library(reshape2)
MeltedActivityData <- melt(ActivityData, id = c("date", "hour", "minutes", "interval"))
StepsPerDay <- dcast(MeltedActivityData, date ~ variable, fun.aggregate = sum)
StepsPerDay

# Histogram for Number of Steps per day
png(file = "Histogram1.png")
hist(StepsPerDay$steps, main = "Histogram for Steps/Day", xlab = "Steps/Day", ylab = "# of Days",
     border = "black", col = "blue")
dev.off()

# Mean and Median Number of Steps per day
mean(StepsPerDay$steps, na.rm = TRUE)
median(StepsPerDay$steps, na.rm = TRUE)

# Average Number of Steps per interval
StepsPerInterval <- dcast(MeltedActivityData, interval ~ variable, fun.aggregate = mean, na.rm = TRUE)
StepsPerInterval$interval <- as.numeric(StepsPerInterval$interval)
StepsPerInterval

png(file = "TimeSeries1.png")
plot(StepsPerInterval$interval, StepsPerInterval$steps, type = "l", 
     xlab = "Interval", ylab = "Average Steps", main = "Steps/Interval")
dev.off()

# Interval with Max Avg number of Steps
MaxInterval = which.max(StepsPerInterval$steps)
StepsPerInterval[MaxInterval,]

# Number of missing values
sum(is.na(ActivityData$steps))

# Imputing NAs with the median steps for that interval
ImputedActivityData <- ActivityData
ImputedActivityData[which(is.na(ImputedActivityData$steps)), 1] <- 
       StepsPerInterval[ImputedActivityData[which(is.na(ImputedActivityData$steps)), 3], 2]

# Total Number of Steps per day after Imputing for NAs
MeltedImputedData <- melt(ImputedActivityData, id = c("date", "hour", "minutes", "interval"))
StepsPerDayImputed <- dcast(MeltedImputedData, date ~ variable, fun.aggregate = sum)
StepsPerDayImputed

# Histogram for Number of Steps per day after Imputing for NAs
png(file = "Histogram2.png")
hist(StepsPerDayImputed$steps, main = "Histogram for Steps/Day (NAs Imputed)", xlab = "Steps/Day", ylab = "# of Days",
     border = "black", col = "red")
dev.off()

# Mean and Median Number of Steps per day after Imputing for NAs
mean(StepsPerDayImputed$steps, na.rm = TRUE)
median(StepsPerDayImputed$steps, na.rm = TRUE)

# Add a new variable for Weekday and Weekend
ImputedActivityData[weekdays(as.Date(ImputedActivityData$date)) %in% c("Saturday", "Sunday"), 6] <- "Weekend"
ImputedActivityData[!weekdays(as.Date(ImputedActivityData$date)) %in% c("Saturday", "Sunday"), 6] <- "Weekday"
colnames(ImputedActivityData)[6] <- "day"

WeekdayActivityData <- ImputedActivityData[ImputedActivityData$day == "Weekday",]
WeekendActivityData <- ImputedActivityData[ImputedActivityData$day == "Weekend",]

MeltedWeekdayData <- melt(WeekdayActivityData, id = c("date", "hour", "minutes", "interval", "day"))
MeltedWeekendData <- melt(WeekendActivityData, id = c("date", "hour", "minutes", "interval", "day"))

WeekdayStepsPerInterval <- dcast(MeltedWeekdayData, interval ~ variable, fun.aggregate = mean, na.rm = TRUE)
WeekdayStepsPerInterval$interval <- as.numeric(WeekdayStepsPerInterval$interval)
WeekdayStepsPerInterval

WeekendStepsPerInterval <- dcast(MeltedWeekendData, interval ~ variable, fun.aggregate = mean, na.rm = TRUE)
WeekendStepsPerInterval$interval <- as.numeric(WeekendStepsPerInterval$interval)
WeekendStepsPerInterval

png(file = "TimeSeries2.png")
par(mfrow = c(2,1), mar=c(2,2,1,1), oma = c(0, 0, 0, 0))
plot(WeekdayStepsPerInterval$interval, WeekdayStepsPerInterval$steps, type = "l", 
     xlab = "Interval", ylab = "Average Steps", main = "Weekday", col = "blue", xaxt = "n")
plot(WeekendStepsPerInterval$interval, WeekendStepsPerInterval$steps, type = "l", 
     xlab = "Interval", ylab = "Average Steps", col = "red", main = "Weekend")
dev.off()

