# Set working directory
# setwd("ReproducibleAnalysis/Projects/Project1/Data&Codes/RepData_Assessment1")

# Url of the data zip file
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}

activity <- read.csv("activity.csv")

TotalbyDay<- aggregate(steps ~ date, activity, sum)
hist(steps_by_day$steps, main = paste("Total Steps"), col="blue", xlab="Number of Steps")
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)

#3) Calculate and report the mean and median of the total number of steps taken per day
TotalbyDay<-aggregate(activity$steps, list(Day=activity$date), FUN = sum, na.rm=TRUE)
names(TotalbyDay)<-c("Day", "TotalSteps")
head(TotalbyDay)

sum1<- as.data.frame(rbind(
  c("Total Number of Steps", sum(TotalbyDay$TotalSteps)),
  c("Mean Number of Steps", trunc(mean(TotalbyDay$TotalSteps))),
  c("Median Number of Steps", median(TotalbyDay$TotalSteps))
))
colnames(sum1)<-c("Description", "Number")
print(sum1)

AvgbyInterval<-aggregate(activity$steps, list(interval=activity$interval)
                         , FUN = mean, na.rm=TRUE)
names(AvgbyInterval)<-c("interval", "AvgSteps")
x<- which.max(AvgbyInterval$AvgSteps)
maxAvg<-AvgbyInterval[x,]
label <- paste("Peak Avg Steps:",trunc(maxAvg$AvgSteps),"@ Time:"
               ,trunc(maxAvg$interval))

plot(AvgbyInterval$interval,AvgbyInterval$AvgSteps, type = "l"
     , col = "blue", lwd = 2
     ,xlab = "Time Stamp (24HRMM)"
     ,ylab = "Average Number of Steps"
)
title("Average Number of Steps Across All Days per 5 Minute Interval")
text (maxAvg$interval+500, maxAvg$AvgSteps, label, cex = 0.8)

#1) Calculate and report the total number of missing values
nrow(activity)
colSums(is.na(activity))


#Create a new dataset that is equal to the original dataset but with the missing data filled in
activity2<-merge(activity, AvgbyInterval, by.x = "interval")
remove<-subset(activity2, is.na(steps))
remove<-remove[,c(1,4,3)]
colnames(remove)<-c("interval", "steps","date")

keep<-subset(activity2, !is.na(steps))
keep<-keep[,1:3]

activity.imputed<-rbind(keep, remove)

nrow(activity.imputed)

TotalbyDay.imputed<-aggregate(activity.imputed$steps
                              ,list(Day=activity.imputed$date)
                              , FUN = sum, na.rm=TRUE)
names(TotalbyDay.imputed)<-c("Day", "TotalSteps")


hist(TotalbyDay.imputed$TotalSteps, col = "blue", breaks = 100 
     ,xlab = c("Total Number of Steps per Day")
     ,xlim = c(0,25000)
     , ylab = c("Frequency of Number of Daily Steps")
     ,main = "Histogram of Number of Daily Steps"
)

sum2<- as.data.frame(rbind(
  c("Total Number of Steps", trunc(sum(TotalbyDay.imputed$TotalSteps))),
  c("Mean Number of Steps", trunc(mean(TotalbyDay.imputed$TotalSteps))),
  c("Median Number of Steps", trunc(median(TotalbyDay.imputed$TotalSteps)))
))
colnames(sum2)<-c("Description", "Number")
print(sum2)

install.packages("knitr")
 install.packages("tidyr") 
install.packages("lubridate")
install.packages("lattice")
summary<- merge(sum1, sum2, by = "Description")
summary$Number.x<-as.numeric(factor(summary$Number.x))
summary$Number.y<-as.numeric.factor(summary$Number.y)
colnames(summary)<-c("Description", "UnadjustedSteps", "AdjustedSteps")

summary<-mutate(summary, 
                PctChange.From.UnAdjusted = 
                  trunc(100*((UnadjustedSteps - AdjustedSteps)/UnadjustedSteps)))

print(summary[,c(1,4)] )


activity3<-mutate(activity.imputed, DOW = wday(activity$date, label = TRUE))

day.id<-as.data.frame(cbind(
  DOW = c("Sun","Mon","Wed","Fri","Sat","Thurs","Tues"),
  End = c("weekend","weekday","weekday","weekday"
          ,"weekend","weekday","weekday")
))
activity3<-merge(activity3, day.id, by.x = "DOW")   
print(str(activity3))

AvgByEnd.Imputed<-aggregate(activity3$steps,
                            list(TimeStamp=activity3$interval
                                 ,WeekEnd.WeekDay = activity3$End)
                            ,FUN = mean, na.rm=TRUE)
names(AvgByEnd.Imputed)<-c("TimeStamp", "WeekEnd.WeekDay", "AvgSteps")

xyplot(AvgByEnd.Imputed$AvgSteps ~ AvgByEnd.Imputed$TimeStamp 
       | AvgByEnd.Imputed$WeekEnd.WeekDay
       ,layout = c(1, 2)
       ,type = "l"
       ,xlab = "5 minute Time Stamp"
       ,ylab = "Average Number of Steps")