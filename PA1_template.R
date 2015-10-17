library(dplyr)
library(plyr)
library(ggplot2)
library(knitr)

setwd("/Users/saran/reproduce")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="activity.zip",method="curl")
unzip(zipfile="./activity.zip")
activity <- read.csv("activity.csv")
nrow(activity)
nactivity <- filter(activity, !is.na(steps)) 
nrow(nactivity)
head(nactivity)
stepsperday <- aggregate(steps ~ date, FUN=sum, data=nactivity)
g <- ggplot(stepsperday, aes(steps))
g <- g + geom_histogram(binwidth=5000,fill="blue",alpha=0.5)
g <- g + ggtitle("Steps per Day")
g <- g + labs(x="Steps/day" ,y="Frequency")
print(g)
      meansteps <- mean(stepsperday$steps)
      meansteps
      mediansteps <- median(stepsperday$steps)
      mediansteps
      
      avgsteps <- aggregate(steps ~ interval, FUN = mean, data=nactivity)
      plot(avgsteps,type="l")
      avgsteps$interval[which.max(avgsteps$steps)]
      
      sum(is.na(activity$steps))
      sactivity <- activity
      nsteps <- is.na(sactivity$steps)
      sum(nsteps)
      avginterval <- tapply(sactivity$steps,sactivity$interval,mean,na.rm=TRUE,simplify=TRUE)     #simplify=TRUE
      sactivity$steps[nsteps] <- avginterval[as.character(sactivity$interval[nsteps])]      
      sum(is.na(sactivity$steps))
      
      stepsperday <- aggregate(steps ~ date, FUN=sum, data=sactivity)
      g <- ggplot(stepsperday, aes(x=steps)) 
      g <- g + geom_histogram(binwidth=5000,fill="blue",alpha=0.5)
      g <- g + labs(title ="Steps per Day", x="Steps/day",y="Frequency")
      print(g)
meansteps <- mean(stepsperday$steps)
meansteps
mediansteps <- median(stepsperday$steps)
mediansteps


sactivity <- mutate(sactivity,wtype=ifelse(as.character(weekdays(as.Date(sactivity$date)))
                                            =="Saturday" |
                                             as.character(weekdays(as.Date(sactivity$date)))=="Sunday","Weekend","Weekday"))
head(sactivity)
sactivity$wtype <- as.factor(sactivity$wtype)

weekday <- subset(sactivity,wtype=="Weekday")
head(weekday)
weekend <- subset(sactivity,wtype=="Weekend")
head(weekend)
avgweekday <- aggregate(steps ~ interval, FUN=mean, data=weekday)
avgweekend <- aggregate(steps ~ interval, FUN=mean, data=weekend)

par(mfrow=c(2,1))
par(mar=c(1,1,1,1))
plot(avgweekday$interval,avgweekday$steps,type="l",xlab="",ylab="")
plot(avgweekend$interval,avgweekend$steps,type="l",xlab="",ylab="")
dev.off()
