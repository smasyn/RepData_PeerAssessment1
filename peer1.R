## Module 5 Peer Assessment 1 Work
dfrData <- read.csv("./Coursera/Module 5 Peer 1/RepData_PeerAssessment1/activity.csv")

## question 1
## what is the mean total number of steps taken per day
dfrByDate <- group_by(dfrData,date)
qd <- summarise(dfrByDate,sum(steps))
hist(qd$sum)
summary(qd$sum)

## question 2
## what is the average daily activity pattern
dfrInt <- group_by(dfrData, interval)
qt <- summarise(dfrInt,mean(steps,na.rm=TRUE))
plot(qt$int,qt$mean,type="l")
m <- which.max(qt$mean)
qt$interval[m]

# https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet

## question 3
## imputing missing values
# http://www.statmethods.net/input/missingdata.html
m <- dfrData[!complete.cases(dfrData),]
nrow(m)
nrow(m)/nrow(dfrData)
# replace the missing value on a day for a given interval by the mean across all days for the interval
# if value is missing in raw data dfrData, obtain the corresponding interval ID
# match the interval ID in the summary to obtain the observation number
# use the observation number to obtain the mean value
# replace the missing value with the mean value
dfrTidy <- dfrData
for (i in 1:nrow(dfrTidy)){
    if (is.na(dfrTidy$steps[i])){ dfrTidy$steps[i] <- qt$mean[match(dfrTidy$interval[i],qt$interval)]}
}
plot(qt$int,qt$mean,type="l")
points(qtTidy$int,qtTidy$mean,type="l",col="red")

## what is the impact of imputing missing values
qtd <- summarise(group_by(dfrTidy,date),sum(steps))
summary(qd$sum)
summary(qtd$sum)
# median is higher
# mean is equal
# 1st Qu. is higher
# 3rd Qu. is lower
par(mfrow=c(1,2))
hist(qd$sum)
hist(qtd$sum)
par(mfrow=c(1,1))

# show it together
## Prepare data for input to barplot
breaks <- pretty(range(c(qd$sum, qtd$sum),na.rm=TRUE), n=20)
D1 <- hist(qd$sum, breaks=breaks, plot=FALSE)$counts
D2 <- hist(qtd$sum, breaks=breaks, plot=FALSE)$counts
dat <- rbind(D1, D2)
colnames(dat) <- paste(breaks[-length(breaks)], breaks[-1], sep="-")

## Plot it
barplot(dat, beside=TRUE, space=c(0, 0.1), las=2,legend.text=c("raw","tidy"))



## question 4
## are there differences in activity patterns between weekdays and weekends
wdc <- weekdays(as.Date(dfrTidy$date))
wdc[wdc=="Saturday"] <- "weekend"
wdc[wdc=="Sunday"] <- "weekend"
wdc[wdc!="weekend"] <- "weekday"
dfrX <- cbind(dfrTidy,cat=wdc)
qtc1 <- summarise(subset(group_by(dfrX,interval),cat=="weekend"),mean(steps))
qtc2 <- summarise(subset(group_by(dfrX,interval),cat=="weekday"),mean(steps))
par(mfrow=c(2,1))
plot(qtc1$interval,qtc1$mean,type="l",main="weekend",xlab="interval",ylab="avg steps")
plot(qtc2$interval,qtc2$mean,type="l",main="weekday",xlab="interval",ylab="avg steps")
par(mfrow=c(1,1))

# http://sape.inf.usi.ch/quick-reference/ggplot2


