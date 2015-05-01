Reproducible Research: Peer Assessment 1
========================================================

#####1. Read file & attach it


```r
data<-read.csv("./activity.csv", colClasses=c("numeric", "Date", "numeric"))
attach(data)
```

#####2. Make a histogram of total number of steps taken per day.


```r
totalsteps <- aggregate(steps ~ date, data, sum)
hist(totalsteps$steps,breaks=20,main="Total number of steps taken per day",xlab="Number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

#####3. Calculate & report the mean & median total number of steps taken per day.


```r
meansteps<-mean(totalsteps$steps)
mediansteps<-median(totalsteps$steps)
meansteps
```

```
## [1] 10766
```

```r
mediansteps
```

```
## [1] 10765
```
The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>. The median number of steps taken per day is 1.0765 &times; 10<sup>4</sup>.

#####4. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalsteps <- aggregate(steps ~ interval, data, mean)
plot(intervalsteps$steps,type="l",ylab="Mean number of steps",xlab="5-minute interval",main="Mean steps taken in 5-minute intervals")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

#####5. Determine which 5-minute interval on average across all the days in the dataset, contains the maximum number of steps


```r
maxsteps<-max(intervalsteps$steps,na.rm=TRUE)
maxsteps
```

```
## [1] 206.2
```

```r
maxsubset<-subset(intervalsteps,steps==maxsteps)
maxsubset
```

```
##     interval steps
## 104      835 206.2
```
The maximum mean number of steps taken was in interval number 835, 206.1698 steps being the mean number of steps.

#####6. Calculate and report the total number of missing values in the dataset.


```r
missingvalues<-which(is.na(data))
missingcount<-length(missingvalues)
missingcount
```

```
## [1] 2304
```
The number of missing values in the dataset is 2304.

#####7. Create a new data set with all of the missing values replaced with the mean for that interval.


```r
library(plyr)
get.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
new.data<- plyr::ddply(data[1:3], .(interval), transform,
                          steps = get.mean(steps),
                          date = date,
                          interval = interval)
new.data <- new.data[order(new.data$date,new.data$interval),]
row.names(new.data) <- 1:nrow(new.data)
```


#####8. Make a histogram of the total number of steps taken each day & calculate and report the mean & median total number of steps taken per day.


```r
new.totalsteps <- aggregate(steps ~ date, new.data, sum)
hist(new.totalsteps$steps,breaks=20,main="Total number of steps taken per day",xlab="Number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
new.meansteps<-mean(new.totalsteps$steps)
new.mediansteps<-median(new.totalsteps$steps)
new.meansteps
```

```
## [1] 10766
```

```r
new.mediansteps
```

```
## [1] 10766
```

After replacing NA values with the mean for that interval, the mean step count was 1.0766 &times; 10<sup>4</sup> (as compared to 1.0766 &times; 10<sup>4</sup>). The new median was 1.0766 &times; 10<sup>4</sup> (as compared to 1.0765 &times; 10<sup>4</sup>). This indicates that the substitution of NA values did not significantly change the mean and median calculations, which makes logical sense.
