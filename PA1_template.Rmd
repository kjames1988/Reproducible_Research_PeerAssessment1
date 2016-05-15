Reproducible Research Project 1
==================================
```{r reading data file , echo= TRUE}

activity_data <- read.csv("activity.csv",header = TRUE,sep = ",")

#dim(activity_data)
#names(activity_data)
#head(activity_data)
#tail(activity_data)
#str(activity_data)
```

-What is mean total number of steps taken per day?

-Calculate and report the mean and median of the total number of steps taken per day

-Calculate the total number of steps taken per day


```{r cleaning data, echo=TRUE}
## removing the NA's
activity_data<- activity_data[complete.cases(activity_data), ]


##months of October and November, 2012 
## finding patterns october grepl("-10-", activity_data$date )*1
## finding patterns november
##grepl("-11-", activity_data$date )*1


#total by month
totaloct <- sum(activity_data$steps*grepl("-10-", activity_data$date )*1,na.rm = TRUE)
totalnov <- sum(activity_data$steps*grepl("-11-", activity_data$date )*1,na.rm = TRUE)
totalsum <- totaloct + totalnov

#sum for october each day

totatday_oct <- rep(0,31)
## rep create a vector of 31 0's
totatday_oct_mean <- rep(0,31)
for (i in 1:31){
    if(i < 10)
        pattern <- paste ("-10-0",i, sep = "")
    else pattern <- paste ("-10-",i,sep = "")
    totatday_oct[i] <- sum(activity_data$steps*grepl(pattern , activity_data$date )*1)
    if(length(which(grepl(pattern , activity_data$date )*1==1))==0)
        totatday_oct[i]<--1   
    
}

#sum for november each day

totatday_nov <- rep(0,30)
## rep create a vector of 31 0's

for (i in 1:30){
    if(i < 10)
        pattern <- paste ("-11-0",i,sep = "")
    else pattern <- paste ("-11-",i,sep = "")
    totatday_nov [i] <- sum(activity_data$steps*grepl(pattern , activity_data$date )*1,na.rm = TRUE)
    if(length(which(grepl(pattern , activity_data$date )*1==1))==0)
        totatday_nov[i]<--1   
}

##Calculate and report the mean and median of the total number of steps taken per day
totalstepsday_mean <- as.integer(mean(c(totatday_oct, totatday_nov)[which(c(totatday_oct, totatday_nov)!=-1)]))

totalstepsday_median <- as.integer(median(c(totatday_oct, totatday_nov)[which(c(totatday_oct, totatday_nov)!=-1)]))


totalstepsday_mean
totalstepsday_median

```
The `mean` number of steps taken each day was `r totalstepsday_mean` steps.

The `median` number of steps taken each day was `r totalstepsday_median` steps.

```{r histogram, echo=TRUE}
## Make a histogram of the total number of steps taken each day
hist(c(totatday_oct, totatday_nov)[which(c(totatday_oct, totatday_nov)!=-1)], col= "grey",main="the total number of steps taken each day" , xlab = "steps")

```
=================================================
What is the average daily activity pattern?

```{r activity pattern, echo=TRUE}
##What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
##the average number of steps taken, averaged across all days (y-axis)


## we made a matrix for the intervals by steps
interval <- matrix(activity_data$steps,,288, byrow = TRUE)

## 2 is here because we found the mean of the column in the steps
avgInterval <- apply(interval,2,mean)


uniqueIntervals <- unique(activity_data$interval)

plot(uniqueIntervals, avgInterval,type= "l" ,xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

##Which 5-minute interval, on average across all the days in the dataset,
##contains the maximum number of steps?
max_intervals <- uniqueIntervals[which.max( avgInterval)]
max_intervals
```
The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is `r max_intervals`.


#Imputing missing values  Compare imputed to non-imputed data.

1 Devise a strategy for filling in all of the missing values in the dataset. 

2 The strategy does not need to be sophisticated. For example, you could use the  the mean for that 5-minute interval, etc.

=======================================


```{r missing value, echo=TRUE}
##Imputing missing values

activity_data_2 <- read.csv("activity.csv",header = TRUE,sep = ",")




##Create a new dataset that is equal to the original dataset but with the missing data filled in.
## we found all the dates in oct and nov. "which" identifies the position of the NAs
NaDates <- unique(activity_data_2$date)[which(c(totatday_oct, totatday_nov)==-1)]
avgInterval<-avgInterval
NaDates <- as.vector(NaDates)

for (i in 1: length(NaDates))
    activity_data_2$steps[ which(grepl(NaDates[i] , activity_data_2$date )*1==1)]<-avgInterval


## WE NEED TO INPUT EITHER THE MEAN INSTEAD OF NAs
totatday_oct_2 <- rep(0,31)

for (i in 1:31){
    if(i < 10)
        pattern <- paste ("-10-0",i, sep = "")
    else pattern <- paste ("-10-",i,sep = "")
    totatday_oct_2[i] <- sum(activity_data_2$steps*grepl(pattern , activity_data_2$date )*1)
    
}

#sum for november each day

totatday_nov_2 <- rep(0,30)
## rep create a vector of 31 0's

for (i in 1:30){
    if(i < 10)
        pattern <- paste ("-11-0",i,sep = "")
    else pattern <- paste ("-11-",i,sep = "")
    totatday_nov_2[i] <- sum(activity_data_2$steps*grepl(pattern , activity_data_2$date )*1)
    
}



##Calculate and report the mean and median of the total number of steps taken per day
totalstepsday_mean_2<- mean(c(totatday_oct_2, totatday_nov_2)[which(c(totatday_oct_2, totatday_nov_2)!=-1)])

totalstepsday_median_2 <- median(c(totatday_oct_2, totatday_nov_2)[which(c(totatday_oct_2, totatday_nov_2)!=-1)])

totalstepsday_mean_2
totalstepsday_median_2

## Make a histogram of the total number of steps taken each day
hist(c(totatday_oct_2, totatday_nov_2)[which(c(totatday_oct_2, totatday_nov_2)!=-1)], col= "grey",main="the total number of steps taken each day" , xlab = "steps")
hist(c(totatday_oct, totatday_nov)[which(c(totatday_oct, totatday_nov)!=-1)], col= "red",main="the total number of steps taken each day" , xlab = "steps",add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("grey", "red") )


mean_diff <- totalstepsday_mean_2 -totalstepsday_mean
median_diff <- totalstepsday_median_2 - totalstepsday_median

mean_diff
median_diff

##Are there differences in activity patterns between weekdays and weekends?

#head(activity_data_2)
activitydata_weekdays <- weekdays(  as.Date(activity_data_2$date))



activitydata_weekdays [-which(grepl("Saturday" , activitydata_weekdays)|(grepl("Sunday" ,activitydata_weekdays)))] <- rep( "Weekday",length(activitydata_weekdays [-which(grepl("Saturday" ,activitydata_weekdays)|(grepl("Sunday" ,activitydata_weekdays)))] ))
activitydata_weekdays [which(grepl("Saturday" , activitydata_weekdays)|(grepl("Sunday" ,activitydata_weekdays)))] <- rep( "Weekend",length(activitydata_weekdays [which(grepl("Saturday" ,activitydata_weekdays)|(grepl("Sunday" ,activitydata_weekdays)))] ))

activity_data_2 <- cbind(activity_data_2,activitydata_weekdays)

WeekDays <- which(activity_data_2[ , 4]== "Weekday")
WeekEnds <- which(activity_data_2[ , 4]== "Weekend")

interval_weekdays <- matrix(activity_data_2$steps[WeekDays],,288, byrow = TRUE)

interval_weekends <- matrix(activity_data_2$steps[WeekEnds],,288, byrow = TRUE)

par(mfrow=c(2,1))

avg_WeekDay <- apply(interval_weekdays,2,mean)

avg_WeekEnd <- apply(interval_weekends ,2,mean)

plot(uniqueIntervals, avg_WeekDay,type = "l", main= "weekday",ylab ="Number of steps", xlab="Interval")
plot(uniqueIntervals, avg_WeekEnd ,type = "l", main = "weekend",ylab="Number of steps", xlab="Interval") 

title("Avg. Daily Steps by Weektype" ,outer = TRUE,line=-1, cex=1.5)

```
