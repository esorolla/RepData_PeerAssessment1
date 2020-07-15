## We load the lubridate library to deal with dates:
library(lubridate)

## We save the local value of the system
Sys.getlocale("LC_TIME")

## We use the "coursera" common language system:
Sys.setlocale("LC_TIME", "English")

## We create an intermediate variable in order to lighten the for loop:
newdate <- as.Date(newfile$date)

newfactor <- factor(c("weekday","weekend")) # we create the newfactor variable

## We assign the value weekend or week to each row of "newfactor":
for (i in 1:length(newfile$date)){
    if (weekdays(newdate[i]) == "Saturday" | weekdays(newdate[i]) == "Sunday"){
        newfactor[i] <- "weekend"
    }
    
    else{
        newfactor[i] <- "weekday"
    }

}

## We bind "newfactor" as a new column of the data frame "newfile":
newfile <- cbind(newfile,newfactor)



## We aggregate the data (group the data) by the minute intervals of the data set
## together with the newfactor accounting for the day of the week and we evaluate
## evaluate the mean value across the days (the remaining variable of "newfile"):
output <- aggregate(newfile$steps,  by=list(interval = newfile$interval, newfactor = newfile$newfactor), FUN=mean)

library(lattice)
## We plot the time series for the weekday and the weekend case separately:
xyplot(x ~ interval | newfactor, data = output, type = "l", ylab = "Number of steps")
