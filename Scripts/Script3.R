## We aggregate (or group by) the minute intervals of the data set and we
## evaluate the mean value across the days:
timeSeries <- aggregate(data$steps,  by=list(interval = data$interval), FUN=mean)

## We plot the requested time series:
plot(timeSeries$interval, timeSeries$x, type = "l", xlab = "interval (minutes)", ylab = "average number of steps")

## We calculate the minute interval where the average number of steps across
## the days is maximum:
reqInterval <- timeSeries$interval[which(timeSeries$x == max(timeSeries$x))]
