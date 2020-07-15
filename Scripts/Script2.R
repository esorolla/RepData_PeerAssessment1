## We calculate the total number of steps taken per day:
total.steps <- with(data, tapply(data$steps, data$date,sum, na.rm = TRUE))

## We create the data frame that we have to plot:
total <- data.frame(steps = total.steps, date = factor(unique(data$date)))

## We calculate the mean and the median of total steps
stmean <- mean(total$steps)
stmedian <- median(total$steps)

## We plot the histogram with the total number of steps per day:
library(ggplot2)
ggplot(total, aes(x=as.Date(date), y=steps)) + geom_bar(stat="identity")+
    scale_x_date(date_labels = "%m/%d")+
    geom_text(x=as.Date("2012-10-26"), y=12000, label="Mean and median", col = "red")+
    geom_text(x=as.Date("2012-10-26"), y=20000, label="year 2012", col = "black", fontsize = 18)+
    geom_hline(yintercept = c(stmean,stmedian),col = "red",linetype = 2)+
    labs(y = "Total steps per day")+
    theme(axis.title.x=element_blank())


