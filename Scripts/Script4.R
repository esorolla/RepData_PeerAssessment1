## We calculate the total number of missing values in the original
## dataset:

missing <- is.na(myfile[,1])

missingTotal <- sum(missing)

## We fill in the missing values with 0, so we avoid to add artificial values
## to the total number of steps per day (even though the average values will vary).
newfile <- myfile # we clone the original data
newfile[missing,1] <- 0 # we replace "NA" by 0.

## We calculate the total number of steps taken per day:
new.total.steps <- with(newfile, tapply(newfile$steps, newfile$date,sum))

## We create the data frame that we have to plot:
new.total <- data.frame(steps = new.total.steps, date = factor(unique(newfile$date)))

## We calculate the mean and the median of total steps
newstmean <- mean(new.total$steps)
newstmedian <- median(new.total$steps)

## We plot the histogram of the new dataset:
library(ggplot2)
ggplot(new.total, aes(x=as.Date(date), y=steps)) + geom_bar(stat="identity")+
    scale_x_date(date_labels = "%m/%d")+
    geom_text(x=as.Date("2012-10-25"), y=10000, label="Mean", col = "red")+
    geom_text(x=as.Date("2012-10-25"), y=11000, label="Median", col = "blue")+
    geom_text(x=as.Date("2012-10-26"),  y=20000, label="year 2012", col = "black", fontsize = 18)+
    geom_hline(yintercept = newstmean,col = "red",linetype = 2)+
    geom_hline(yintercept = newstmedian,col = "blue",linetype = 2)+
    labs(y = "Total steps per day")+
    theme(axis.title.x=element_blank())
