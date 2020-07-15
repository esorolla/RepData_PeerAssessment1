## Loading and preprocessing the data
unzip("activity.zip")
myfile <- read.csv("activity.csv")
head(myfile)

## We ignore and remove the "NA" values (we update the data set name)
data <- myfile[complete.cases(myfile[,1]),]

