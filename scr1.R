---
    title: "Reproducible Research: Peer Assessment 1"
output: 
    html_document:
    keep_md: true
---
    
    ## Loading and preprocessing the data
    
    In this section, the script checks to see if the file to be analyzed or the 
zip containing the file is present.  If not, the appropriate actions are taken. 
Then the data for the dates is properly formatted and rows with NAs are removed. 
Finally, the script finds each day that has valid observations and counts the 
instances thereof. 
```{r}
##  As written, I use two packages in addition to the base
##  they are listed below
library(dplyr)
library(lattice)


fileUrl<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

##  First, we look for the file in the working directory
if (!file.exists("activity.csv")){
    
    ##  If not, we see if the zip file containing the data is present    
    if (!file.exists("activity.zip")){
        
        ##  If not, download the file and notify user        
        download.file(fileUrl, destfile="activity.zip", mode = "wb")
        dateDownloaded <- date()
        dlMessage <- paste("Activity monitoring data successfully downloaded:",
                           dateDownloaded)
        print(dlMessage)
    }
    
    ##  Now unzip and notify user  
    unzip ("activity.zip")
    dateUnzipped<- date()
    zipMessage <- paste("Activity monitoring data successfully uzniped:", dateUnzipped)
    print(zipMessage)
}

##  read the file from the csv into a dataframe
activity <- read.csv("activity.csv", header=TRUE,
                     colClasses=c( "numeric", "character", "numeric"), sep =",", na="NA")

##  Convert date to date format
activity[,2]<-as.Date(activity[,2])

##  Remove rows with absent values (for now)
cc_steps<-activity[complete.cases(activity[,1]),]

##  Gather each unique day, then count the number of days
unique_days<-unique(cc_steps[,2])
number_of_days<-length(unique_days)

steps_per_day<-data.frame()
```       

## What is mean total number of steps taken per day?
The script cycles through each valid day inserting a row that contains the 
date, total number of steps observed.  Using this data it prints a histogram of 
the frequency of a range of total steps per day.  Finally, it uses the table to 
calcuate and round to the nearest whole number the mean and median steps per day. 
```{r}
##  For each day, create a sum of the total steps
for (i in 1:number_of_days) {
    day<-unique_days[i]
    steps_per_day[i,1]<-as.character(day)
    steps_in_a_day<-filter(cc_steps, cc_steps$date==day)
    steps_per_day[i,2]<-sum(steps_in_a_day[,1], na.rm=TRUE)
}

##  Print a histogram of steps per day
hist(steps_per_day[,2], xlab= "Steps per day",
     main ="Histogram of the total number of steps taken each day")

##  Calculate mean steps per day, then print
mean_steps<-round(mean(steps_per_day[,2]), digits=0)
mean_message<-paste("Mean steps:", mean_steps)
print(mean_message)

##  Calculate median steps per day, then print
median_steps<-median(steps_per_day[,2])
median_message<-paste("Median steps:", median_steps)
print(median_message)
```


## What is the average daily activity pattern?
To find the average daily activity pattern, the script gathers each interval, 
then loops through each interval summing the number of steps observed, and 
places this in a table along with the interval.  Using the base system, a time 
series plot is used to display this information.  Then the the  5-minute 
interval, that on average across all the days in the dataset, contains the maximum 
number of steps is calculated and displayed. 
```{r}

##  Create data frame to hold steps for each time interval
steps_by_time<-data.frame()

##  Gather all times and county how many there are
unq_times<-unique(activity[,3])
num_of_times<-length(unq_times)

##  For each time interval create a table row that contains time interval,
##  total steps in that interval and for a later calculation complete cases
for (i in 1:num_of_times) {
    time<-unq_times[i]
    steps_by_time[i,1]<-time
    steps_in_an_interval <- filter(cc_steps, cc_steps$interval==time)
    steps_by_time[i,2]<-mean(steps_in_an_interval[,1], na.rm=TRUE)
    steps_by_time[i,3]<-length(steps_in_an_interval[,1])
}
##  label the columns
colnames(steps_by_time)<-c("Interval", "Total.Steps", "Complete.Cases")

##  Plot total steps by time
with(steps_by_time, plot(Interval, Total.Steps, xlab = "Time Interval",
                         ylab="Average Number of Steps", main="Average Steps per 5-minute interval", 
                         type = "l", pch=NA))

##  Find and print max value
max_val<-which.max(steps_by_time[,2])
interval_message<-paste("Highest number of steps are in the", steps_by_time[max_val,1], "interval")
print(interval_message)
```


## Imputing missing values
The script counts and displays takes the rows removed in earlier (due to the 
                                                                  presence of NAs).  It then loops through these rows and inserts the average
value from that time interval (calculated above), these are then incorporated
into the full table.  We then calculate steps per day, as above, using the 
table with imputed data.  A histogram of this data is displayed along with the 
mean (to nearest whole number) and median.
```{r}
##  For this section I chose to average the number of steps for a timer interval
##  to replace the missing values

##  First, find all rows with missing step count
missing_vals <- is.na(activity[,1])

##  Count those rows and print
num_of_missing_vals<-length(missing_vals)
val_message <- paste("Number of missing values:", num_of_missing_vals)
print(val_message)

#   Create data frame of NA's
rows_to_replace <- activity[missing_vals,]

##  For each missing row with a missing value, find the entry for total steps
##  and divide that by complete cases, put that into rows_to_replace data frame
for (i in 1:num_of_missing_vals){
    val_to_match<-rows_to_replace[i,3]
    new_val<-round(steps_by_time[which(steps_by_time$Interval == 
                                           val_to_match), 2])
    rows_to_replace[i,1]<-new_val
    
}

#   create new table using all of old values
filled_table<-activity

#   put replacement rows into table to provide complete dataset
filled_table[missing_vals,]<-rows_to_replace


#   Create new data frame fill it with unique days as above, this time with
#   table with filled NAs
steps_per_day_filled<-data.frame()
unique_days<-unique(filled_table[,2])
number_of_days<-length(unique_days)

##  For each day, create a sum of the total steps 
for (i in 1:number_of_days) {
    day<-unique_days[i]
    steps_per_day_filled[i,1]<-as.character(day)
    steps_in_a_day_filled<-filter(filled_table, filled_table$date==day)
    steps_per_day_filled[i,2]<-sum(steps_in_a_day_filled[,1])
    
}

#   Print a histogram of the new total steps data
hist(steps_per_day_filled[,2], xlab="Steps per day",
     main ="Histogram of the total number of steps with imputed data")

#   Calculate, print mean of filled table
mean_steps_filled<-round(mean(steps_per_day_filled[,2]), digits=0)
filled_mean_message<-paste("Mean steps (filled):", mean_steps_filled)
print(filled_mean_message)

#   Calculate, print median of filled table
median_steps_filled<-median(steps_per_day_filled[,2])
filled_median_message<-paste("Median steps (filled):", median_steps_filled)
print(filled_median_message)
```


## Are there differences in activity patterns between weekdays and weekends?
Now the script determine whether each observation is a weekend or weekday, then 
associates an appropriate factor variable with that row, as well as labeling 
the column. The data is then broken up by this factor and the mean of the steps 
for each time interval is calculated and placed in a data frame.  The tables are 
joined and then using lattice a plot is made comparing Weekends vs. Weekdays.
```{r}
##  For each days (which was calculated and counted above), determine if it was
##  a week day or end, and in a new column, place the appropriate factor variable
for (h in 1:number_of_days)    {
    day_of_week<-weekdays(unique_days[h])
    
    if (day_of_week=="Saturday"||day_of_week=="Sunday"){
        end_or_day <- "Weekend"
    }
    else {
        end_or_day <- "Weekday"
    }
    
    first_obv <- ((h-1)*288)+1
    last_obv <-(h*288)
    
    filled_table[first_obv:last_obv, 4]<-end_or_day
    
}


##  Label new column
colnames(filled_table)[4]<-c("Weekend.or.day")

##  Convert all values in new column to type factor
filled_table<-transform(filled_table, Weekend.or.day = factor(Weekend.or.day))


##  Select all values of type Weekday
days<-filter(filled_table, Weekend.or.day=="Weekday")

##  Now, calculate the mean of the steps for each interval, place that in a 
##  data frame and convert these means to numeric type which was lost when
##  using tapply.  Finally, label the columns
weekday_means<-tapply(days$steps, days$interval, mean)
weekday_means_df<-data.frame(weekday_means)
weekday_means_df[,2]<-as.numeric(rownames(weekday_means_df))
weekday_means_df[,3]<-"Weekday"
colnames(weekday_means_df)<-c("mean_of_wdays", "interval", "Weekend.or.day")


##  Now do the same for type weekend
ends<-filter(filled_table, Weekend.or.day=="Weekend")
weekend_means<-tapply(ends$steps, ends$interval, mean)
weekend_means_df<-data.frame(weekend_means)
weekend_means_df[,2]<-as.numeric(rownames(weekend_means_df))
weekend_means_df[,3]<-"Weekend"
colnames(weekend_means_df)<-c("mean_of_wdays", "interval", "Weekend.or.day")

##  Bind the two tables together
means_df<-rbind(weekday_means_df, weekend_means_df)

##  Plot, using lattice, mean number of steps seperated by weekend and weekday
##  Finally, print
p<-xyplot(mean_of_wdays ~ interval |Weekend.or.day, data=means_df, type="l", layout = c(1,2), 
          ylab="Average Number of Steps", xlab="Time interval")
print(p)
```{r}