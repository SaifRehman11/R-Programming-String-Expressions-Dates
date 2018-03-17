#setwd("~/R Programming") Answer to Question 1

#Installing all of the libraries

library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

#Reading in the csv file and printing the dimensions for Question 2

storms <- read.csv(file = "Australia_severe_storms_1975-2015.csv")
print("Answer to Number 2: Dimensions of the data frame untouched")
print(dim(storms))

#Getting rid of the last 6 columns of the storms data frame as well as removing ID
storms <- select(storms, Event.ID, Database, Date.Time, Nearest.town, State, Latitude, Longitude)

#Removing waterspout events from the data frame
storms <- filter(storms, Database != "Waterspout")

#Printing the head and dimensions of the new data frame Answer for Question 3
print("Answer to number 3: dimensions and head after filtering out comments and ID and Waterspout")
print(head(storms))
print((dim(storms)))

#Adding a new column which adds timezone depending on the State of the row
storms$timezone <- c()

#Creating that as a function, but not taking into account Broken Hill yet

timezoneallocation <- function(state) { 
  if(state == "QLD"){timezone <- "Australia/Queensland"}
  else if(state == "VIC") {timezone <- "Australia/Victoria"}
  else if(state == "SA") {timezone <- "Australia/South"}
  else if(state == "WA") {timezone <- "Australia/West"}
  else if(state == "TAS") {timezone <- "Australia/Tasmania"}
  else if(state == "NT") {timezone <- "Australia/North"}
  else if(state == "ACT") {timezone <- "Australia/ACT"}
  else if(state == "NSW")  {timezone <- "Australia/NSW"}
}

#Using a for loop to iterate through the data frame, first checking if the nearest town isn't Broken hill
#if town isn't Broken Hill, then just allocate timezone according to the function using the state as input
#However, if it is Broken hill, then put the timezone as broken hill (Answer to number 4)

for (i in 1:nrow(storms))  {
  if(!str_detect(storms$Nearest.town[i], "(?i)broken.*hill.*")) {storms$timezone[i] <- timezoneallocation(storms$State[i])}
  else(storms$timezone[i] <- "Australia/Broken_Hill")
}

#Adding a column of utctime to the dataframe
storms$utctime <- c()

#Making sure that the Date.Time column in the data frame is now recognized as a time
storms$Date.Time <- dmy_hm(storms$Date.Time)



#Iterating through each row and adding timezone recognition to each row using timezone and the date.time 
#This gives out results in Unix time however for some reason

for (i in 1:nrow(storms)) {
 storms$utctime[i] <- force_tz(storms$Date.Time[i], tzone = storms$timezone[i])
} 


#Adding this code to add another column called utctimecorrect and converting unix time to regular time
origin <- ymd_hm("1970-01-01 00:00", tz="UTC")

storms$utctimecorrect <- c()

storms$utctimecorrect <- origin + storms$utctime[1]

for (i in 1:nrow(storms)) { 
  storms$utctimecorrect[i] <- origin + seconds(storms$utctime[i])
    }

#Since I created two columns, I wanted to get rid of the utctime one that has time in unix time 
#Also printed the head of the data frame as answer to number 5

storms <- select(storms, Event.ID, Database, Date.Time, Nearest.town, State, Latitude, Longitude, timezone, utctimecorrect)

print("Answer to number 5: first few rows with added utctimecorrect column")
print(head(storms))


##Number 6, creating month and year columns and doing a for loop to get month and year for each row
storms$month <- c()
storms$year <- c()

for (i in 1:nrow(storms)) {
  storms$month[i] <- month(storms$utctimecorrect[i])
}

for (i in 1:nrow(storms)) {
  storms$year[i] <- year(storms$utctimecorrect[i])
}

#Printing the results (Answer to number 6), the formatting is just askew because of my long column names
print("Answer to number 6: head of data frame with month and year column added")
print(head(storms))


### Code for question 7


group2 <- group_by(storms, Database, month)


stormsummary <- summarise(group2,
                          Count = n()
)



stormcountplot <- ggplot(stormsummary) + geom_line(aes(as.factor(month), Count, group = Database, col=Database)) + ggtitle("Answer to number 7: Counts of events for each month") + labs(x = "Months", y = "Count")
print(stormcountplot)
