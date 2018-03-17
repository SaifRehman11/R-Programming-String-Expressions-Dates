library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

storm <- read.csv("Australia_severe_storms_1975-2015.csv", stringsAsFactors=FALSE)

#Combining all of the Comments categories into an All.comments category and separating each combination with a space

storm$All.comments <- paste(storm$Comments, storm$X, storm$X.1, storm$X.2, storm$X.3, storm$X.4, sep=' ')

#Selecting only the columns in Question 1 : Event.ID, Database, Date.Time, State, All.comments
storm <- select(storm, Event.ID, Database, Date.Time, State, All.comments)

#Changing variable types to match
storm$Date.Time <-dmy_hm(storm$Date.Time)

#Answer to number 1
print(sapply(storm, class))

##Question 2

#Creating an expression to find out any form of flash flood, includes Flash flood, uppercase and lower case, and flashflood(ing)

expr <- "[Ff]lash.*[Ff]lood+"

#Implementing the expression to find out the rows which have this expression
storm$detect <- str_detect(storm$All.comments, expr)
#Setting up a year column
storm$year <- year(storm$Date.Time)
#Summing up the totals of the detect column by year and assigning it to count
count <- tapply(storm$detect, storm$year, sum)

#Converting count to a data frame so it makes it easier to graph by binding year into it
year <- c(1975:2015)
flashfloodcount <- cbind(year, count)
flashfloodcount <- as.data.frame(flashfloodcount)
#Finally plotting the results as a line with year as the x variable and count as the y variable
flashfloodplot <- ggplot(flashfloodcount) + geom_line(aes(x=year, y= count)) + labs(x = "Year", y="Count of Flash Floods") + ggtitle("Answer to number 2")
print(flashfloodplot)

##Question3 

#Expression to match all variants of Knots, Knot, kmh, km/h, kts, kt
expr0 <- "\\w+.[Kk]nots?|\\w+.[Kk]m/h|\\w+.[Kk]mh|\\w+.[Kk]ts|\\w+.[Kk]t"

#Expression to match the word preceding kmh or km/h
expr4 <- "\\w+.[Kk]mh|\\w+.[Kk]m.h"

#Expression to match all digits (to get rid of the kmh, or knots after str_extract)
expr5 <- "\\d+"


#Expression to match any variety of knots and kts or kt with word preceding
expr7 <- "\\w+.[Kk]nots?|\\w+.[Kk]ts?"

#Finding all of the kmh or km/h winds with the preceding word
storm$kmh <- str_extract(storm$All.comments, expr4)
#Using those extracted kmh speeds to extract only the digits
storm$kmh2 <- str_extract(storm$kmh, expr5)


#Finding all of the instances which measures Knots or km/h
storm$stormwind <- str_detect(storm$All.comments, expr0)
#Creating an empty variable to store in
stormwind <- c()
#Storing that empty variable where it finds any measure of Knots of km/h or any variation of them
stormwind <- storm[which(storm$stormwind == "TRUE"),]
#Converting that to number to do calculations on it
stormwind$kmh2 <- as.numeric(stormwind$kmh2)
#Converting kmh to knots by dividing it by 1.852
stormwind$kmhtoknot <- stormwind$kmh2/1.852
#Creating a new column with the rounded knots value derived from the kmh conversion
stormwind$fin <- round(stormwind$kmh2/1.852, digit = 0)

#Extracting all of the knots variations and the preceding words
stormwind$knot <- str_extract(stormwind$All.comments, expr7)
#Extracting the digits only from the knots variations
stormwind$knotfinal <- str_extract(stormwind$knot, expr5)
#If there are NA's in the knotfinal column (which only contained knots)
#Then this one will fill in those NA's with the rounded final kmh converted into knots
#So the knots extraction is prioritized and only the NA's are filled with the rounded knots from kmh
stormwind$knotfinal[is.na(stormwind$knotfinal)] <- stormwind$fin[is.na(stormwind$knotfinal)]

#Since I created a bunch of new columns and it was getting confusing, selecing only the relevant columns with the final knots
stormfinal <- select(stormwind, Event.ID, Database, State, All.comments, knotfinal)
#Creating new data set that is grouped by state (even though not necessary for final result, could have used the previous data frame)
stormgroup <- group_by(stormfinal, State)
#Converting the final knot values as numeric so we can plot
stormgroup$knotfinal <- as.numeric(stormgroup$knotfinal)
#Getting rid of one value which had no state
stormgroup <- stormgroup[!stormgroup$State == "",]
#Getting rid of all the NA values which had false positives
stormgroupfinal <- na.omit(stormgroup)

#Answer to number 3, plotting the boxplot with the wind speeds (in knots) separated by State
windspeedplot <- ggplot(stormgroupfinal) + geom_boxplot(aes(x=stormgroupfinal$State, y=stormgroupfinal$knotfinal)) + coord_cartesian(ylim = c(0, 110)) + ggtitle("Answer to number 3") + labs(x="State", y = "Wind Speeds (in Knots)")
print(windspeedplot)
