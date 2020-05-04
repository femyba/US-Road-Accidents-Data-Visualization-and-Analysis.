# title: Data visualization and Analysis of US Accidents dataset
# author: Olufemi Babalola

# installing required packages 
install.packages("lubridate")
install.packages("corrplot")
install.packages("data.table")
install.packages("tidyverse")
install.packages("ggmap")


#load tidyverse package into library
library(tidyverse)

# import the US_Accidents dataset into rstudio
US_Accidents <- read.csv("~/US_Accidents.csv")
view(US_Accidents)

## data exploration and cleaning the dataset ####

# check the class of the dataset
class(US_Accidents)

# check the Structure of this dataset
glimpse(US_Accidents)

# check column names in the dataset
colnames(US_Accidents)

# check for missing values in dataframe
any(is.na(US_Accidents))

# listing all columns with missing values in our dataframe
colnames(US_Accidents)[!complete.cases(t(US_Accidents))]

# Next, remove all columns containing at least one na values 
# from the dataset and assigned as Accidentsdata
Accidentsdata <- US_Accidents[, colSums(is.na(US_Accidents)) == 0]

# Convert factors in data.frame columns to characters
i <- sapply(Accidentsdata, is.factor)
Accidentsdata[i] <- lapply(Accidentsdata[i], as.character)

# check the structure of Accidentsdata
glimpse(Accidentsdata)

# next, drop some of the columns of the dataframe
RoadAccidents <- select (Accidentsdata,-c(Description,Street,Side,Zipcode,Country,Airport_Code,Weather_Timestamp,Turning_Loop,Civil_Twilight,Nautical_Twilight,Astronomical_Twilight))
View(RoadAccidents)

# calculate duration for each accident
RoadAccidents$duration_Hours <- with(RoadAccidents, difftime(End_Time,Start_Time, units = "hours"))
RoadAccidents$duration_Mins <- with(RoadAccidents, difftime(End_Time,Start_Time, units = "mins"))

# check structure of the new dataset
glimpse(RoadAccidents)

# create visualizations from the dataset with ggplot

# load the package ggplot2 into library
library(ggplot2)

theme_set(theme_minimal())

# bar plot of Source
ggplot(RoadAccidents) +
  geom_bar(aes(x = Source), fill = 'blue') +
  ggtitle("API Source for USA Accidents Records")

# barplot of severity
ggplot(RoadAccidents) +
  geom_bar(aes(x = Severity), fill = 'green') +
  ggtitle("Severity of Road Accidents in United States")

# barplot of severity by Time zone
ggplot(RoadAccidents, aes(Severity)) +
  geom_bar(aes(fill = Timezone)) +
  ggtitle("Severity of Road Accidents by Timezone")

# a second plot for severity of accident by timezones
ggplot(data = RoadAccidents, mapping = aes(x = Severity, fill = Timezone)) +
  geom_bar(position = "dodge") +
  ggtitle("Severity of Road Accidents by Timezone")

# trend of road accidents by severity
#RoadAccidents$date <- as.Date(RoadAccidents$End_Time)
#RoadAccidents
#ggplot(data = RoadAccidents, mapping = aes(x = date, y = Severity)) +
#  geom_line() +
#  ggtitle("Severity of Road Accidents")

# scatterplot() of RoadAccidents by Time zones
ggplot(data = RoadAccidents) +
       geom_point(mapping = aes(x = Start_Lng, y = Start_Lat, fill=Timezone))
                     
# load ggmap into library
#library(ggmap)

# plot of accidents by States
ggplot(duration_State) +
  geom_bar(aes(x = State), fill = 'red')

# the total number of Road accidents  
df_State <- RoadAccidents %>%
  group_by(State) %>%
  summarize(number=n())
df_State
ggplot(df_State, aes(x=State, y=number)) +
  geom_col(fill="purple") +
  ggtitle("Road Accidents by States")

# sort df_State data 
data <- df_State[order(-df_State$number),]

# check the number of road accidents for top 10 State
# and assigned to new data frame "States"
States <- head(data, 10)

# plot the number of road accidents by each State
ggplot(States, aes(x=State, y=number)) +
  geom_col(fill="brown") +
  ggtitle("Road Accidents for Top 10 States")


# the total number of Road accidents by Cities 
df_City <- RoadAccidents %>%
  group_by(City) %>%
  summarize(number=n())
df_City


# sort df_City data 
data_City <- df_City[order(-df_City$number),]

# check the number of road accidents for the top 10 City
# and assigned to new data frame "Cities"
Cities <- head(data_City, 10)

# plot the number of road accidents by each State
ggplot(Cities, aes(x=City, y=number)) +
  geom_col(fill="Orange") +
  ggtitle("Road Accidents for Top 10 Cities")

# plot of weather conditions
ggplot(RoadAccidents) +
  geom_bar(aes(x = Weather_Condition), fill = 'red')

# the Weather Conditions in Road accidents  
Wc <- RoadAccidents %>%
  group_by(Weather_Condition) %>%
  summarize(number=n())
Wc

# sort Wc data 
data_Wc <- Wc[order(-Wc$number),]

# check the top 7 weather conditions for road accidents
# and assigned to new data frame "weather"
data_Wc2 <- head(data_Wc, 7)

# plot of 7 Top Weather Conditions in road accidents
ggplot(data_Wc2, aes(x=Weather_Condition, y=number)) +
  geom_col(fill=rainbow(7)) +
  ggtitle("7 Top Weather Conditions in Road Accidents")