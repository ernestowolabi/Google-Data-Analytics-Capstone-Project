#Loading up necessary libraries
library(tidyverse)
library(lubridate)


#Loading data frames using the read_csv function
daily_activity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleep_day <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
heartrate_seconds <- read_csv("Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
weight <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")


#Exploring the table structure and variables
head(daily_activity)
head(sleep_day)
head(weight)
head(heartrate_seconds)
colnames(daily_activity)
colnames(sleep_day)
colnames(weight)
colnames(heartrate_seconds)
str(daily_activity)
str(sleep_day)
str(weight)
str(heartrate_seconds)


#Checking count of distinct users by their id
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight$Id)
n_distinct(heartrate_seconds$Id)


#check for duplicates
sum(duplicated(daily_activity))
sum(duplicated(sleep_day))
sum(duplicated(weight))
sum(duplicated(heartrate_seconds))


#removing duplicates
sleep_day_2 <- sleep_day %>% 
  distinct() 
sum(duplicated(sleep_day_2))


#summarise data to get insights
daily_activity %>% 
  select(TotalSteps,TotalDistance,SedentaryMinutes,Calories) %>%
  summary()
daily_activity %>% 
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>% 
  summary()
sleep_day_2 %>% 
  select(TotalSleepRecords,TotalMinutesAsleep,TotalTimeInBed) %>% 
  summary()
heartrate_seconds %>% 
  select(Value) %>% 
  summary()
weight %>% 
  select(WeightKg,BMI) %>% 
  summary()


#merging data frames
#first of all, trim whitespace from date and rename dates
daily_activity2 <- daily_activity %>% 
  mutate(ActivityDate=trimws(ActivityDate)) %>% 
  rename(Date=ActivityDate)
sleep_day_3 <- sleep_day_2 %>% 
  mutate(SleepDay=trimws(SleepDay)) %>% 
  rename(Date=SleepDay)
heartrate_seconds2 <- heartrate_seconds %>%
  mutate(Time=trimws(Time)) %>% 
  rename(Date=Time)
weight2 <- weight %>% 
  mutate(Date=trimws(Date))


#then change date format of daily_activity_2
daily_activity_cleaned <- daily_activity2 %>% 
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
str(daily_activity_cleaned)
View(daily_activity_cleaned)


#now to do the same to sleep_day_3
sleep_day_cleaned <- sleep_day_3 %>% 
  mutate(Date=as.Date(Date,format="%m/%d/%Y %I:%M:%S %p"))
View(sleep_day_cleaned)
str(sleep_day_cleaned)

#then that of weight_2
weight_cleaned <- weight2 %>% 
  mutate(Date=as.Date(Date,format="%m/%d/%Y %I:%M:%S %p"))
View(weight_cleaned)
str(weight_cleaned)


#merging daily activity and sleep data by id and date
daily_merged_data <- merge(sleep_day_cleaned, daily_activity_cleaned, by=c('Id','Date'))
head(daily_merged_data)
View(daily_merged_data)


#then merging daily activity and weight data
daily_activity_weight <- merge(weight_cleaned, daily_activity_cleaned, by=c('Id','Date'))
head(daily_activity_weight)
View(daily_activity_weight)


#visualize the relationship between steps taken and calories expended
ggplot(data=daily_merged_data, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs Calories expended")


#visualizing the relationship between steps taken and sleep duration
ggplot(data=daily_merged_data, aes(x=TotalSteps, y=TotalMinutesAsleep)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs TotalMinutesAsleep")


#visualizing the relationship between total distance walked and calories burned
ggplot(daily_merged_data, aes(x=TotalDistance,y= Calories))+
  geom_point() +
  geom_smooth(col='pink') +
  labs(title="Total Distance vs Calories expended")


#To get the daily usage of the smart device, i'll use the group by and summarise function 
#to count distinct users and how often they used the device. 
#Grouping into "low use", "moderate use" and "high use".
daily_use <- daily_merged_data %>%
  group_by(Id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))
View(daily_use)
n_distinct(daily_merged_data$Id)


#visualizing smart device usage data
ggplot(data=daily_use)+
  geom_bar(mapping=aes(x=usage, fill=usage))


#visualizing the relationship between BMI and steps taken
ggplot(data = daily_activity_weight, aes(x = TotalSteps, y = BMI)) +
  geom_point() +
  geom_smooth(col = 'pink')+
  labs(title="Total steps vs BMI")


#visualizing the relationship between BMI and calories burned
ggplot(data = daily_activity_weight, aes(x = Calories, y = BMI)) +
  geom_point() +
  geom_smooth(col = 'pink')+
  labs(title="Calories expended vs BMI")


#to visualize most active day of the week,
#I'll first extract the days of the week from the date then proceed to analyze.
Day <- daily_activity_cleaned %>%
  mutate(DayOfWeek = weekdays(Date))
View(Day)

# Convert 'DayOfWeek' column into a factor with custom levels specifying the order of days of the week
Day$DayOfWeek <- factor(Day$DayOfWeek, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Sort the data frame by the 'DayOfWeek' column
Day <- Day[order(Day$DayOfWeek), ]

# View the sorted data frame
View(Day)

#here comes the visualization for most active day of the week.
ggplot(data=Day,)+
  geom_bar(mapping=aes(x=DayOfWeek, fill=DayOfWeek))+
  labs(title="Activity Grouped by Weekday")



install.packages('sass')

