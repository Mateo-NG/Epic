---
title: "Bellabeat Casestudy"
author: "Matthew Solomon"
date: "2023-07-10"
output:
  pdf_document: default
  html_document: default
---

## Case Study 2: How Can a Wellness Technology Company Play It Smart?

### About the Company:
Bellabeat, founded in 2013 by Urška Sršen and Sando Mur, is a high-tech company specializing in health-focused smart products for women. Combining technology and artistry, Bellabeat empowers women by collecting data on activity, sleep, stress, and reproductive health, providing valuable insights into their well-being. The company has experienced rapid growth since its inception and has established itself as a tech-driven wellness brand for women. Bellabeat products are available through various channels, and the company heavily invests in digital marketing to reach its target audience.

### Products:

- Bellabeat App: A health tracking app that provides users with data related to their activity, sleep, stress, menstrual cycle, and mindfulness habits. It connects to Bellabeat's line of smart wellness products.
- Leaf: Bellabeat's classic wellness tracker that can be worn as a bracelet, necklace, or clip. It tracks activity, sleep, and stress, connecting to the Bellabeat app.
- Time: A wellness watch that combines the appearance of a classic timepiece with smart technology. It tracks user activity, sleep, and stress, and also connects to the Bellabeat app.
- Spring: A smart water bottle that tracks daily water intake to ensure proper hydration. It connects to the Bellabeat app to monitor hydration levels.
- Bellabeat Membership: A subscription-based program that provides users with personalized guidance on nutrition, activity, sleep, health, beauty, and mindfulness.

## Data Source:
FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)

## Loading necessary packages
```{r installing packages}
library(tidyverse)
library(readr)
library(janitor)
library(skimr)
library(ggplot2)
library(dplyr)
library(hms)
```

#Importing data
```{r Importing data}
setwd("/cloud/project/Fitabase Data 4.12.16-5.12.16")
calories <- read.csv("dailyCalories_merged.csv")
activity <- read.csv("dailyActivity_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")
steps <- read.csv("dailySteps_merged.csv")
heart_rate <- read.csv("heartrate_seconds_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
intensities <- read_csv("dailyIntensities_merged.csv")
```

#Checking unique values in the dataset
```{r Checking unique values in the dataset}
n_unique(activity$Id)
n_unique(sleep$Id)
n_unique(steps$Id)
n_unique(calories$Id)
n_unique(weight$Id)
n_unique(heart_rate$Id)
n_unique(intensities$Id)
```

"Our dataset is unique due to the inclusion of 33 individuals and multiple health-related variables. We have data on daily activity, steps, calories burned, sleep duration, weight, and heart rate. This diversity allows us to capture a broad range of behaviors and patterns.
Among the 33 individuals, we have sleep data for 24 individuals, weight data for 8 individuals, and heart rate data for 14 individuals. Although data collection durations and frequencies may vary, this dataset offers valuable insights into the relationship between different variables and individuals' health and wellness profiles.
By analyzing the correlations between daily activities, steps, calorie burn, sleep duration, and heart rate, we can uncover meaningful insights to guide future marketing strategies and enhance the development of health-focused products for women.
In summary, our unique dataset encompasses 33 individuals with multiple health-related variables. Through our analysis, we aim to extract valuable insights that contribute to the growth of Bellabeat in the global smart device market."

# Potential Analyses

1. Considering these unique values, you can perform various analyses and explore relationships between different variables for these individuals. Here are some potential analyses to conduct:

2. Steps and Calorie Burn: Investigate the relationship between daily steps (from the steps dataset) and calorie burn (from the calories dataset) for the individuals who have data for both variables.

3. Activities and Intensity: Explore the relationship between daily activities (from the activity dataset) and intensity levels (from the Intensities dataset) for the individuals who have data for both variables.

4. Total Minutes Asleep vs Total Time In Bed

5. Total Distance vs Calories

6. Calories vs. Total Steps

# Cleaning and Formatting Calories Data
```{r Cleaning and Formatting Calories Data}
 if ("ActivityDay" %in% names(calories)) {
  calories <- calories %>%
    rename(activity_date = ActivityDay) %>%
    mutate(activity_date = as.Date(activity_date, format = "%m/%d/%Y"))
}

clean_calories_data <- function(calories) {
  calories <- calories %>%
    distinct() %>%
    drop_na()
  
  return(calories)
}
clean_names(calories)
# Clean the calories dataset
cleaned_calories <- clean_calories_data(calories)
head(cleaned_calories)
```
# Descriptive Statistics for the Calories Dataset
```{r Descriptive Statistics for the Calories Dataset}
summary_stats <- cleaned_calories %>%
  summarize(
    max_calories = max(Calories),
    min_calories = min(Calories),
    mean_calories = mean(Calories)
  )

head(summary_stats)
```

#Cleaning and Formatting the Steps Dataset
```{r Cleaning and Formatting the Steps Dataset}
head(steps)
```

##Cleaning the Steps Data
```{r Cleaning the Steps Data}
# Rename and format date column
if ("ActivityDay" %in% names(steps)) {
  steps <- steps %>%
    rename(activity_date = ActivityDay) %>%
    mutate(activity_date = as.Date(activity_date, format = "%m/%d/%Y"))
}

# Clean and format data function
clean_steps_data <- function(steps) {
  steps <- steps %>%
    distinct() %>%
    drop_na()
  
  return(steps)
}

# Clean the steps dataset
cleaned_steps <- clean_steps_data(steps)

head(cleaned_steps)
```

#Descriptive Statistics for the Steps Dataset
```{r Descriptive Statistics for the Steps Dataset}
steps_summary_stats <- cleaned_steps %>%
  summarize(
    max_StepTotal = max(StepTotal),
    min_StepTotal = min(StepTotal),
    mean_StepTotal = mean(StepTotal)
  )

head(steps_summary_stats)
```

#Cleaning the Activities Dataset
```{r Cleaning the Activities Dataset, include=FALSE}
clean_activity_data <- function(activity) {
  activity <- activity %>%
    distinct() %>%
    drop_na()
  
  # Convert variable types
  activity$SedentaryMinutes <- as.numeric(activity$SedentaryMinutes)
  activity$LightlyActiveMinutes <- as.numeric(activity$LightlyActiveMinutes)
  activity$VeryActiveMinutes <- as.numeric(activity$VeryActiveMinutes)
  activity$FairlyActiveMinutes <- as.numeric(activity$FairlyActiveMinutes)
  
  # Rename column
  if ("ActivityDate" %in% names(activity)) {
    activity <- activity %>%
      rename(activity_date = ActivityDate) %>%
      mutate(activity_date = as.Date(activity_date, format = "%m/%d/%Y"))
  }
  
  return(activity)
}

# Clean and format the steps dataset
cleaned_activity <- clean_activity_data(activity)

clean_names(cleaned_activity)
glimpse(cleaned_activity)
```

#Descriptive Statistics for the Activities Dataset
```{r Descriptive Statistics for the Activities Dataset}
activity_summary_stats <- cleaned_activity %>%
  summarize(
    max_TotalSteps = max(TotalSteps),
    min_TotalSteps = min(TotalSteps),
    avg_TotalSteps = mean(TotalSteps),
    max_TotalDistance = max(TotalDistance),
    min_TotalDistance = min(TotalDistance),
    avg_TotalDistance = mean(TotalDistance),
    max_VeryActiveDistance = max(VeryActiveDistance),
    min_VeryActiveDistance = min(VeryActiveDistance),
    avg_VeryActiveDistance = mean(VeryActiveDistance),
    max_SedentaryMinutes = max(SedentaryMinutes),
    min_SedentaryMinutes = min(SedentaryMinutes),
    avg_SedentaryMinutes = mean(SedentaryMinutes),
    max_SedentaryActiveDistance = max(SedentaryActiveDistance),
    min_SedentaryActiveDistance = min(SedentaryActiveDistance),
    mean_SedentaryActiveDistance = mean(SedentaryActiveDistance),
    max_ModeratelyActiveDistance = max(ModeratelyActiveDistance),
    min_ModeratelyActiveDistance = min(ModeratelyActiveDistance),
    mean_ModeratelyActiveDistance = mean(ModeratelyActiveDistance),
    max_LightlyActiveMinutes = max(LightlyActiveMinutes),
    min_LightlyActiveMinutes = min(LightlyActiveMinutes),
    mean_LightlyActiveMinutes = mean(LightlyActiveMinutes),
    max_Calories = max(Calories),
    min_Calories = min(Calories),
    mean_Calories = mean(Calories),
    max_FairlyActiveMinutes = max(FairlyActiveMinutes),
    min_FairlyActiveMinutes = min(FairlyActiveMinutes),
    mean_FairlyActiveMinutes = mean(FairlyActiveMinutes)
  )
print(activity_summary_stats)
```

#Cleaning the Intensities Dataset
```{r Cleaning the Intensities Dataset}
str(intensities)
# Rename and format date column in 
if ("ActivityDay" %in% names(intensities)) {
  intensities <- intensities %>%
    rename(activity_date = ActivityDay) %>%
    mutate(activity_date = as.Date(activity_date, format = "%m/%d/%Y"))
}

# Clean and format data function
clean_intensities_data <- function(steps) {
  intensities <- intensities %>%
    distinct() %>%
    drop_na()
  
  return(intensities)
}

# Clean the steps dataset
cleaned_intensities <- clean_intensities_data(intensities)

head(cleaned_intensities)
```

#Descriptive Statistics for the Intensities Dataset
```{r Descriptive Statistics for the Intensities Dataset}
intensities_summary_stats <- cleaned_intensities %>%
  summarize(
    max_VeryActiveDistance = max(VeryActiveDistance),
    min_VeryActiveDistance = min(VeryActiveDistance),
    avg_VeryActiveDistance = mean(VeryActiveDistance),
    max_SedentaryMinutes = max(SedentaryMinutes),
    min_SedentaryMinutes = min(SedentaryMinutes),
    avg_SedentaryMinutes = mean(SedentaryMinutes),
    max_SedentaryActiveDistance = max(SedentaryActiveDistance),
    min_SedentaryActiveDistance = min(SedentaryActiveDistance),
    avg_SedentaryActiveDistance = mean(SedentaryActiveDistance),
    max_ModeratelyActiveDistance = max(ModeratelyActiveDistance),
    min_ModeratelyActiveDistance = min(ModeratelyActiveDistance),
    mean_ModeratelyActiveDistance = mean(ModeratelyActiveDistance),
    avg_LightlyActiveMinutes = max(LightlyActiveMinutes),
    min_LightlyActiveMinutes = min(LightlyActiveMinutes),
    mean_LightlyActiveMinutes = mean(LightlyActiveMinutes),
    avg_FairlyActiveMinutes = max(FairlyActiveMinutes),
    min_FairlyActiveMinutes = min(FairlyActiveMinutes),
    avg_FairlyActiveMinutes = mean(FairlyActiveMinutes)
  )
head(intensities_summary_stats)

```

#Merging Necessary Data for Analysis
```{r Merging Necessary Data for Analysis}
merged_calories_activity <- inner_join(cleaned_activity, cleaned_calories, by = "Id")
head(merged_calories_activity)
```

```{r merged_activity_intensities }
merged_activity_intensities <- inner_join(cleaned_activity, cleaned_intensities, by = "Id", "activity_date")
head(merged_activity_intensities)
```
#Relationship between Total Distance vs Calories
```{r Relationship between Total Distance vs Calories}
correlation <- cor(activity$TotalDistance, activity$Calories)
ggplot(data = activity) +
  geom_point(mapping = aes(x = TotalDistance, y = Calories, color = Calories)) +
  labs(title = "Relationship between Total Distance vs Calories", caption = "Mobius") +
  xlab("Total Distance") +
  ylab("Calories") +
  annotate("text", x = Inf, y = Inf, label = paste0("Correlation: ", round(correlation, 2)), 
           hjust = 1 , vjust = 2)
```

#Creating a scatter plot Calories vs Steps
```{r Create scatter plot Calories vs Steps}
correlation <- cor(cleaned_activity$TotalSteps, cleaned_activity$Calories)

ggplot(data = cleaned_activity) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories, color = Calories)) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_smooth(mapping = aes(x = TotalSteps, y = Calories), color = "orange") +
  labs(title = "Relationship: Calories vs. Total Steps", x = "Total Steps", y = "Calories") +
  annotate("text", x = Inf, y = Inf, label = paste0("Correlation: ", round(correlation, 2)), 
           hjust = 1, vjust = 1)

```
A correlation coefficient of 0.59 indicates that there is a tendency for higher values of "TotalSteps" to be associated with higher values of "Calories". The positive sign of the correlation coefficient suggests that as the number of total steps increases, there is a tendency for the calorie count to also increase.


#Relationship between Total Minutes Asleep vs Total Time In Bed
```{r Total Minutes Asleep vs Total Time In Bed}
# Calculate correlation coefficient
correlation <- cor(sleep$TotalMinutesAsleep, sleep$TotalTimeInBed)

# Create point plot with a different color
ggplot(data = sleep) +
  geom_point(mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed), color = "brown") +
  labs(title = "Relationship between Total Minutes Asleep vs Total Time In Bed", caption = "Mobius") +
  annotate("text", x = Inf, y = Inf, label = paste0("Correlation: ", round(correlation, 2)), 
           hjust = 1, vjust = 1)


```
A correlation coefficient value of 0.93 indicates a very strong positive relationship between "TotalMinutesAsleep" and "TotalTimeInBed" in the sleep dataset.
In the context of the sleep data, a correlation coefficient of 0.93 suggests that there is a high degree of association between the total minutes asleep and the total time spent in bed. It indicates that as the total minutes asleep increase, there is a strong tendency for the total time spent in bed to also increase, and vice versa.

#Visualization of the Relationship between Activity and Intensity
```{r Relationship between Activity and Intensity}
correlation <- cor(merged_activity_intensities$TotalSteps, merged_activity_intensities$VeryActiveMinutes.x)

# Creating a scatter plot
ggplot(merged_activity_intensities, aes(x = TotalSteps, y = VeryActiveMinutes.x)) +
  geom_point() +
  labs(x = "Total Steps", y = "Very Active Minutes") +
  ggtitle("Total Steps vs Very Active Minutes") +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = Inf, y = Inf, label = paste0("Correlation: ", round(correlation, 2)), 
           hjust = 1, vjust = 1)

```
Based on the correlation coefficient of 0.67, there is a moderately strong positive relationship between "TotalSteps" and "VeryActiveMinutes.x" in the merged_activity_intensities data.
A correlation coefficient of 0.67 indicates that there is a tendency for higher values of "TotalSteps" to be associated with higher values of "VeryActiveMinutes.x". The positive sign of the correlation coefficient suggests that as the number of total steps increases, there is a tendency for the number of very active minutes to also increase.

#Findings
Based on the analysis of the data and the insights gained, we can explain how consumers are using their smart devices and how these insights can guide the marketing strategy for Bellabeat. Here are the key findings and recommendations:

-Steps and Calorie Burn: There is a positive relationship between daily steps and calorie burn. Consumers who take more steps tend to burn more calories. This insight suggests that promoting increased physical activity through step tracking can be an effective marketing strategy. Bellabeat can emphasize the calorie-burning benefits of their products, highlighting how they help users achieve their fitness goals.

-Activities and Intensity: There is a positive relationship between daily activities and intensity levels. Consumers who engage in more activities also tend to have higher intensity levels. Bellabeat can leverage this insight by promoting their products' ability to track and analyze different activity levels. They can emphasize the importance of varying activity intensity for a well-rounded fitness routine.

-Total Minutes Asleep vs Total Time In Bed: The strong positive relationship between total minutes asleep and total time in bed indicates that individuals who spend more time in bed have the opportunity to accumulate more sleep. Bellabeat can emphasize the importance of adequate sleep and position their products as tools for improving sleep quality and duration.

-Total Distance vs Calories: There is a moderate positive relationship between total distance and calories burned. Consumers who cover more distance tend to burn more calories. Bellabeat can promote their products' ability to track distance and highlight the calorie-burning benefits of incorporating more physical activity into daily routines.

-Calories vs Total Steps: There is a positive relationship between calories burned and total steps. Consumers who take more steps tend to burn more calories. Bellabeat can emphasize the calorie-burning benefits of increased step count and position their products as tools for tracking and achieving fitness goals.

-Relationship between Activity and Intensity: There is a moderately strong positive relationship between total steps and very active minutes. Consumers who take more steps tend to have more very active minutes. Bellabeat can highlight the relationship between steps and activity intensity, positioning their products as tools for tracking and promoting an active lifestyle.

Based on these findings, Bellabeat can tailor their marketing campaigns to emphasize the benefits of their products in promoting physical activity, tracking different activity levels, improving sleep, and helping users achieve their fitness goals. By leveraging these insights, Bellabeat can better meet the needs and preferences of their target audience, ultimately driving growth and success in the wellness technology market.

#Conclusion
The analysis of the unique dataset for Bellabeat provides valuable insights into the usage patterns and relationships between health-related variables among consumers. These insights play a crucial role in shaping Bellabeat's marketing strategy and positioning in the global smart device market. By leveraging these insights, Bellabeat can:

1. Enhance Product Development: The analysis uncovers correlations between different variables such as activity, steps, calorie burn, sleep duration, and heart rate. These correlations can inform the development of new features and functionalities in Bellabeat's products. For example, integrating sleep tracking and analysis features to improve sleep quality or incorporating personalized activity recommendations based on steps and calorie burn.

2. Refine Messaging and Targeting: The findings provide a deeper understanding of how consumers use their smart devices. Bellabeat can align its messaging and marketing efforts to highlight the specific benefits and features that resonate most with its target audience. For instance, emphasizing the relationship between activity and intensity to attract fitness enthusiasts or promoting sleep-related features to target individuals seeking better sleep patterns.

3. Drive User Engagement: With insights into the correlations between different health-related variables, Bellabeat can create personalized experiences for its users. By leveraging the data collected from their smart devices, Bellabeat can offer tailored recommendations, challenges, and insights to keep users engaged and motivated in their health and wellness journey.

4. Expand Digital Marketing Reach: The analysis reveals key insights into consumer behavior and preferences. Bellabeat can leverage this information to refine its digital marketing strategies and campaigns, ensuring targeted and effective communication with its audience. By identifying specific segments within their user base, Bellabeat can optimize its advertising channels, social media campaigns, and influencer partnerships to reach the right audience at the right time.

5. Foster Customer Satisfaction and Loyalty: By aligning their products and marketing efforts with the identified correlations, Bellabeat can deliver value-added experiences to its customers. This, in turn, can enhance customer satisfaction and loyalty. By continuously analyzing user data and incorporating feedback, Bellabeat can further refine its products and services to meet the evolving needs of its customer base.

In conclusion, the analysis of the unique dataset for Bellabeat provides valuable insights that can shape the company's product development, messaging, targeting, digital marketing, and customer engagement strategies. By leveraging these insights effectively, Bellabeat can solidify its position as a leading wellness technology company and drive growth in the competitive global smart device market.