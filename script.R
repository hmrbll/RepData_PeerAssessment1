# 1. Code for reading in the dataset and/or processing the data

#Unzip data file
unzip("activity.zip")
#Read file
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)

# 2. Histogram of the total number of steps taken each day

#Aggregate data for number of steps taken each day
library(dplyr)
selected_data <- activity %>%
        select(date, steps)
agg_data <- selected_data %>%
        group_by(date) %>%
                summarise(total_steps = sum(steps))
#Barplot of number of steps taken each day
library(ggplot2)
ggplot(data = agg_data, aes(x = date, y = total_steps)) +
        geom_bar(stat = "identity", fill = "cyan", color = "black", alpha = 0.5) +
                labs(x = "Date", y = "Steps", title = "Number of steps taken by day") + 
                        theme(plot.title = element_text(hjust = 0.5))

#  3. Mean and median number of steps taken each day

selected_data <- activity %>%
        select(date, steps)
agg_data <- selected_data %>%
        group_by(date) %>%
                summarise(mean_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))