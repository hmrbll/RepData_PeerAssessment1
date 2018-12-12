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

# 3. Mean and median number of steps taken each day

selected_data <- activity %>%
        select(date, steps)
agg_data <- selected_data %>%
        group_by(date) %>%
                summarise(mean_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))

# 4. Time series plot of the average number of steps taken

selected_data <- activity %>%
        select(date, steps)
agg_data <- selected_data %>%
        group_by(date) %>%
                summarise(mean_steps = mean(steps, na.rm = TRUE))
ggplot(data = agg_data, aes(x = date, y = mean_steps)) +
        geom_line(color = "blue") +
                labs(x = "Date", y = "Average number of steps", title = "Time series of average number of steps") +
                        theme(plot.title = element_text(hjust = 0.5))

# 5. The 5-minute interval that, on average, contains the maximum number of steps

selected_data <- activity %>%
        filter(steps == max(steps, na.rm = TRUE)) %>%
                select(date, interval)

# 6. Code to describe and show a strategy for imputing missing data

imputed_data <- activity[complete.cases(activity), ]

# 7. Histogram of the total number of steps taken each day after missing values are imputed

#Aggregate data for number of steps taken each day
imputed_data$date <- as.character(imputed_data$date)
selected_data <- imputed_data %>%
        select(date, steps)
agg_data <- selected_data %>%
        group_by(date) %>%
                summarise(total_steps = sum(steps))
#Barplot of number of steps taken each day
ggplot(data = agg_data, aes(x = date, y = total_steps)) +
        geom_bar(stat = "identity", fill = "cyan", color = "black", alpha = 0.5) +
                labs(x = "Date", y = "Steps", title = "Number of steps taken by day w/o NAs") + 
                        theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())

# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends