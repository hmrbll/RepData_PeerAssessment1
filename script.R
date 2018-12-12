#Unzip data file
unzip("activity.zip")
#Read file
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)