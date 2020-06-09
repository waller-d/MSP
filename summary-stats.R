##############################
####################  waller-d
#
#
#  Data summary and descriptive
#  statistics
#
#
####################
##############################

library(tidyverse)
library(psych)

# Function to remove variables that have less than 50 categories
less_than_50 <- function(x_list){
  n <- length(x_list[[1]])
  if(n < 50){
    print(x_list)
  }
}

# Function to compute standard deviation with NAs removed
standard_dev <- function(x){
  sd(x, na.rm = TRUE)
}


##### Student data #####

# Create a data frame containing all categorical data
# Create a list that contains lists of possible values for each categorical variable
cat_data_students <- students %>% select_if(is.character)
categories_students <- sapply(cat_data_students, unique)

# Send list output to a text file for lists that contain less than 50 categories
# This creates a file with all unique categories for each variable
sink("cat_data_students.txt")
for (i in 1:length(categories_students)) {
  less_than_50(categories_students[i])
}
sink()

# Create data frames containing all numberical and all logical data
log_data_students <- students %>% select_if(is.logical)
num_data_students <- students %>% select_if(is.numeric)

# Send summary statistics to a text file for numerical and logical data
sink("log_data_students_stat.txt")
summary(log_data_students)
sink()
sink("num_data_students_stat.txt")
summary(num_data_students)
sink()

# Calculate distributions for logical data
# Calculate standard deviations for logical data
# Combine data for descriptive statistics of logical data
p_students <- colMeans(log_data_students, na.rm = TRUE)
sd_students_log <- sqrt(p_students*(1-p_students))
students_stats_log <- cbind(p_students, sd_students_log)

# Calculate standard deviation for numerical data
avg_students_num <- colMeans(num_data_students, na.rm = TRUE)
sd_students_num <- sapply(num_data_students, standard_dev)
students_stats_num <- cbind(avg_students_num, sd_students_num)

# Combine the two data frames into a single data frame
# Calculate the correlation matrix for these variables
students_z <- cbind(log_data_students,num_data_students)
students_r <- lowerCor(students_z)

# Write the correlation matrix to a CSV file
write_csv(as.data.frame(students_r), "correlation-matrix-students.csv")


##### Semester data #####

# Create a data frame containing all categorical data
# Create a list that contains lists of possible values for each categorical variable
cat_data_semesters <- semesters %>% select_if(is.character)
categories_semesters <- sapply(cat_data_semesters, unique)

# Send list output to a text file for lists that contain less than 50 categories
sink("cat_data_semesters.txt")
for (i in 1:length(categories_semesters)) {
  less_than_50(categories_semesters[i])
}
sink()

# Create data frames containing all numberical and all logical data
log_data_semesters <- semesters %>% select_if(is.logical)
num_data_semesters <- semesters %>% select_if(is.numeric)

# Send summary statistics to a text file for numerical and logical data
sink("log_data_semesters_stat.txt")
summary(log_data_semesters)
sink()
sink("num_data_semesters_stat.txt")
summary(num_data_semesters)
sink()

# Calculate distributions for logical data
# Calculate standard deviations for logical data
# Combine data for descriptive statistics of logical data
p_semesters <- colMeans(log_data_semesters, na.rm = TRUE)
sd_semesters_log <- sqrt(p_semesters*(1-p_semesters))
semesters_stats_log <- cbind(p_semesters, sd_semesters_log)

# Calculate standard deviation for numerical data
avg_semesters_num <- colMeans(num_data_semesters, na.rm = TRUE)
sd_semesters_num <- sapply(num_data_semesters, standard_dev)
semesters_stats_num <- cbind(avg_semesters_num, sd_semesters_num)

# Combine the two data frames into a single data frame
# Calculate the correlation matrix for these variables
semesters_z <- cbind(log_data_semesters,num_data_semesters)
semesters_r <- lowerCor(semesters_z)