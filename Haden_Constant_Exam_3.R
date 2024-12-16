# clear all
rm(list = ls())
cat("\014")
graphics.off()

# load in necessary libraries
library(dplyr)

# load in data
data <- read.csv("ECON_418-518_Exam_3_Data.csv")

# create new variable for if an observation is in november or not
if_Nov <- ifelse(data$time_period == "Nov", 1, 0)
# add if_Nov to data table
data$if_Nov <- if_Nov

# create new variable for if the state is new jersey or not
if_NJ <- ifelse(data$state == 1, 1, 0)
# add if_NJ to data table
data$if_NJ <- if_NJ

# finding mean total employment in each state in each time period
# new data table mean_emp, which is the mean total_emp for when if_NJ = 0 and
# if_Nov = 0, when if_NJ = 1 and if_Nov = 0, and so on through all combinations
mean_emp <- aggregate(total_emp ~ if_NJ + if_Nov, data = data, FUN = mean,
                      na.rm = TRUE)

# estimate a difference-in-differences model w/out using lm() (i.e, manually)
did <- (mean_emp$total_emp[4] - mean_emp$total_emp[3]) - (mean_emp$total_emp[2]
                                                          - mean_emp$total_emp[1])
# difference-in-differences model w/ lm()
did2 <- lm(total_emp ~ if_NJ * if_Nov, data = data)
# summarize to find coefficient
summary(did2)

# create new restaurant fixed effect
rest_fe <- factor(data$restaurant_id)
# new did model using lm(), this time with restaurant fixed effect
did3 <- lm(total_emp ~ if_NJ * if_Nov + rest_fe, data = data)
# summarize to find coefficient
summary(did3)
