#setup stuff
rm(list=ls())
#install.packages("outliers")
library("outliers")
set.seed(100)
#load data
setwd("~/WP Mac Drive/Intro to Analytics Modeling")
data <- read.table("uscrime.txt", header = TRUE, )
crime_data <- data$Crime


hist(crime_data, prob = TRUE)

#mean and standard deviation of the Crime data
mean_crime <- mean(crime_data)
sd_crime <- sd(crime_data)

#find x values for the normal curve
x_values <- seq(min(crime_data), max(crime_data), length = 100)

# find y values
y_values <- dnorm(x_values, mean = mean_crime, sd = sd_crime)

# Plot the normal curve
lines(x_values, y_values)

#run the grubbs test on the last column of our data
#Use type = 11, a test for two outliers in opposite tails
grubbs.test(crime_data,type = 11)

#type = 10 means we test for one outlier in one tail
grubbs.test(crime_data,type = 10, two.sided = FALSE)

for (i in 1:10) {
  grubbs <- grubbs.test(crime_data,type = 10, two.sided = FALSE, opposite = TRUE)
  crime_data <- crime_data[-which.max(crime_data)]
  print(grubbs)
}


data_sorted <- data[order(crime_data), ]
plot(1:47,data_sorted[,16],type="p")

shapiro.test(crime_data)

