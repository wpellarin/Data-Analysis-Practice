#setup stuff
rm(list=ls())
set.seed(100)

#load data
setwd("~/WP Mac Drive/Intro to Analytics Modeling")
data <- read.table("temps.txt", header = TRUE)

#Flatten the data into a vector
vdata <- as.vector(unlist(data[,2:21]))
plot(vdata, type = "l")

# Create time series
tsdata <- ts(vdata, start = 1996, frequency = 123)

# Holt-Winters model
model <- HoltWinters(tsdata, beta = FALSE, seasonal = "m")
plot(model$fitted)

model2 <- HoltWinters(tsdata, beta = FALSE, seasonal = "a")
plot(model2$fitted)

#plot TS data
plot(tsdata, type = "l", xlim = c(2003,2005))

# Plot model$fitted on top of tsdata
lines(model$fitted[,1], col = "blue")
lines(model2$fitted[,1], col = "red")

#First, save the seasonal coefficients in a matrix with row = day, column = year
fitData <- matrix(nrow = 123, ncol = 19)
for (i in 1:19) {
  fitData[,i] <- model$fitted[(123*(i-1)+1):(123*i),3]
  }

#plot them
matplot(1:nrow(fitData), fitData, col = 1:ncol(data), type = "l")

#Take the mean and std. dev of each row AKA the mean seasonal coefficient for each day
mean_list <- rowMeans(fitData[1:50,])
std_list <- apply(fitData[1:50,],2,sd)

CUSUM_mean <- mean(mean_list)
CUSUM_std <- mean(std_list)
cat("Mean: ", CUSUM_mean, "Standard Deviation: ", CUSUM_std)


CUSUM_T <- 6*CUSUM_std
CUSUM_C <- .7*CUSUM_std

#create a matrix to house our variance from mean @ time t
Var_T_matrix <- matrix(nrow = nrow(fitData), ncol = ncol(fitData))

#create a matrix to house our S_t accumulations
S_t_matrix <- matrix(nrow = nrow(fitData), ncol = ncol(fitData))
S_t <- 0

#loop columns, then rows to calculate each value
for (c in 1:ncol(fitData)) {
  S_t <- 0
  for (i in 1:nrow(fitData)) {
    
    #Use CUSUM formula to calculate change at that time
    Var_t <- (-fitData[i,c] + CUSUM_mean - CUSUM_C)
    
    #Store the difference in our matrix (with 2 decimals)
    Var_T_matrix[i,c] <- round(Var_t, digits = 2)
    
    #If the difference is greater than zero, we increment S_t
    if (Var_t > 0) {
      S_t <- S_t + Var_t
      S_t_matrix[i,c] <- S_t
    }
    #Otherwise, we set S_t back to 0
    else {
      S_t <- 0
      S_t_matrix[i,c] <- S_t
    }
  }}

#plot our S_t
matplot(1:nrow(S_t_matrix),S_t_matrix[,-1], col = 1:ncol(S_t_matrix), type = "l")
abline(h = CUSUM_T, col = "red", lty = 2)

matplot(1:nrow(S_t_matrix),S_t_matrix[,-1], col = 1:ncol(S_t_matrix), type = "l", ylim=c(0,150))
abline(h = CUSUM_T, col = "red", lty = 2)

first_cross <- matrix(nrow = (ncol(S_t_matrix)), ncol = 2)
first_cross[,1] <- 1997:2015

#loop columns and rows again
for (j in 1:ncol(fitData)) {
  for (k in 1:nrow(fitData)) {
    #turn our matrix value into a numeric for our comparison to work
    num <- as.numeric(S_t_matrix[k,j])
    #if it's larger than T, save the index and break the inner loop
    if (num > CUSUM_T) {
      first_cross[j,2] <- k
      break
    }
  }
}

plot(first_cross)
median(first_cross[,2])