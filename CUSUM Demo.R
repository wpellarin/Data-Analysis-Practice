#setup stuff
rm(list=ls())
set.seed(100)

#load data
setwd("~/WP Mac Drive/Intro to Analytics Modeling")
data <- read.table("temps.txt", header = TRUE)

#plot it
matplot(1:nrow(data),data[,-1], col = 1:ncol(data), type = "l")

mean_list <- colMeans(data[1:45,-1])
print(mean_list)

std_list <- apply(data[1:45,-1],2,sd)
print(std_list)

CUSUM_mean <- mean(mean_list)
CUSUM_std <- mean(std_list)
cat("Mean: ", CUSUM_mean, "Standard Deviation: ", CUSUM_std)

CUSUM_T <- 25
CUSUM_C <- 2

#create a matrix to house our variance from mean @ time t
Var_T_matrix <- matrix(nrow = nrow(data), ncol = ncol(data))
Var_T_matrix[,1] <- data[,1]

#create a matrix to house our S_t accumulations
S_t_matrix <- matrix(nrow = nrow(data), ncol = ncol(data))
S_t_matrix[,1] <- data[,1]
S_t <- 0

#loop columns, then rows to calculate each value
for (c in 2:ncol(data)) {
  S_t <- 0
  for (i in 1:nrow(data)) {
    
    #Use CUSUM formula to calculate change at that time
    Var_t <- (-data[i,c] + CUSUM_mean - CUSUM_C)
    
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

first_cross <- matrix(nrow = (ncol(S_t_matrix)-1), ncol = 2)
first_cross[,1] <- 1996:2015

#loop columns and rows again
for (j in 2:ncol(data)) {
  for (k in 1:nrow(data)) {
    #turn our matrix value into a numeric for our comparison to work
    num <- as.numeric(S_t_matrix[k,j])
    #if it's larger than T, save the index and break the inner loop
    if (num > CUSUM_T) {
      first_cross[j-1,2] <- k
      break
    }
  }
}

plot(first_cross)
median(first_cross[,2])
#--------------------------------------------------
#setup stuff
rm(list=ls())
set.seed(100)

#load data
setwd("~/WP Mac Drive/Intro to Analytics Modeling")
data <- read.table("temps.txt", header = TRUE)

noday <- matrix(nrow = nrow(data), ncol = ncol(data)-1)
noday <- data[,-1]

#plot it
matplot(1:ncol(noday),t(noday), type= "l", col = 1:nrow(noday))

mean_list <- rowMeans(noday[,1:7])
std_list <- apply(noday[,1:7],1,sd)

print(mean_list)
print(std_list)

  
cross_count <- matrix(nrow = ncol(noday), ncol = 1)
CUSUM_C <- 3
CUSUM_T <- 6
for (n in 1:nrow(noday)){

#create a matrix to house our variance from mean for each day/year combo
Var_T_matrix <- matrix(nrow = nrow(noday), ncol = ncol(noday))

#create a matrix to house our S_t accumulations
S_t_matrix <- matrix(nrow = nrow(noday), ncol = ncol(noday))
S_t <- 0

#loop rows, then columns to calculate each value
for (c in 1:nrow(noday)) {
  S_t <- 0
  count <- 0
  for (i in 1:ncol(noday)) {
    
    #Use CUSUM formula to calculate change at that time
    Var_t <- (noday[c,i] - mean_list[c] - CUSUM_C)
    
    #Store the difference in our matrix (with 2 decimals)
    Var_T_matrix[c,i] <- round(Var_t, digits = 2)
    
    #If the difference is greater than zero, we increment S_t
    if (Var_t > 0) {
      S_t <- S_t + Var_t
      S_t_matrix[c,i] <- S_t
    }
    #Otherwise, we set S_t back to 0
    else {
      S_t <- 0
      S_t_matrix[c,i] <- S_t
    }
    #turn our matrix value into a numeric for our comparison to work
    num <- as.numeric(S_t_matrix[c,i])
    #if it's larger than T, increment count
    if (num > CUSUM_T) {
      count <- count + 1
      cross_count[i,1] <- count
      }
    }
  }
}
plot(cross_count)
