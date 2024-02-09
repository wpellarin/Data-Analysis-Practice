library(kknn)
#install.packages("caret")
library(caret)
set.seed(100)
setwd("C:/Users/Wyatt/Documents/WP Mac Drive/Intro to Analytics Modeling")
# Take the loaded data and turn it into a data frame. Columns 1-10 are attributes, and column 11 is Yi, the "correct" decision about that data point.
data <- read.table("credit_card_data.txt", header = FALSE)

#First, separate data into 20% test data and 80% data for training/validation
#set the percent to be test data as 20%, randomize a set of numbers from 1-nrow(data), then set test_data and train_data
perc_test <- .2
shuffled_nums <- sample(1:nrow(data))
num_rows_test <- round(perc_test * nrow(data))
num_rows_train <- nrow(data) - num_rows_test
test_data <- data[shuffled_nums[1:num_rows_test], , drop = FALSE]
train_data <- data[shuffled_nums[(num_rows_test + 1):nrow(data)], , drop = FALSE]

kstart <- 1 #set variables for the range of k to check
kend <- 20

#set k_fold to 10 for 10 groups of data, and make those groups 5 times. $V11 specifies column 11 as the response variable
k_fold <- 10
times <- 1
groups <- createMultiFolds(train_data$V11,k = k_fold, times = times)
#createmultifolds returns a list of indices for each k that excludes the randomized values chosen to be the validation fold 
#each group has 470/471 values, which is expected (.8*654)*.9
#now, we want to train then validate the model on each of those groups

for (j in 1:(k_fold*times)){
  #set the testing data by excluding rows with indices in the output in "groups", since those are the training groups in the k-fold validation
  k_test <- train_data[-unlist(groups[[j]]), , drop = FALSE]
  #set the training data by including rows in that index
  k_train <- train_data[unlist(groups[[j]]), , drop = FALSE]
  #run the kknn model
  for (k in kstart:kend) { #loop k
    count <- 0
    model <- kknn(V11~., train = k_train, test = k_test, k = k, distance = 2, kernel = "optimal", scale = TRUE)
    #Round the fitted value to 0 or 1 to be the "decision" of the model
    decisions <- round(fitted.values(model))
    for (i in 1:(nrow(k_test))){
    if (decisions[i] == k_test[i,11]) { #If the decision matches the actual outcome in our data, add a 1 to count
      count = count +1
    }
    }
    accuracy <- count / nrow(k_test)
    print(paste("k in k-fold =",j,", k in KKNN = ",k, ", Accuracy = ",accuracy))
  }
}

# plot fraction correct as function of k
plot(AvgFit[,1], AvgFit[,2], type = "l", xlab = "Number of Nearest Neighbors", ylab = "Fraction of Correct Decisions")
max_index <- which.max(AvgFit[,2])
points(AvgFit[,1][max_index], AvgFit[,2][max_index], col = "red", pch = 16)
text(AvgFit[,1][max_index], AvgFit[,2][max_index], labels = paste("Max Fraction:", AvgFit[,2][max_index], "at k =", AvgFit[,1][max_index]), pos = 4, col = "red")