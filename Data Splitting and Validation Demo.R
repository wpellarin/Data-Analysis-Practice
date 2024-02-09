rm(list=ls()) 
library(kknn)
library(caret)
set.seed(100)
setwd("C:/Users/Wyatt/Documents/WP Mac Drive/Intro to Analytics Modeling")
data <- read.table("credit_card_data.txt", header = FALSE)

#First, separate data into 20% test data and 80% data for training/validation
#set the percent to be test data as 20%, randomize a set of numbers from 1-nrow(data), then set test_data and train_data
perc_test <- .2
shuffled_nums <- sample(1:nrow(data))
num_rows_test <- round(perc_test * nrow(data))
num_rows_train <- nrow(data) - num_rows_test
test_data <- data[shuffled_nums[1:num_rows_test], , drop = FALSE]
train_data <- data[shuffled_nums[(num_rows_test + 1):nrow(data)], , drop = FALSE]

#set variables for the range of k to check
kstart <- 1
kend <- 20

#now we'll do k-fold cross validation on the data
#set k_fold to 10 for 10 groups of data, and make those groups 1 time. $V11 specifies column 11 as the response variable
#createmultifolds returns a list of indices for each k that excludes the randomized values chosen to be the validation fold 
k_fold <- 10
times <- 1
groups <- createMultiFolds(train_data$V11, k = k_fold, times = times)

#each group has 470/471 values, which is expected (.8*654)*.9
#now, we want to train then validate the model on each of those groups

#matrix to store accuracy
accuracy_matrix <- matrix(nrow = k_fold * times, ncol = kend - kstart + 2)

for (fold in 1:(k_fold * times)) {
  
  #set the validation data by excluding rows with indices in the output in "groups", since those are the training groups in the k-fold validation
  k_test <- train_data[-unlist(groups[[fold]]), , drop = FALSE]
  #set the training data by including rows in that index
  k_train <- train_data[unlist(groups[[fold]]), , drop = FALSE]
  
  for (k_loop in kstart:kend) {
    #run the model and count how many it gets right
    count <- 0
    model <- kknn(V11 ~ ., train = k_train, test = k_test, k = k_loop, distance = 2, kernel = "optimal", scale = TRUE)
    decisions <- round(fitted.values(model))
    
    for (i in 1:(nrow(k_test))) {
      if (decisions[i] == k_test[i, 11]) {
        count <- count + 1
      }
    }
    
    accuracy <- count / nrow(k_test)
    
    #save accuracy in the matrix
    accuracy_matrix[fold, k_loop - kstart + 1] <- accuracy
    accuracy_matrix[fold, kend - kstart + 2] <- k_loop
    
    #print additional information
    cat("Fold =", fold, ", k in KKNN =", k_loop, ", Accuracy =", accuracy, "\n")
    cat("Training Set Size:", nrow(k_train), "\n")
    cat("Testing Set Size:", nrow(k_test), "\n")
    cat("Decision split in Training Set:", table(k_train$V11), "\n")
    cat("Decision split in Testing Set:", table(k_test$V11), "\n\n")
  }
}

#calculate and print average accuracy by k
avg_accuracy_by_k <- colMeans(accuracy_matrix[, 1:(kend - kstart + 1)], na.rm = TRUE)
avg_accuracy_matrix <- cbind(avg_accuracy_by_k, 1:(kend - kstart + 1))
colnames(avg_accuracy_matrix) <- c("Average Accuracy", "k Value")
print("Average Accuracy by k:")
print(avg_accuracy_matrix)

#k=15 has the highest average accuracy in our kknn model, but we also want to check the SVM model, so we'll do something similar:
library(kernlab)
cloop_count <- 0
fold <- 0
clist <- c(0.00001, 0.0001,0.001,0.01,0.1,1,10,100,1000,10000,100000)
svm_accuracy_matrix <- matrix(nrow = (length(clist)*k_fold*times), ncol = 3)
colnames(svm_accuracy_matrix) <- c("Fold","C Value", "Accuracy")

for (fold in 1:(k_fold * times)) {
  
  #set the testing data by excluding rows with indices in the output in "groups", since those are the training groups in the k-fold validation
  k_test <- train_data[-unlist(groups[[fold]]), , drop = FALSE]
  #set the training data by including rows in that index
  k_train <- train_data[unlist(groups[[fold]]), , drop = FALSE]
  
  for (cloop in clist) {
    #train the model on k_train to find the best magnitude of c
    cloop_count <- cloop_count + 1
    count <- 0
    svmmodel <- ksvm(as.matrix(k_train[,1:10]),as.factor(k_train[,11]),type="C-svc",kernel="vanilladot",C=cloop,scaled=TRUE)
    # see what the model predicts
    pred <- predict(svmmodel,k_train[,1:10])
    pred
    # see what fraction of the model’s predictions match the actual classification
    CalcPerc <- (sum(pred == k_train[,11]) / nrow(k_train))
    
    #print additional information
    cat("Fold =", fold, ", c in SVM =", cloop, ", Accuracy =", CalcPerc, "\n")
    cat("Training Set Size:", nrow(k_train), "\n")
    cat("Testing Set Size:", nrow(k_test), "\n")
    cat("Decision split in Training Set:", table(k_train$V11), "\n")
    cat("Decision split in Testing Set:", table(k_test$V11), "\n\n")
    
    #add to accuracy matrix
    svm_accuracy_matrix[cloop_count, ] <- c(fold,cloop, CalcPerc)

  }
}

# Calculate and print average accuracy per c value
avg_accuracy_by_c <- tapply(svm_accuracy_matrix[, 3], svm_accuracy_matrix[, 2], mean, na.rm = TRUE)
print(avg_accuracy_by_c)

#We see that a C value between 0.01 and 100 give an accuracy of 86.233%, higher than the kknn model's highest.
#We could run the SVM model to try more granular C values, but for time/processing restraints 
#We'll decide to use c=10 and use our full 80% training data to train with that model, and then our 20% testing data to test.

svmmodel <- ksvm(as.matrix(train_data[,1:10]),as.factor(train_data[,11]),type="C-svc",kernel="vanilladot",C=.1,scaled=TRUE)
pred <- predict(svmmodel,test_data[-11])
CalcPerc <- (sum(pred == test_data[,11]) / nrow(test_data))
cat("Calculated accuracy of chosen model on test data =",CalcPerc)

#Now, a quick runthrough without cross-validation, just a simple train/validate/test split
rm(list=ls()) 
library(kknn)
library(caret)
set.seed(100)
setwd("C:/Users/Wyatt/Documents/WP Mac Drive/Intro to Analytics Modeling")
data <- read.table("credit_card_data.txt", header = FALSE)

#setup variables and split data
perc_test <- .15
perc_val <- .15
shuffled_nums <- sample(1:nrow(data))
num_rows_test <- round(perc_test * nrow(data))
num_rows_val <- round(perc_val * nrow(data))
num_rows_train <- nrow(data) - num_rows_test - num_rows_val
test_data <- data[shuffled_nums[1:num_rows_test], , drop = FALSE]
val_data <- data[shuffled_nums[1:num_rows_val], , drop = FALSE]
train_data <- data[shuffled_nums[(num_rows_test + num_rows_val + 1):nrow(data)], , drop = FALSE]
kstart <- 1
kend <- 20

for (k_loop in kstart:kend) {
  #run the model and count how many it gets right per k
  count <- 0
  model <- kknn(V11 ~ ., train = train_data, test = val_data, k = k_loop, distance = 2, kernel = "optimal", scale = TRUE)
  decisions <- round(fitted.values(model))
  
  for (i in 1:(nrow(val_data))) {
    if (decisions[i] == val_data[i, 11]) {
      count <- count + 1
    }
  }
  
  accuracy <- count / nrow(val_data)
  cat("k = ",k_loop," Accuracy = ",accuracy,"\n")
}

#finally, we test the model on our test data
count <- 0
model <- kknn(V11 ~ ., train = train_data, test = test_data, k = 6, distance = 2, kernel = "optimal", scale = TRUE)
decisions <- round(fitted.values(model))

for (i in 1:(nrow(test_data))) {
  if (decisions[i] == test_data[i, 11]) {
    count <- count + 1
  }
}

accuracy <- count / nrow(test_data)
cat("Accuracy on test data with k = 6: ", accuracy)