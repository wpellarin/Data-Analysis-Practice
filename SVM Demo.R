#install.packages("kernlab")

rm(list = ls())
library(kernlab)


# Take the loaded data and turn it into a data frame. Columns 1-10 are attributes, and column 11 is Yi, the "correct" decision about that data point.
data <- read.table("credit_card_data.txt", header = FALSE)
head(credit_card_data.txt)
tail(credit_card_data.txt)
# Create a support vector machine model using the ksvm function, loop it with different C values to find the best predictor.Initialize a BestC variable.
BestPerc <- 0
Ceq <- 0
BestCeq <- 0
while (Ceq < 100){
Ceq <- (Ceq+0.01)
Ceq <- 0.0014
model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=Ceq,scaled=TRUE)

# Print the summary of the SVM model
print(model)
# calculate a1…am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]]) #xmatrix are support vectors
a
# calculate a0
a0 <- -model@b
a0
# see what the model predicts
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
CalcPerc <- (sum(pred == data[,11]) / nrow(data))
print(Ceq)
if (CalcPerc > BestPerc){
  BestPerc = CalcPerc
  BestCeq = Ceq
}
}
print(BestCeq)
print(BestPerc)