#setup stuff
library(caret)
set.seed(100)
rm(list=ls())
#load iris data
data(iris)

#set predictors and response
predictors <- iris[,1:4]
response <- iris[,5]

#use ggplot to visualize the unscaled data to then make predictions from
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#now trying the same thing with scaled data
scaled_iris <- scale(iris[,1:4])
scaled_iris <- data.frame(scaled_iris, Species = iris$Species)
ggplot(scaled_iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
ggplot(scaled_iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#we use kmeans() on all of our scaled data and test varying the number of centers
elbow <- matrix(nrow = 20,ncol = 2, )
colnames(elbow) <- c("Cluster centers", "Total within cluster sum of squares")
for (c_cent in 1:20) {
  #nstart of 5 means it will choose 5 different sets of starting clusters and return
  #the solution with the lowest total within-cluster sum of squares.
  cluster <- kmeans(scaled_iris[,1:4], centers = c_cent, nstart = 5)
  
  #add the # of centers and the total within sum of squares to make an elbow plot later
  elbow[c_cent, 1] <- c_cent
  elbow[c_cent, 2] <- cluster[["tot.withinss"]]
}
#plot and add label
plot(elbow[,1], elbow[,2], type = "p", xlab = "Number of Clusters", ylab = "Total Within Sum of Squares")
text(elbow[4, 1], elbow[4, 2], labels = paste("4 Centers, tot.withinss  \n = ", elbow[4,2]), pos = 4, offset = 3, col = "red")
arrows(x0 = elbow[4, 1]+3, y0 = elbow[4, 2]+1, x1 = elbow[4, 1], y1 = elbow[4, 2], angle = 30, length = 0.1, col = "red")

#plot the data with 3 and 4 clusters for visualization:
cluster <- kmeans(scaled_iris[,3:4], centers = 3, nstart = 5)

#add the model decisions to the scaled data
iris_with_clusters <- cbind(scaled_iris, Cluster = as.factor(cluster$cluster))

#plot it
ggplot(iris_with_clusters, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point()

#plot it but petals
ggplot(iris_with_clusters, aes(x = Petal.Length, y = Petal.Width, color = Cluster)) + geom_point()

#now to calculate the percent that the full model got correct
predicted_clusters <- as.factor(cluster$cluster)
true_species <- as.factor(iris[, 5])
total_correct <- 0
setosa_correct <- 0
setosa_incorrect <- 0
versicolor_correct <- 0
versicolor_incorrect <- 0
virginica_correct <- 0
virginica_incorrect <- 0

#loop to check each row for correctness. cluster # matched with species, then increment count if correct
for (i in 1:nrow(iris)) {
  if ((predicted_clusters[i] == 1) && (true_species[i] == "setosa")) {
    total_correct <- total_correct + 1
    setosa_correct <- setosa_correct + 1
    }
  if ((predicted_clusters[i] != 1) && (true_species[i] == "setosa")) {
    setosa_incorrect <- setosa_incorrect + 1}
    
  if ((predicted_clusters[i] == 2) && (true_species[i] == "versicolor")) {
    total_correct <- total_correct + 1
    versicolor_correct <- versicolor_correct + 1
    
  if ((predicted_clusters[i] != 2) && (true_species[i] == "versicolor")) {
      versicolor_incorrect <- versicolor_incorrect + 1}
  }
  if ((predicted_clusters[i] == 3) && (true_species[i] == "virginica")) {
    total_correct <- total_correct + 1
    virginica_correct <- virginica_correct + 1
  }
  if ((predicted_clusters[i] != 3) && (true_species[i] == "virginica")) {
    virginica_incorrect <- virginica_incorrect + 1}
}
# Calculate accuracy
accuracy <- total_correct / nrow(iris)
setosa_perc <- setosa_correct / (setosa_correct + setosa_incorrect)
versicolor_perc <- versicolor_correct / (versicolor_correct + versicolor_incorrect)
virginica_perc <- virginica_correct / (virginica_correct + virginica_incorrect)

# Print the accuracy
cat("Accuracy of model with k=3 using just petal length/width as predictors =", accuracy, "\nSetosa Accuracy =", setosa_perc, "\nVersicolor Accuracy =", versicolor_perc,"\nVirginica Accuracy =", virginica_perc)