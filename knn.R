# KNN Function. Note, both x and y must have the EXACT SAME columns.
# Just the number of rows changes.
# Thus, we could split the data simply by rows and not even consider the rating or y value.
custom_knn <- function(x, y, pred, num_clusters) {
  library(regtools)
  result <- kNN(as.matrix(x), as.matrix(y), as.matrix(pred), num_clusters)
  return (result)
}