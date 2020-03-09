# KNN Function. Note, both x and y must have the EXACT SAME columns.
# Just the number of rows changes.
# Thus, we could split the data simply by rows and not even consider the rating or y value.
custom_knn <- function(x, y, test, nc) {
  library(regtools)
  result <- kNN(as.matrix(x), as.matrix(y), as.matrix(test), nc)
  return (result)
}

# Load data
source('./data_loader.R')
datasets <- load_data(y_is_last_column = FALSE)

head(inst_eval_data)