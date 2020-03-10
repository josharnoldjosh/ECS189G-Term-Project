# Our main KNN function
knn <- function(df, test, nc) {
  library(regtools)
  
  # Create X and y
  x <- df 
  y <- df$rating
  
  # Remove y from X matrices
  x$rating <- NULL
  test$rating <- NULL
  
  # Pass through model
  result <- kNN(x, y, test, nc)
  
  # Return result
  return (result$regests)
}

# Load data
source('./data_loader.R')
datasets <- load_project_data()

# Embed means (Optional)
datasets$InstEval <- embedMeans(datasets$InstEval, cache='ie')

# Helper function
score <- function(split, y_hat) {
  source('./eval.R')
  y <- split$test$rating
  mape_result <- mape(y, y_hat)
  return (mape_result)
}

nc_grid_search <- c(1:50)
history <- c()
for (nc in nc_grid_search) {
  
  # K Fold cross validation
  k_fold_score <- 0
  for (i in 1:10) {
    split <- k_fold(datasets$InstEval, 10, i)
    y_hat <- knn(split$train, split$test, nc)
    k_fold_score <- k_fold_score + score(split, y_hat)
  }
  k_fold_score <- k_fold_score/10
  history <- c(history, k_fold_score)
  print("K Fold score")
  print(nc)
  print(k_fold_score)
}

print(history)

