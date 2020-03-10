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

#datasets$InstEval$itemID <- NULL

# Split data
split <- train_test_split(datasets$InstEval)

# Pass through model & predict
y_hat <- knn(split$train, split$test, 20)

# Calculate MAPE
source('./eval.R')
y <- split$test$rating
score <- mape(y, y_hat)
print(score)
