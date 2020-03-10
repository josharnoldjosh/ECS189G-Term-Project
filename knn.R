# Our main KNN function
knn <- function(df, test, nc) {
  library(regtools)
  x <- df[, 1:2] # Forcefully exclude y
  y <- df$rating
  test <- test[, 1:2] # Forcefully exclude y from test
  result <- kNN(x, y, test, nc)
  return (result$regests)
}

# Load data
source('./data_loader.R')
datasets <- load_project_data()

# Convert to numeric (Only if we don't need factors)
datasets$InstEval <- numeric(datasets$InstEval)

# Embed means (Optional)
datasets$InstEval <- embedMeans(datasets$InstEval, cache='ie')

# Split data
split <- train_test_split(datasets$InstEval)

# Pass through model & predict
y_hat <- knn(ie_split$train, ie_split$test, 10)

# Calculate MAPE
y <- ie_split$test$rating
score <- mean(abs(y - y_hat))
print(score)



