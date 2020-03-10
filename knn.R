# KNN Function. Note, both x and y must have the EXACT SAME columns.
# Just the number of rows changes.
# Thus, we could split the data simply by rows and not even consider the rating or y value.
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

# Convert to numeric
datasets$SongList <- numeric(datasets$SongList)

# Embed means (Optional)
datasets$SongList <- embedMeans(datasets$SongList, cache='song')

# Split data
split <- train_test_split(datasets$SongList)

# Get result
y_hat <- knn(ie_split$train, ie_split$test, 10)

# Compare
y <- ie_split$test$rating
score <- mean(abs(y - y_hat))
print("MAPE: ", score)



