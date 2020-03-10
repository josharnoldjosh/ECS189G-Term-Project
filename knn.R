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
datasets$SongList <- embedMeans(datasets$SongList, cache='song')

# Helper function
score <- function(split, y_hat) {
  source('./eval.R')
  y <- split$test$rating
  mape_result <- mape(y, y_hat)
  return (mape_result)
}

# grid search -------

upper_limit <- 10
nc_grid_search <- c(1:upper_limit)

ie_history <- c()
for (nc in nc_grid_search) {
  
  # K Fold cross validation
  k_fold_score <- 0
  for (i in 1:10) {
    split <- k_fold(datasets$InstEval, 10, i)
    y_hat <- knn(split$train, split$test, nc)
    k_fold_score <- k_fold_score + score(split, y_hat)
  }
  k_fold_score <- k_fold_score/10
  ie_history <- c(ie_history, k_fold_score)
  print("K Fold score")
  print(nc)
  print(k_fold_score)
}
ie_df <- data.frame(c(1:upper_limit), ie_history, rep("InstEval", upper_limit))
names(ie_df) <- c("x",'y', 'group')

song_history <- c()
for (nc in nc_grid_search) {
  
  # K Fold cross validation
  k_fold_score <- 0
  for (i in 1:10) {
    split <- k_fold(datasets$SongList, 10, i)
    y_hat <- knn(split$train, split$test, nc)
    k_fold_score <- k_fold_score + score(split, y_hat)
  }
  k_fold_score <- k_fold_score/10
  song_history <- c(song_history, k_fold_score)
  print("K Fold score")
  print(nc)
  print(k_fold_score)
}
song_df <- data.frame(c(1:upper_limit), song_history, rep("SongList", upper_limit))
names(song_df) <- c("x",'y', 'group')

df <- rbind(ie_df, song_df)

library(ggplot2)
ggplot(data=df, aes(x=x, y=y, colour=group)) + geom_line() + geom_point()
