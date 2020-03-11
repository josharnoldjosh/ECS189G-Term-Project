fit_knn <- function(train, nc) {
  library(regtools)
  
  # Define x & y
  y<-train$rating
  train$rating<-NULL
  x<-preprocessx(train, nc)
  
  # Fit the model
  model<-knnest(y, x, nc)
  
  # Return value
  return(model)
}

pred_knn <- function(test, model) {
  y_test<-test$rating
  split$test$rating<-NULL
  y_hat <- predict(result, as.matrix(split$test))
  y_hat<-round(y_hat)
  return (y_hat)
}