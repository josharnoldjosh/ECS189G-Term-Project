knn <- function(train, test, nc) {
  
  # Load x & y
  x_train<-train
  x_train$rating <- NULL
  y_train<-train$rating
  
  # Load test
  x_test<-test
  x_test$rating <- NULL
  
  # Fit
  knnout <- kNN(x_train, y_train, x_test, nc)
  
  # Calculate vectors of ratings
  f<-function(idx) {
    return(train[idx, ]$rating)
  }
  rating_vec<- apply(knnout$whichClosest, 1, f)
  rating_vec <- t(rating_vec)
  
  # Calculate probs
  source('./eval.R')
  look_up <- form_look_up_table(x_test, rating_vec)
  
  # Create output
  output <- "KNN Output Object"
  
  attributes(output)$probs <- look_up
  attributes(output)$y_hat <- round(knnout$regests)
  
  # Return
  return(output)
}