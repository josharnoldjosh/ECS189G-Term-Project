knn <- function(train, test, nc) {
  
  # Load x & y
  x_train<-train
  x_train$rating <- NULL
  y_train<-train$rating
  
  # Load test
  x_test<-test
  x_test$rating <- NULL
  
  # Fit
  result <- kNN(x_train, y_train, x_test, nc)
  
  # Calculate vectors of ratings
  f<-function(idx) {
    return(train[idx, ]$rating)
  }
  rating_vec<- apply(result$whichClosest, 1, f)
  rating_vec <- t(rating_vec)
  
  # Calculate probs
  source('./eval.R')
  probs <- votes_to_prob(rating_vec)
  
  # Return
  return(probs)
}