knn <- function(train, test, nc=50) {
  
  # Load x & y
  x_train<-train
  x_train$rating <- NULL
  y_train<-train$rating
  
  # Load test
  x_test<-test
  x_test$rating <- NULL
  
  # Fit
  #knnout <- kNN(x_train, y_train, x_train, nc)
  knnout <- kNN(x_train, y_train, x_test, nc)
  
  # Calculate vectors of ratings
  f<-function(idx) {
    return(train[idx, ]$rating)
  }
  rating_vec<- apply(knnout$whichClosest, 1, f)
  rating_vec <- t(rating_vec)
  
  # Calculate probs
  source('./eval.R')
  probs <- votes_to_prob(rating_vec)
  
  # Create output
  output <- "KNN Output Object"
  
  attributes(output)$probs <- probs
  attributes(output)$y_hat <- round(knnout$regests)
  
  # Return
  return(output)
}

# Special Args
# nc, the number of clusters, 50 or 100 should be great
# cache, a unique name for the dataset you are using
rating_kNN <- function (train, specialArgs) {
  
  if (is.null(specialArgs[["nc"]])) {
    #stop("Couldn't find 'nc', an integer value in the specialArgs for KNN.")
    nc <- 100
  }else{
    nc <- specialArgs[["nc"]]
  }
  
  if (is.null(specialArgs[["cache"]])) {
    stop("Couldn't find 'cache', an character/string value in the specialArgs for KNN.")
  }
  

  probsFitOut<-list(predMethod = "kNN", train_data = train, nc = nc, cache=specialArgs[["cache"]], userList=train$userID, itemList=train$itemID)
  class(probsFitOut)<-"recProbs"
  return (probsFitOut)
}

kNN_predict <- function (probsFitOut, newXs) {
  source("./data_loader.R")
  
  # Load Data
  train <- probsFitOut[["train_data"]]
  cache <- probsFitOut[["cache"]]
  test <- newXs
  
  # Embed Data
  result <- embed_test_from_train(train, cache, test)
  train <- result[["train"]]
  test <- result[["test"]]
  
  # Ensure test is appropriate
  test$userID <- as.numeric(test$userID)
  test$itemID <- as.numeric(test$itemID)

  # Parameters
  nc <- as.numeric(probsFitOut[["nc"]])
  
  # Get the output
  output <- knn(train, test, nc)
  
  # Get probs
  probs <- attr(output, 'probs')
  
  probs <- as.data.frame(probs)
  names(probs) <- c("1","2", "3", "4", "5")
  
  # Return the output
  return (probs)
}
