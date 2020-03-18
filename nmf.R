# Updates a dataframes "rating" column to be 0 or 1 based on the idx value passed to it
extract_binary_matrix <- function(data, idx) {
  f <- function(row) {
    if (is.na(row[3])) {
      return (NA)
    }
    
    if (row[3] == idx) {
      return (1)
    }
    return (0)
  }
  data$rating <- apply(data, 1, f)
  return (data)
}

# Takes a dataframe and returns a list of the dataframes
# Converted to binary matrices.
# See "extract_binary_matrix"
generate_binary_matrices <- function(data) {
  f <- function(idx) {
    return (extract_binary_matrix(data, idx))
  }
  
  return (lapply(1:5, f))
}

# Helper functions for training & predicting with NMF
nmf_train <- function(train, dim=50, index1=TRUE) {
  library(recosystem)
  library(lme4)
  library(Matrix)
  r <- Reco()
  train_mem <- data_memory(train$userID, train$itemID, train$rating, index1 = TRUE)
  r$train(train_mem, opts=list(dim=dim, nmf = TRUE))
  return (r)
}
nmf_pred <- function(r, test_mat, bias=0) {
  test <- data_memory(test_mat$userID, test_mat$itemID, test_mat$rating, index1 = TRUE)
  preds <- r$predict(test, out_memory())
  preds <- preds + bias
  preds <- round(preds)
  return (preds)
}
nmf_train_pred <- function(train, test, idx, dim=100, bias=0, index1=TRUE) {
  train_mats <- generate_binary_matrices(train)
  test_mats <- generate_binary_matrices(test)
  model <- nmf_train(train_mats[[idx]], dim, index1)
  preds <- nmf_pred(model, test_mats[[idx]], bias)
  return (preds)
}

# Convert binary output to "votes"
get_votes_from_nmf_output <- function(results, forest_size) {
  vote_mult <- t(replicate(dim(results)[1], rep(c(1, 2, 3, 4, 5), forest_size)))
  results <- results*vote_mult
  return (results)
}

# Train all the NMFs
train_all <- function(train, test, dim, bias, forest_size, index1) {
  f <- function(idx) {
    return (nmf_train_pred(train, test, idx, dim, bias, index1))
  }
  preds<-lapply(1:5, f)
  df <- data.frame(Reduce(cbind, preds))
  names(df) <- c("1", "2", "3", "4", "5")
  return (df)
}

# Our main function
# Given train and test data, will output probabilities
# Dim specifies the latent space
# Bias specifies whether we're more likely to have more votes (less confident)
# Forest size specifies more predictions to average out the error but more computation time
# index1 specifies whether or not our datasets start from 1 or 0 with the user and item ID's
nmf <- function(train, test, dim=100, bias=0, forest_size=0, index1 = TRUE) {
  
  # Bias
  if (bias < 0) {
    bias = 0
  }else if (bias > 0.45) {
    bias <- 0.45
  }
  
  # Train nmfs
  df <- train_all(train, test, dim, bias, index1)
  if (forest_size > 0) {
    for (i in 1:forest_size) {  
      df <- cbind(df, train_all(train, test, dim, bias, index1))
    }
  }

  # Get votes
  df <- get_votes_from_nmf_output(df, forest_size)
  
  # Get probs
  probs <- votes_to_prob(df, replace_na=0.2)
  
  # Return
  return(probs)
}


# Tunes a single nmf
# Saves the output params
nmf_tune <- function(data, ext='nmf') {
  train_data <- data_memory(data$userID, data$itemID, data$rating, index1 = TRUE)
  r = Reco()
  
  to_search <- c(0.01, 0.1)
  
  result <- r$tune(train_data, opts = list(dim      = c(10L, 100L,250L, 500L),
                                           costp_l1 = to_search,
                                           costp_l2 = to_search,
                                           costq_l1 = to_search,
                                           costq_l2 = to_search,
                                           lrate    = to_search)
                                          )
  nmf_ops <- result$min
  
  # Save
  save(nmf_ops, file = paste("tune/",ext, ".RData", sep=""))
  
  return (nmf_ops)
}

ratingNMF <- function (train, specialArgs) {
  
  # Load Special Args
  if (is.null(specialArgs[["dim"]])) {
    dim <- 100
  }else{
    dim <- specialArgs[["dim"]]
  }
  if (is.null(specialArgs[["bias"]])) {
    bias <- 0.3
  }else{
    bias <- specialArgs[["bias"]]
  }
  if (is.null(specialArgs[["forest_size"]])) {
    forest_size <- 3
  }else{
    forest_size <- specialArgs[["forest_size"]]
  }
  
  probsFitOut<-list(
      predMethod = "NMF",
      train_data = train,
      userList=train$userID,
      itemList=train$itemID,
      dim=dim,
      bias=bias,
      forest_size=forest_size
    )
  
  class(probsFitOut)<-"recProbs"
  
  return (probsFitOut)
}

NMFPredict <- function (probsFitOut, newXs) {
  train <- probsFitOut[["train_data"]]
  
  # For test, we need to add "space" for the ratings to be predicted
  # We do not know the ratings for newXs, so we put NA
  test <- newXs
  test$rating <- rep(NA, nrow(test))
  
  dim <- probsFitOut[["dim"]]
  bias <- probsFitOut[["bias"]]
  forest_size <- probsFitOut[["forest_size"]]
  result <- nmf(train, test, dim = dim, bias=bias, forest_size=forest_size)
  return (result)
}