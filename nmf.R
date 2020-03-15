# Updates a dataframes "rating" column to be 0 or 1 based on the idx value passed to it
extract_binary_matrix <- function(data, idx) {
  f <- function(row) {
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

# Given a train and a test dataset, will return a vector of votes
nmf <- function(train, test, dim=100, bias=0, forest_size=0, index1 = TRUE) {
  
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
  probs <- votes_to_prob(df)
  
  # Return
  return(probs)
}