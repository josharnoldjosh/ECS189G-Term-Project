nmfFinalModel <- function(data_name.tst) {
  library(recosystem)
  # Template nmf model used as a basis to create other nmf models
  
  # Extract data from the GroupLens 100k dataset
  all <- data_name.tst
  
  # TODO: Clean the data
  # all <- all[,c('usernum', 'movienum', 'rating')]
  
  # Get sample size, which is 95% of all the data
  smp_size <- floor(0.95 * nrow(all))
  
  # set the seed to make partition reproducible
  set.seed(9875)
  
  # get the indexes of 95% of the data
  train_ind <- sample(seq_len(nrow(all)), size = smp_size)
  
  # get training and test data
  train_data <- all[train_ind, ] #95% of all data
  test_data <- all[-train_ind, ] #5% of all data
  
  # TODO: need to create an object of class 'DataSource', specifying which
  # columns are user IDs, item IDs, and ratings for training and test set
  # all.trn <- (data_memory(train_data$usernum, train_data$movienum, train_data$rating, index1 = TRUE))
  # all.tst <- (data_memory(test_data$usernum, test_data$movienum, test_data$rating, index1 = TRUE))
  
  # recosystem package actions takes place within r
  r <- Reco()
  
  # TODO: do the factorization, with rank ...; do use NMF
  r$train(all.trn, opts=list(dim=50, nmf = TRUE))
  
  # COMMENTED OUT CODE PURPOSE: Get the W and H matrix and save it
  # result <- r$output(out_memory(), out_memory())
  
  # W <- result$P
  # H <- t(result$Q)
  
  # Save W and H into expected file
  # save(W, H, file = "WH.RData")
  
  # W and H matrices are here
  WH <- load("WH.RData")
  
  # Returns a predict.txt file with all 5000 predictions
  preds <- r$predict(all.tst)
  
  # Calculate MAPE - returns NaN
  # mape <- mean(abs((test_data$rating - preds) / test_data$rating)) * 100
  # cat("Mean Absolute Percentage Error:", mape, "\n")
}