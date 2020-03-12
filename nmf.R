NMF<- function() {
  # So far this is only for insteval, we can also change it to accomodate song dataset
  library(recosystem)
  library(lme4)
  library(Matrix)
  data(InstEval)
  all <- InstEval
  
  # TODO: Clean the data
  all <- all[,c('s', 'd', 'y')]
  colnames(all) <- c("Student", "Professor", "Rating")
  nrow(all)
  head(all)
  
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
  # columns are Student IDs, Professor IDs, and Ratings for training and test set
  all.trn <- (data_memory(train_data$Student, train_data$Professor, train_data$Rating, index1 = TRUE))
  all.tst <- (data_memory(train_data$Student, train_data$Professor, train_data$Rating, index1 = TRUE))
  
  #head(all.trn)
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
  #WH <- load(InstEval)
  
  # Returns a predict.txt file with all 5000 predictions
  preds <- r$predict(all.tst, out_memory())
  print(preds)
  # Calculate MAPE - returns NaN
  # mape <- mean(abs((test_data$rating - preds) / test_data$rating)) * 100
  # cat("Mean Absolute Percentage Error:", mape, "\n")
}
