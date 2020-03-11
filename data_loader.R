# Ensure the dataset is standardized
# Maybe concatonate other columns after
load_data <- function(data) {
  to_select <- c("userID", "itemID", "rating")
  data <- data[, to_select]
  data<-shuffle(data) # shuffle data
  data[is.na(data)] <- mean(data$rating)
  data$userID <- as.factor(data$userID)
  data$itemID <- as.factor(data$itemID)
  data$rating <- as.numeric(data$rating)
  return (shuffle(data))
}

# This function loads data into memory
load_project_data <- function() {
  library(regtools)
  library(lme4)
  
  names(InstEval) <- c("userID", "itemID", "a", "b", "c", "d", "rating")
  InstEval <- InstEval[, c("userID", "itemID", "rating")]
  InstEval <- load_data(InstEval)
  
  SongList <- read.csv(file = './data/songsDataset.csv')
  names(SongList) <- c("userID", "itemID", "rating")
  SongList <- load_data(SongList)
    
  datasets <- list()
  datasets$InstEval <- InstEval
  datasets$SongList <- SongList
  return (datasets)
}

# Train test split with a ratio
train_test_split <- function(data, test_ratio=0.3) {
  library(caret)
  train.index <- createDataPartition(data$rating, p = (1-test_ratio), list = FALSE)
  train <- data[ train.index,]
  test  <- data[-train.index,]
  result <- list()
  result$train <- train
  result$test <- test
  return (result)
}

embedUserID <- function (data, cache='') {
  # Form user data
  path <- paste('./user_data/', cache, '_user.RData', sep='')
  if (!file.exists(path)) {
    formUserData(data, fileOut = path)
  }
  
  # Load user data
  load(path)  
  user_data <- retval
  
  f <- function(user_id) {
    return (mean(user_data[[as.integer(user_id)]]$ratings))
  }
  
  data$userID <- sapply(data$userID, f)
  
  return (data)
}

embedItemID <- function (data, cache='') {
  # Form user data
  path <- paste('./user_data/', cache, '_item.RData', sep='')
  
  # PERFORM SWAP
  data <- data[, c(2, 1, 3)]
  
  if (!file.exists(path)) {
    formUserData(data, fileOut = path)
  }
  
  # Load user data
  load(path)  
  user_data <- retval
  
  f <- function(user_id) {
    return (mean(user_data[[as.integer(user_id)]]$ratings))
  }
  
  data$itemID <- sapply(data$itemID, f)
  
  # PERFORM SWAP
  data <- data[, c(2, 1, 3)]
  
  return (data)
}

# TODO: Maybe modify code to embed the item ID's too?
embedMeans <- function(data, cache='') {
  library(rectools)
  
  # Import Conversion
  data <- numeric(data)
  
  # Check if empty cache
  if (cache == '') {
    stop("Please provide a valid name for a cache file.")
  }
  
  data <- embedUserID(data, cache)
  data <- embedItemID(data, cache)
  return (data)
}

# Converts to dummies & dataframe format
dummify <- function(data) {
  library(regtools)
  data <- factorsToDummies(data, omitLast=TRUE)
  data <- as.data.frame(data, omitLast=TRUE)
  return (data)
}

# Converts relevant columns to numeric
numeric <- function(data) {
  data$userID <- as.numeric(data$userID)
  data$itemID <- as.numeric(data$itemID)
  data$rating <- as.numeric(data$rating)
  return (data)
}

# shuffles a dataset
shuffle <- function(data) {
  data <- data[sample(nrow(data)),]
  return (data)
}

# data is input data
# k is the k-fold value, e.g 10
# i is what fold we want, starting from 1 until 10
k_fold <- function(data, i, k=10) {
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  result <- list()
  result$train <- trainData
  result$test <- testData
  return (result)
}
