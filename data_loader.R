
# TODO: Maybe modify code to embed the item ID's too?
embedMeans <- function(data) {
  library(rectools)
  
  if (!all(names(data)[1:3] == c("userID", "itemID", "rating"))) {
    stop("Make sure 'is_y_last=FALSE' when calling 'load_data'")
  }
  
  embedData <- function (data, path) {
    load(path)
    ie_form <- retval
    
    f <- function(x) {
      return (mean(user_data[[as.integer(x)]]$ratings))
    }
    
    data$UserID <- sapply(data$userID, f)
    data$ItemID <- NULL
    return (data)
  }
  
  if (nrow(data) == 73421) {
    return (embedData(data, './user_data/ie.RData'))
  }else{
    return (embedData(data, './user_data/song.RData'))
  }
}

# This function loads data into memory
load_data <- function() {
  library(regtools)
  library(lme4)
  
  InstEval <- InstEval[, c("userID", "itemID", "rating")]
  
  SongList <- read.csv(file = './data/songsDataset.csv')
  names(SongList) <- c("userID", "itemID", "rating")
    
  datasets <- list()
  datasets$InstEval <- InstEval
  datasets$SongList <- SongList
  return (datasets)
}

# Ensure the dataset is standardized
load_general_dataset <- function(data) {
  data <- data[, c("userID", "itemID", "rating")]
  return (data)
}

# Train test split
train_test_split <- function(data, test_ratio=0.3) {
  library(caret)
  train.index <- createDataPartition(data$y, p = (1-test_ratio), list = FALSE)
  train <- data[ train.index,]
  test  <- data[-train.index,]
  result <- list()
  result$train <- train
  result$test <- test
  return (result)
}

# This function only needs to be run once, but probably never
# Check if output files exist
init <- function() {
  library(rectools)
  datasets <- load_data(is_y_last = FALSE)
  formUserData(datasets$InstEval[, 1:3], fileOut = "./user_data/ie.RData")
  formUserData(datasets$SongList[,1:3], fileOut = "./user_data/song.RData")
}