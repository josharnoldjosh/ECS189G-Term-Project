embedMeans <- function(data) {
  library(rectools)
  
  if (!all(names(data)[1:3] == c("UserID", "ItemID", "rating"))) {
    stop("Make sure 'is_y_last=FALSE' when calling 'load_data'")
  }
  
  embedData <- function (data, path) {
    load(path)
    ie_form <- retval
    
    f <- function(x) {
      return (mean(user_data[[as.integer(x)]]$ratings))
    }
    
    data$UserID <- sapply(data$UserID, f)
    data$ItemID <- NULL
    names(data)[1] <- "EmbeddedUserID"
    return (data)
  }
  
  if (nrow(data) == 73421) {
    return (embedData(data, './user_data/ie.RData'))
  }else{
    return (embedData(data, './user_data/song.RData'))
  }
}

# This function loads data into memory
load_data <- function(is_y_last=F) {
  library(lme4)
  library(regtools)
  names(InstEval) <- c("UserID", "ItemID", "studage", "lectage", "service", "dept", "rating")
  if (!is_y_last) {
    InstEval <- InstEval[, c("UserID", "ItemID", "rating", "studage", "lectage", "service", "dept")]
  }
  # Convert factors to dummies... ?
  SongList <- read.csv(file = './data/songsDataset.csv')
  names(SongList) <- c("UserID", "ItemID", "rating")
  datasets <- list()
  datasets$InstEval <- InstEval
  datasets$SongList <- SongList
  return (datasets)
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