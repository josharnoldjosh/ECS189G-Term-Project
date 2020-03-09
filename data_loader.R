# This function loads data into memory
load_data <- function(y_is_last_column=FALSE) {
  library(lme4)
  names(InstEval) <- c("user_id", "item_id", "studage", "lectage", "service", "dept", "y")
  if (!y_is_last_column) {
    InstEval <- InstEval[, c("user_id", "item_id", "y", "studage", "lectage", "service", "dept")]
  }
  SongList <- read.csv(file = 'songsDataset.csv')
  names(SongList) <- c("user_id", "item_id", "y")
  datasets <- list()
  datasets$InstEval <- InstEval
  datasets$SongList <- SongList
  return (datasets)
}