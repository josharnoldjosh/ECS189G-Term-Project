# KNN Function. Note, both x and y must have the EXACT SAME columns.
# Just the number of rows changes.
# Thus, we could split the data simply by rows and not even consider the rating or y value.
custom_knn <- function(df, test, nc) {
  library(regtools)
  x <- df
  y <- df$y
  result <- kNN(x, y, test, nc)
  return (result)
}

# Load data
source('./data_loader.R')
datasets <- load_project_data()
datasets$InstEval <- embedMeans(datasets$InstEval, cache='ie')
datasets$SongList <- embedMeans(datasets$SongList, cache='song')
head(datasets$InstEval)
head(datasets$SongList)