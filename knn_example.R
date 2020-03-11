source('./data_loader.R')
source('./eval.R')
source('./knn.R')

# Load data
datasets <- load_project_data()

# Embed data
datasets$InstEval <- embedMeans(datasets$InstEval, cache='ie')

# Split data into test & train
split <- train_test_split(datasets$InstEval)

# Fit model
output <- knn(split$train, split$test, 50)

# Probs
probs <- attributes(output)$probs
head(probs)

# Output
y_hat <- attributes(output)$y_hat

# Calculate MAPE
score<-mape(split$test$rating, y_hat)

# Print
score


