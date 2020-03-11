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
probs <- knn(split$train, split$test, 5)

print(probs[1, ])

# Calculate MAPE
#score<-mape(split$test$rating, y_hat)

# Print
#score


