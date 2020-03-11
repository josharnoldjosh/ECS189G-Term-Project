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
model<-fit_knn(split$train, 100)

# Predict y_hat
y_hat<-pred_knn(split$test, model)

# Calculate MAPE
score<-mape(split$test$rating, y_hat)

# Print
score