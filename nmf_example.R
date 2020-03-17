source('./data_loader.R')
source('./nmf.R')
source('./eval.R')

# Load data
datasets <- load_project_data()

# Split data into test & train
split <- train_test_split(datasets$InstEval)

# Get output from nmf
result <- nmf(split$train, split$test, dim = 100, bias=0.3, forest_size=3)

# View output
head(result)

#############################
#       Tune the data       #
#############################

#nmf_tune(datasets$InstEval, 'InstEval')
nmf_tune(datasets$SongList, 'SongList') # Run me before bed