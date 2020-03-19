#This is a file to test the TermProject.R 

# Source the necessary files
source("./TermProject.R")
source("./data_loader.R")

# Load the datasets(InstEval, songs)
datasets<-load_project_data()
ie<-datasets[[1]]
song<-datasets[[2]]

#This is the size of train dataset
subsetTrainSize<-1000
#This is the size of test dataset
subsetTestSize<-100

ietrain<-ie[1:subsetTrainSize,]
songtrain<-song[1:subsetTrainSize,]

ieusers<-which(is.element(ie$userID[-c(1:subsetTrainSize)],ietrain$userID)) + subsetTrainSize
ieitems<-which(is.element(ie$itemID[-c(1:subsetTrainSize)],ietrain$itemID)) + subsetTrainSize
ietest<-ieusers[which(is.element(ieusers,ieitems))]
ietest<-ie[ietest,]
ietest<-ietest[1:subsetTestSize,]

songusers<-which(is.element(song$userID[-c(1:subsetTrainSize)],songtrain$userID)) + subsetTrainSize
songitems<-which(is.element(song$itemID[-c(1:subsetTrainSize)],songtrain$itemID)) + subsetTrainSize
songtest<-songusers[which(is.element(songusers,songitems))]
songtest<-song[songtest,]
songtest<-songtest[1:subsetTestSize,]

# Create the test data
#sampleIndex<-sample(1:nrow(ie),100)
#userID<-ie$userID[sampleIndex]
#sampleIndex<-sample(1:nrow(ie),100)
#itemID<-ie$itemID[sampleIndex]
#newXs<-data.frame(userID,itemID)

################################################################################################
#Below are an example to call the ratingProbsFit and its prediction function

#CART
probsFitOutCART<-ratingProbsFit(ietrain, 5, "CART", FALSE, NULL)
predsCART<-predict(probsFitOutCART,ietest)
predsCART<-probs_to_rating(predsCART)
mapeCART<-mape(ietest$rating,predsCART)
head(predsCART)

# kNN
# Cache : a necessary parameter, a unique name for the dataset you are using
# NC : the number of clusters to use for the KNN. 100 seems to be the best
probsFitOutKNN<-ratingProbsFit(ietrain, 5, "kNN", TRUE, list(cache="ie", nc=100))
predsKNN<-predict(probsFitOutKNN, ietest)
predsKNN<-probs_to_rating(predsKNN)
mapeKNN<-mape(ietest$rating,predsKNN)
head(predsKNN)

# NFM
# Dim : Latent space for NMF
# Bias : A value between 0 and 0.5 | The threshold for "considering" a vote
# Forest size : How many "forests" of NMF's to create. The higher the number, the more fine-grained the probabilities will be 
probsFitOutNMF<-ratingProbsFit(ietrain, 5, "NMF", TRUE, list(dim=100, bias=0.1, forest_size=4)) # May take like 3 minutes to compute
predsNMF<-predict(probsFitOutNMF, ietest)
predsNMF<-probs_to_rating(predsNMF)
mapeNMF<-mape(ietest$rating,predsNMF)
head(predsNMF)

# Logit
# Glmcov: this argument is "userID" or "itemID". User select which column to use as a covariate
probsFitOutLogit<-ratingProbsFit(ietrain, 5, "logit", FALSE, list(glmcov="userID"))
predsLogit<-predict(probsFitOutLogit, ietest)
predsLogit<-probs_to_rating(predsLogit)
mapeLogit<-mape(ietest$rating,predsLogit)
head(predsLogit)

################################################################################################

mapeKNN
mapeCART
mapeNMF
mapeLogit

