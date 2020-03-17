#This is a file to test the TermProject.R 

# Source the necessary files
source("./TermProject.R")
source("./data_loader.R")

# Load the datasets(InstEval, songs)
datasets<-load_project_data()
ie<-datasets[[1]]
song<-datasets[[2]]

# Create the test data
sampleIndex<-sample(1:nrow(ie),100)
userID<-ie$userID[sampleIndex]
sampleIndex<-sample(1:nrow(ie),100)
itemID<-ie$itemID[sampleIndex]
newXs<-data.frame(userID,itemID)

################################################################################################

#CART
probsFitOutCART<-ratingProbsFit(ie, 5, "CART", TRUE, NULL)
predsCART<-predict(probsFitOutCART,newXs)
head(predsCART)

# kNN
# Cache : a necessary parameter, a unique name for the dataset you are using
# NC : the number of clusters to use for the KNN. 100 seems to be the best
probsFitOutKNN<-ratingProbsFit(ie, 5, "kNN", TRUE, list(cache="ie", nc=100))
predsKNN<-predict(probsFitOutKNN, newXs)
head(predsKNN)

# NFM
# Dim : Latent space for NMF
# Bias : A value between 0 and 0.5 | The threshold for "considering" a vote
# Forest size : How many "forests" of NMF's to create. The higher the number, the more fine-grained the probabilities will be 
probsFitOutNMF<-ratingProbsFit(ie, 5, "NMF", TRUE, list(dim=100, bias=0.375, forest_size=3)) # May take like 3 minutes to compute
predsNMF<-predict(probsFitOutNMF, newXs)
head(predsNMF)

# Add Logit
# ...

################################################################################################