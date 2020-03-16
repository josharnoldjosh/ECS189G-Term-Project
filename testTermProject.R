#This is a file to test the TermProject.R 

#load the necessary files
source("./TermProject.R")
source("./data_loader.R")

#load the datasets(InstEval, songs)
datasets<-load_project_data()
ie<-datasets[[1]]
song<-datasets[[2]]

##sampleIndex<-sample(1:nrow(ie),300)
##ieSample<-ie[sampleIndex,]
#ieSample<-InstEval[1:500,]
#ieSample<-ieSample[,c(1,2,7)]
#names(ieSample)<-c("userID","itemID","rating")

#create the test data
sampleIndex<-sample(1:nrow(ie),100)
userID<-ie$userID[sampleIndex]
sampleIndex<-sample(1:nrow(ie),100)
itemID<-ie$itemID[sampleIndex]
newXs<-data.frame(userID,itemID)


#CART
probsFitOutCART<-ratingProbsFit(ie,5,"CART",TRUE,NULL)
predsCART<-predict(probsFitOutCART,newXs)


#Logit
#sa<-list()
##userID is the covariate
#sa$glmcov<-"userID"
##itemID is the covariate
##sa$glmcov<-"itemID"
#probsFitOutLogit <- ratingProbsFit(ie,5,"logit",FALSE,sa)
#predsLogit<-predict(probsFitOutLogit,newXs)
