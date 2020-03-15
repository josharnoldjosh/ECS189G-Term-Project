# ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){

#     #load library
#     #library(rectools)

#     #check if the dataIn is correct or not
#     if(length(dataIn) != 3){
#         stop("Error: dataIn must have 3 columns")
#     }
#     if(names(dataIn)[1] != "userID" || names(dataIn)[2] != "itemID" || names(dataIn)[3] != "rating"){
#         stop("Error: 3 columns must be named as \"userID\",\"itemID\",\"rating\"")
#     }
#     if(!is.factor(dataIn[,1]) || !is.factor(dataIn[,1])){
#         stop("Error: userID and itemID must be factor")
#     }

#     #if embedMeans = TRUE => embed the dataset
#     #   embedMeans = FALSE => do nothing
#     if(embedMeans){
#         if(predMethod == "NMF"){
#             stop("Error: embedding is not valid for NMF")
#         }
#         dataIn <- ratingEmbed(dataIn,maxRating)
#         print(head(dataIn))
#     }

#     probsFitOut<-switch(predMethod,
#     "logit" = print("logit called"),
#     "NMF" = print("NMF called"),
#     "kNN" = print("kNN called"),
#     "CART" = ratingCART(dataIn,maxRating,embedMeans,specialArgs),
#     stop("Error: predMethod is incorrect")
#     )

#     return(probsFitOut)

# }

# predict.recProbs <- function(probsFitOut,newXs){

#     #check if there are new users or items
#     newUser<-setdiff(newXs$userID,probsFitOut$userList)
#     newItem<-setdiff(newXs$itemID,probsFitOut$itemList)
#     if(length(newUser) != 0 || length(newItem) != 0){
#         stop("Error: there are new users and items")
#     }

#     preds<-switch(probsFitOut$predMethod,
#     "logit" = print("logit predict"),
#     "NMF" = print("NMF predict"),
#     "kNN" = print("kNN predict"),
#     "CART" = CARTPredict(probsFitOut,newXs),
#     stop("Error: predMethod is incorrect")
#     )

#     return(preds)
# }

CARTPredict <- function(probsFitOut,newXs){
  #preds<-predict(probsFitOut$rpout,newXs)
  
  preds<-NULL
  for(i in 1:probsFitOut$maxRating){
    predrp<-predict(probsFitOut$rplist[[i]],newXs)
    preds<-cbind(preds,predrp)
  }
  colnames(preds)<-c(1:probsFitOut$maxRating)
  for(i in 1:nrow(preds)){
    preds[i,]<-preds[i,]/sum(preds[i,])
  }
  return(preds)
}

#TODO
#1)implement embedmeans
#3)find the best parameter(minsplit)
ratingCART <- function(dataIn,maxRating,embedMeans,specialArgs) {
  print("CART called")
  #library(partykit)
  library(rpart)
  
  if(embedMeans){
    
    embedData<-ratingEmbed(dataIn, maxRating)
    
    
  }else{
    rplist<-as.list(NULL)
    for(i in 1:maxRating){
      dummyrating<-as.integer(dataIn$rating==i)
      dataInDummy<-data.frame(dataIn$userID,dataIn$itemID,dummyrating)
      names(dataInDummy)<-c("userID","itemID","dummyrating")
      
      rpout<-rpart(dummyrating~userID+itemID,data=dataInDummy,method="poisson",control=rpart.control(minsplit=10,cp=0.001))
      #prune the tree
      prunedrpout<-prune(rpout,cp=rpout$cptable[which.min(rpout$cptable[,4]),1])
      
      rplist[[i]]<-prunedrpout
    }
  }

  #make the userlist
  userList<-droplevels(dataIn$userID)
  userList<-unique(userList)

  #make the itemlist
  itemList<-droplevels(dataIn$itemID)
  itemList<-unique(itemList)

  #initialize the probsFitOut
  probsFitOut<-list(predMethod = "CART",maxRating = maxRating, embedMeans = embedMeans, specialArgs = specialArgs, rplist = rplist, userList = userList, itemList = itemList)
  class(probsFitOut)<-"recProbs"

  #return the probsFitOut
  return(probsFitOut)
}

#return the embedded dataset
ratingEmbed <- function(dataIn,maxRating){
    
    #call formUserData
    fud<-formUserData(dataIn)

    #make the userID column
    userID<-c(1:length(fud))
    userID<-as.factor(userID)

    #make the meanRating column
    meanRating<-c()
    for(i in 1:length(fud)){
        meanRating<-c(meanRating,mean(fud[[i]]$ratings))
   }

    #make the embedded dataframe
    dataIn<-data.frame(userID,meanRating)
    return(dataIn)
}

#source("./data_loader.R")
#datasets<-load_project_data()
#ie<-datasets[[1]]
#song<-datasets[[2]]

##sampleIndex<-sample(1:nrow(ie),500)
##ieSample<-ie[sampleIndex,]

##ieSample<-InstEval[1:500,]
##ieSample<-ieSample[,c(1,2,7)]
##names(ieSample)<-c("userID","itemID","rating")

#probsFitOut<-ratingProbsFit(ie,5,"CART",FALSE,NULL)
##reorder randomly the userID and itemID in dataIn
#sampleIndex<-sample(1:nrow(ie),100)
#userID<-ie$userID[sampleIndex]
#sampleIndex<-sample(1:nrow(ie),100)
#itemID<-ie$itemID[sampleIndex]

##make newXs. there are no new users and items
#newXs<-data.frame(userID,itemID)
#preds<-predict(probsFitOut,newXs)
