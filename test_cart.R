#create the models for logit, NMF, kNN, and CART
#the input dataset must have only 3 columns "userID", "itemID", "rating"
#return a recProbs object and users can predict the probabilities with that object
ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
  
  #read the predMethods files
  source("./eval.R")
  
  #load library
  library(rectools)
  
  #check if the dataIn is correct or not
  if(length(dataIn) != 3){
    stop("Error: dataIn must have 3 columns")
  }
  if(names(dataIn)[1] != "userID" || names(dataIn)[2] != "itemID" || names(dataIn)[3] != "rating"){
    stop("Error: 3 columns must be named as \"userID\",\"itemID\",\"rating\"")
  }
  if(!is.factor(dataIn[,1]) || !is.factor(dataIn[,1])){
    stop("Error: userID and itemID must be factor")
  }
  
  #if embedMeans = TRUE => embed the dataset
  #   embedMeans = FALSE => do nothing
  # if(embedMeans){
  #     if(predMethod == "NMF"){
  #         stop("Error: embedding is not valid for NMF")
  #     }
  #     dataIn <- ratingEmbed(dataIn,maxRating)
  #     print(head(dataIn))
  # }
  
  #create a recProbs object based on the predMethod
  probsFitOut<-switch(predMethod,
                      "logit" = ratingGlm(dataIn,maxRating,embedMeans,specialArgs),
                      "NMF" = print("NMF called"),
                      "kNN" = print("kNN called"),
                      "CART" = ratingCART(dataIn,maxRating,embedMeans,specialArgs),
                      stop("Error: predMethod is incorrect")
  )
  
  #return the recProbs object
  return(probsFitOut)
  
}

#predict the probabilities from a recProbs object
#the test data is not allowed to have any new users and items
predict.recProbs <- function(probsFitOut,newXs){
  
  #check if there are new users or items
  newUser<-setdiff(newXs$userID,probsFitOut$userList)
  newItem<-setdiff(newXs$itemID,probsFitOut$itemList)
  if(length(newUser) != 0 || length(newItem) != 0){
    stop("Error: there are new users and items")
  }
  
  #calculate the probability matrix
  preds<-switch(probsFitOut$predMethod,
                "logit" = logitPredict(probsFitOut,newXs),
                "NMF" = print("NMF predict"),
                "kNN" = print("kNN predict"),
                "CART" = CARTPredict(probsFitOut,newXs),
                stop("Error: predMethod is incorrect")
  )
  
  #return the probability matrix
  return(preds)
}

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
    rplist<-as.list(NULL)
    for(i in 1:maxRating){
      inputdata<-data.frame(embedData[,1],embedData[,i+1])
      names(inputdata)<-c("userID","embedProb")
      rpout<-rpart(embedProb~userID,data=inputdata,method="anova",control=rpart.control(minsplit=10,cp=0.001))
      #prune the tree
      #prunedrpout<-prune(rpout,cp=rpout$cptable[which.min(rpout$cptable[,4]),1])
      
      #rplist[[i]]<-prunedrpout
      rplist[[i]]<-rpout
    }
    
    
  }else{
    rplist<-as.list(NULL)
    for(i in 1:maxRating){
      dummyrating<-as.integer(dataIn$rating==i)
      dataInDummy<-data.frame(dataIn$userID,dataIn$itemID,dummyrating)
      names(dataInDummy)<-c("userID","itemID","dummyrating")
      print(specialArgs$minsplit)
      rpout<-rpart(dummyrating~userID+itemID,data=dataInDummy,method="poisson",control=rpart.control(minsplit=specialArgs$minsplit,cp=0.001))
      #prune the tree
      prunedrpout<-prune(rpout,cp=rpout$cptable[which.min(rpout$cptable[,4]),1])
      print(prunedrpout)
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
  formUserData(dataIn, fileOut = "./data.RData")
  
  load("./data.RData")
  fud<-retval
  
  #make the userID column
  userID<-c(1:length(fud))
  userID<-as.factor(userID)
  
  #make the probability column
  probs<-NULL
  for(i in 1:length(fud)){
    row<-c()
    for(j in 1:maxRating){
      prob<-sum(fud[[i]]$rating==j)/length(fud[[i]]$rating)
      row<-c(row,prob)
    }
    probs<-rbind(probs,row)
  }
  
  #make the embedded dataframe
  dataIn<-data.frame(userID,probs)
  colnames(dataIn)<-c("userID",1:maxRating)
  rownames(dataIn)<-c()
  return(dataIn)
}
