#This is a file to calculate the optimal parameter(minsplit) for CART
#and plot the graph

#read the functions
source("test_cart.R")
source("data_loader.R")
source("eval.R")
library(rectools)
library(ggplot2)

#initialize the dataset
datasets<-load_project_data()
ie<-datasets[[1]]
song<-datasets[[2]]

#make the training and test dataset for k fold cross validation
ieKfDataList<-as.list(NULL)
songKfDataList<-as.list(NULL)
for(i in 1:10){
  iekf<-k_fold(ie,i)
  songkf<-k_fold(song,i)
  new<-which(!is.element(iekf$test$userID,iekf$train$userID))
  if(length(new)!=0){
    iekf$test<-iekf$test[-new,]
  }
  new<-which(!is.element(iekf$test$itemID,iekf$train$itemID))
  if(length(new)!=0){
    iekf$test<-iekf$test[-new,]
  }
  
  new<-which(!is.element(songkf$test$userID,songkf$train$userID))
  if(length(new)!=0){
    songkf$test<-songkf$test[-new,]
  }
  new<-which(!is.element(songkf$test$itemID,songkf$train$itemID))
  if(length(new)!=0){
    songkf$test<-songkf$test[-new,]
  }

  #iekf$test<-iekf$test[-which(!is.element(iekf$test$itemID,iekf$train$itemID)),]
  #iekf$test<-iekf$test[is.element(iekf$test$userID,iekf$train$userID) && is.element(iekf$test$itemID,iekf$train$itemID),]
  #songkf$test<-songkf$test[is.element(songkf$test$userID,songkf$train$userID) && is.element(songkf$test$itemID,songkf$train$itemID),]
  ieKfDataList[[i]]<-iekf
  songKfDataList[[i]]<-songkf
}

result<-NULL
for(j in 1:10){
  sa<-list()
  sa$minsplit<-2^j
  #do the k fold cross validation
  mp<-0
  for(i in 1:10){
    print(i)
    #train the model
    probsFitOut<-ratingProbsFit(songKfDataList[[i]]$train,5,"CART",FALSE,sa)
    #test the model
    preds<-predict(probsFitOut,songKfDataList[[i]]$test)
    preds[,2]<-preds[,2]*2
    preds[,3]<-preds[,3]*3
    preds[,4]<-preds[,4]*4
    preds[,5]<-preds[,5]*5
    rating_preds<-apply(preds,1,sum)
    names(rating_preds)<-NULL
    #calculate the mape
    mp<-mp + mape(ieKfDataList[[i]]$test$rating,rating_preds)
    print(mp)
  }
  mp<- mp/10
  print(mp)
  result_row<-c(sa$minsplit,mp)
  result<-rbind(result,result_row)
}
print(result)
result<-data.frame(result)
rownames(result)<-NULL
colnames(result)<-c("minsplit","MAPE")
