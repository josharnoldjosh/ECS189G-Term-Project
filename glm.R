ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){

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
    if(embedMeans){
        if(predMethod == "NMF"){
            stop("Error: embedding is not valid for NMF")
        }
        dataIn <- ratingEmbed(dataIn,maxRating)
        print(head(dataIn))
    }else{
    }

    probsFitOut<-switch(predMethod,
    "logit" = ratingGlm(dataIn,maxRating,embedMeans,specialArgs),
    "NMF" = print("NMF called"),
    "kNN" = print("kNN called"),
    "CART" = print("CART called"),
    stop("Error: predMethod is incorrect")
    )

    return(probsFitOut)

}

predict.recProbs <- function(probsFitOut,newXs){

    #check if there are new users or items
    newUser<-setdiff(newXs$userID,probsFitOut$userList)
    newItem<-setdiff(newXs$itemID,probsFitOut$itemList)
    if(length(newUser) != 0 || length(newItem) != 0){
        stop("Error: there are new users and items")
    }

    preds<-switch(probsFitOut$predMethod,
    "logit" = logitPredict(probsFitOut,newXs),
    "NMF" = print("NMF predict"),
    "kNN" = print("kNN predict"),
    "CART" = print("CART predict"),
    stop("Error: predMethod is incorrect")
    )

    return(preds)
}

#
# This function predict the probability of ratings with glm models
#
logitPredict <- function(probsFitOut,newXs){
    preds<-NULL
    for(i in 1:probsFitOut$maxRating){
        predglm<-predict(probsFitOut$glmList[[i]],newXs, type="response")
        preds<-cbind(preds,predglm)
    }
    colnames(preds)<-c(1:probsFitOut$maxRating)
    return(preds)
}

#calculate the glm model
#TODO 
#1) it takes too much time to calculate glm model
ratingGlm <- function(dataIn,maxRating,embedMeans,specialArgs){
    #initialize glm list
    glmlist<-as.list(NULL)
    for(i in 1:maxRating){
        #make an dummy raing column
        dummyrating<-as.integer(dataIn$rating==i)

        #make a dataset for glm
        dataInDummy<-data.frame(dummyrating,dataIn[1:2])

        #print(dataInDummy)

        #make glm molel
        glmout<-switch(specialArgs$glmcov,
            "userID" = glm(dummyrating~userID,data=dataInDummy,family=binomial),
            "itemID" = glm(dummyrating~itemID,data=dataInDummy,family=binomial),
            "all" = glm(dummyrating~.,data=dataInDummy,family=binomial),
            stop("Error: specialArgs$glmcov must be \"userID\" or \"itemID\" or \"all\"")
        )

        #add the glm model into the glm list
        glmlist[[i]]<-glmout
    }

    #make the userlist
    userList<-droplevels(dataIn$userID)
    userList<-unique(userList)

    #make the itemlist
    itemList<-droplevels(dataIn$itemID)
    itemList<-unique(itemList)

    #initialize the probsFitOut
    probsFitOut <- list(predMethod = "logit", maxRating = maxRating,embedMeans = embedMeans, specialArgs = specialArgs, glmList = glmlist, userList = userList, itemList = itemList)
    class(probsFitOut)<-"recProbs"

    #return the probsFitOut
    return(probsFitOut)
}

#return the embedded dataset
#ratingEmbed <- function(dataIn,maxRating){
#    
#    #call formUserData
#    fud<-formUserData(dataIn)
#
#    #make the userID column
#    userID<-c(1:length(fud))
#    userID<-as.factor(userID)
#
#    #make the meanRating column
#    meanRating<-c()
#    for(i in 1:length(fud)){
#        meanRating<-c(meanRating,mean(fud[[i]]$ratings))
#   }
#
#    #make the embedded dataframe
#    dataIn<-data.frame(userID,meanRating)
#    return(dataIn)
#}

#initialize the dataset
source("./data_loader.R")
datasets<-load_project_data()
ie<-datasets[[1]]
song<-datasets[[2]]
sampleIndex<-sample(1:nrow(ie),300)
ieSample<-ie[sampleIndex,]
#ieSample<-InstEval[1:100,]
#ieSample<-ieSample[,c(1,2,7)]
#names(ieSample)<-c("userID","itemID","rating")
#print(ieSample)

#sa is Special Args
sa<-list()
#userID is the covariate
sa$glmcov<-"userID"
#itemID is the covariate
#sa$glmcov<-"itemID"

probsFitOut <- ratingProbsFit(ieSample,5,"logit",FALSE,sa)
#probsFitOut<-ratingProbsFit(songs,5,"logit",FALSE,)

#reorder randomly the userID and itemID in dataIn
sampleIndex<-sample(1:100,100)
userID<-ieSample$userID[sampleIndex]
sampleIndex<-sample(1:100,100)
itemID<-ieSample$itemID[sampleIndex]

#make newXs. there are no new users and items
newXs<-data.frame(userID,itemID)
preds<-predict(probsFitOut,newXs)
#print(round(preds,digit=3))
