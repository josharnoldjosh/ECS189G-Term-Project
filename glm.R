ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){

    #load library
    library(rectools)

    #check if the dataIn is correct or not
    if(length(dataIn) != 3){
        stop("Error: dataIn must have 3 columns")
    }
    if(names(dataIn)[1] != "userID" || names(dataIn)[2] != "itemID" || names(dataIn)[3] != "rating"){
        stop("Error: 3 columns must be named as \"userID\",\"itemId\",\"rating\"")
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

    switch(predMethod,
    "logit" = ratingGlm(dataIn,maxRating,embedMeans,specialArgs),
    "NMF" = print("NMF called"),
    "kNN" = print("kNN called"),
    "CART" = print("CART called"),
    stop("Error: predMethod is incorrect")
    )


}

#calculate the glm model
#TODO 
#1) how to use embedded dataset to glm?
#2) it takes too much time to calculate glm model
#3) there gonna be too much coefficients i.e. model gonna be overfitted
#4) Is it nonsense to use userID and itemID as covariates?
ratingGlm <- function(dataIn,maxRating,embedMeans,specialArgs){
    print("logit called")
    if(embedMeans){
        #initialize glm list
        glmlist<-as.list(NULL)
        #
        # implement the glm for embedded dataset
        # but how?
        #
    }else{
        #initialize glm list
        glmlist<-as.list(NULL)
        for(i in 1:maxRating){
            #make an dummy raing column
            dummyrating<-as.integer(dataIn$rating==i)

            #make a dataset for glm
            dataInDummy<-data.frame(dummyrating,dataIn[1:2])

            #make glm molel
            glmout<-glm(dummyrating~.,data=dataInDummy,family=binomial)

            #add the glm model into the glm list
            glmlist[[i]]<-glmout
        }
    }
    print(glmlist)
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

library(lme4)

#create InstEval dataset
ie<-InstEval
ie<-ie[,c(1,2,7)]
names(ie)<-c("userID","itemID","rating")

#create songs dataset
#songs<-read.csv("songs.csv",header=T)
#songs[,1]<-as.factor(songs[,1])
#songs[,2]<-as.factor(songs[,2])
#names(songs)<-c("userID","itemID","rating")

ratingProbsFit(ie,5,"logit",FALSE,)

#ratingProbsFit(songs,5,"logit",FALSE,)