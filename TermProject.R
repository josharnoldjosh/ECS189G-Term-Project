
#create the models for logit, NMF, kNN, and CART
#the input dataset must have only 3 columns "userID", "itemID", "rating"
#return a recProbs object and users can predict the probabilities with that object
ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){

    #read the predMethods files
    source("./CART.R")
    source("./glm.R")
    source("./knn.R")
    source("./eval.R")

    # Load library
    library(rectools)
  
    # Use our LoadData function
    dataIn <- load_data(dataIn)

    # Check if the dataIn is correct or not
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
      "kNN" = rating_kNN(dataIn, specialArgs),
      "CART" = ratingCART(dataIn,maxRating,embedMeans,specialArgs),
      stop("Error: predMethod is incorrect")
    )

    #return the recProbs object
    return(probsFitOut)
}

#predict the probabilities from a recProbs object
#the test data is not allowed to have any new users and items
predict.recProbs <- function(probsFitOut, newXs){
  
    #read the predMethods files
    source("./CART.R")
    source("./glm.R")
    source("./knn.R")
    source("./eval.R")

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
      "kNN" = kNN_predict(probsFitOut, newXs),
      "CART" = CARTPredict(probsFitOut,newXs),
      stop("Error: predMethod is incorrect")
    )

    #return the probability matrix
    return(preds)
}