mape <- function(y, y_hat) {
  return (mean(abs(y-y_hat)))
}

votes_to_prob <- function(votes, replace_na=0) {
  
  get_counts <- function(row) {
    default<-cbind(
      c(1, 0),
      c(2, 0),
      c(3, 0),
      c(4, 0),
      c(5, 0)
    )
    counts <- table(row)
    default[2, 1] <- counts["1"]
    default[2, 2] <- counts["2"]
    default[2, 3] <- counts["3"]
    default[2, 4] <- counts["4"]
    default[2, 5] <- counts["5"]
    default[is.na(default)] <- 0
    return (default)
  }
  
  counts_to_prob <- function(counts) {
    divisor<-sum(counts[2, ])
    to_divide <- rbind( rep(1, 5), rep(divisor, 5))
    result <- (counts / to_divide)
    return(result)
  }
  
  get_probs<-function(row) {
    counts <- get_counts(row)
    probs <- counts_to_prob(counts)
    probs <- round(probs[2, ], digit=3)
    return (probs)
  }
  
  result<-apply(votes, 1, get_probs)
  result <- t(result)
  
  # Temp
  result[is.na(result)] <- replace_na
  
  return(result)
}

form_look_up_table <- function(test, rating_vec) {
  look_up_table <- cbind(test, votes_to_prob(rating_vec))
  return (look_up_table)
}

#
# This function convert probabilities to rounded expected rating
# preds: this is the output of predict.recProbs
# return value: a data frame that has nrow(preds) rows and 1 columns
# the value is the rounded expected rating
#
probs_to_rating <- function(preds){
  for(i in 1:ncol(preds)){
    preds[,i]<-preds[,i]*i
  }
  result_rating<-apply(preds,1,sum)
  #remove the labels
  names(result_rating)<-NULL
  #round the expected rating
  result_rating<-round(result_rating)
  return(result_rating)
}