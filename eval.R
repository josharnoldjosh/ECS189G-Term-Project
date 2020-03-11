mape <- function(y, y_hat) {
  return (mean(abs(y-y_hat)))
}

votes_to_prob <- function(votes) {
  
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
    return (round(probs[2, ], digit=3))
  }
  
  result<-apply(votes, 1, get_probs)
  result <- t(result)
  return(result)
}

