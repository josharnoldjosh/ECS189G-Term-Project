mape <- function(y, y_hat) {
  return (mean(abs(y-y_hat)))
}