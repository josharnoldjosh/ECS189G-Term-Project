is_formatted_correctly <- function(data) {
  
  # Check if first two columns are User ID and Item ID respectively
  result <- names(data)[1:3] == c("user_id", "item_id", "y")
  if (all(result)) {
    return (TRUE)  
  }
  
  # Check if first two columns are User ID and Item ID respectively
  result <- names(data)[1:3] == c("user_id", "item_id", "y")
  if (all(result)) {
    return (TRUE)  
  }
  
  return (FALSE)
}