CART <- function() {
  
  library(partykit)
  library(lme4)
  data(InstEval)
  all <- InstEval
  
  all <- all[,c('s', 'd', 'y')]
  colnames(all) <- c("Student", "Professor", "Rating")
  head(all)
  #str(ie)
  #all of the columns are factors besides the "y" column
  #y column is the ratings column
  
  tree <- ctree( Rating ~ Student + Professor, data = all)
  temp_data <- data.frame(Student = factor(1), Professor = factor(1002))
  #str(temp_data)
  
  output <- predict(tree, temp_data)
  
}

print(CART())