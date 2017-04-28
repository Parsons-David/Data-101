# David Parsons
# Data 101 - Rutgers University
# 198:142:01

# Setup

# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("e1071")
install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")
library(rpart)
library(rpart.plot)


input <- read.csv("P5_train1.csv")
# test <- read.csv("Marriage_testing_students1.csv")
partitionData <- function(){
  samp.total <- sample(nrow(input), 4488, replace = FALSE)
  samp.train <- sample(nrow(input), 1795, replace = FALSE)
  samp.test <- sample(nrow(input[setdiff(samp.total, samp.train),]), 2244, replace = FALSE)
  
  train <- input[samp.train,] # random 40% of data
  test <- input[samp.test,] # random 50% of data
}
print(summary(input))

MSE <- 100
for(j in c(3:17)){
  MSE <- 100
  tree <- rpart(Revenue ~ ., control = rpart.control(minsplit = 5, cp = 0.05), data = input)
  for(i in c(1:1000)){
    samp.total <- sample(nrow(input), 4488, replace = FALSE)
    samp.train <- sample(nrow(input), (4488 * (j/20)), replace = FALSE)
    samp.test <- sample(nrow(input[setdiff(samp.total, samp.train),]), (4488 * ((18 - j + 1)/20)), replace = FALSE)
    
    train <- input[samp.train,] # random 40% of data
    test <- input[samp.test,] # random 50% of data
    
    tree.lm <- lm(Revenue ~ ., data = train)
    
    # Creating Prediction Tree
    tmpTree <- rpart(Revenue ~ ., control = rpart.control(minsplit = 5, cp = 0.1), data = train)
    
    # summary(tree)
    # rpart.plot(tree)
    
    outVec <- predict(tree.lm, newdata=test)
    # mean(test$Revenue == test.tree)
    
    tmpMSE <- mean((outVec - test$Revenue)^2)
    
    if(tmpMSE < MSE){
      MSE <- tmpMSE
      tree <- tmpTree
    }
    
  }
  print(paste("Train:", (j/20), "Test:", ((18 - j)/20), " -> ", MSE))
  rpart.plot(tree)
}
# print(summary(tree))


MSE <- 100

tree.lm <- lm(Revenue ~ ., data = input)
for(i in c(1:10000)){
  samp.total <- sample(nrow(input), 4950, replace = FALSE)
  samp.train <- sample(nrow(input), (4950 * 0.65), replace = FALSE)
  samp.test <- sample(nrow(input[setdiff(samp.total, samp.train),]), (4950 * 0.25), replace = FALSE)
  
  train <- input[samp.train,] # random 40% of data
  test <- input[samp.test,] # random 50% of data
  
  tmpTree.lm <- lm(Revenue ~ ., data = train)
  
  # summary(tree)
  # rpart.plot(tree)
  
  outVec <- predict(tmpTree.lm, newdata=test)
  # mean(test$Revenue == test.tree)
  
  tmpMSE <- mean((outVec - test$Revenue)^2)
  
  if(tmpMSE < MSE){
    MSE <- tmpMSE
    tree.lm <- tmpTree.lm
  }
  
}
print(MSE)

# Validation Testing
# CrossValidation::cross_validate(test, tree.lm, 1000, 0.5)

# Predicting on Real Data
data <- read.csv("P%_test_students.csv")
finalVec <- predict(tree.lm, newdata = data)

# To File
submission<-read.csv("P5_sample.csv")
submission$Revenue <- finalVec
write.csv(submission, 'submission.csv', row.names= FALSE)