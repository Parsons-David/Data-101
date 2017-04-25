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


# Data Import
input <- read.csv("Marriage_training.csv")
test <- read.csv("Marriage_testing_students1.csv")

print(summary(train))


# Prediction

# Partition Data
train <- input[1:349,]
val <- input[350:750,]

# Creating Prediction Tree
tree <- rpart(STATUS ~ (Joint.Income+(GroomPersonality==BridePersonality)+(GroomAge<BrideAge)+(GroomAge==BrideAge)), control = rpart.control(minsplit = 5), data = train)

summary(tree)
rpart.plot(tree)

test.tree <- predict(tree, newdata=val, type="class")
mean(val$STATUS == test.tree)

# Validation Testing
CrossValidation::cross_validate(val, tree, 1000, 0.5)

# Prediction with test data
test_predict <- predict(tree, newdata = test, type = "class")
summary(test_predict)

# Writing Submission To File
submission<-read.csv("Marriage_sample.csv")
submission$STATUS<-test_predict
write.csv(submission, 'submission.csv', row.names= FALSE)
