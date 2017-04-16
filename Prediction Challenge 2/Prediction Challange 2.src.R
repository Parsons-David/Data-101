# David Parsons
# Data 101 - Rutgers University
# 198:142:01

library(rpart)
library(rpart.plot)
options(scipen=999)

train <- read.csv("M2017_train.csv")
out <- read.csv("M2017_test_students.csv")

print(summary(train))

predict <- rpart(GRADE ~ SCORE+ASKS_QUESTIONS+LEAVES_EARLY+PARTICIPATION,data=train, minsplit = 50, cp=0.03)
summary(predict)

rpart.plot(predict)

test.tree <- predict(predict,newdata=train, type="class")

mean(train$GRADE == test.tree)

out.tree <- predict(predict,newdata=out, type="class")

submission<-read.csv("M2017_sample_submission.csv")
submission$GRADE<-out.tree
write.csv(submission, 'submission.csv', row.names= FALSE)