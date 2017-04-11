# David Parsons
# Data 101 - Rutgers University
# 198:142:01

knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
options(scipen=999)

train <- read.csv("M2017_train.csv")

train[,7] <- ""
colnames(train)[7] <- "GRADELEVEL"
train$GRADELEVEL <- as.numeric(0)
train$GRADELEVEL[train$GRADE == "A"] <- as.numeric(5) 
train$GRADELEVEL[train$GRADE == "B"] <- as.numeric(4) 
train$GRADELEVEL[train$GRADE == "C"] <- as.numeric(3) 
train$GRADELEVEL[train$GRADE == "D"] <- as.numeric(2) 
train$GRADELEVEL[train$GRADE == "F"] <- as.numeric(1) 

train[,8] <- ""
colnames(train)[8] <- "SCORELEVEL"
train$SCORELEVEL <- cut(train$SCORE, 10, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

train[,9] <- ""
colnames(train)[9] <- "IDLEVEL"
train$IDLEVEL <- cut(train$STUDENTID, 10, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

vars = colnames(train)

summary(train)


scatPlot <- function(x, y, frm){
  
  title = paste(x,y, sep = " vs ")
  plot(frm[,x], frm[,y], col = rainbow(8), main = title, las = 2, xlab = x, ylab = y)
}

heatPlot <- function(frm, yAxis, xAxis, heat) {
  subCen <- subset(frm)
  print(ggplot(data = subCen, aes(x = subCen[,xAxis], y = subCen[,yAxis]), las = 2)+ labs(x = xAxis, y = yAxis, las = 2) + geom_tile(aes(fill = subCen[,heat]))+
          scale_fill_gradient2(low="darkblue", high="red", guide="colorbar") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

t <- subset(train, train$GRADE == "D")

for(v in vars){
  print(v) 
  barplot(height=table(t[,v]))
}

t <- train

c <- "SCORELEVEL"
for(v in vars){
  if(c != v){
    scatPlot(c, v, t)
  }
}

c <- "SCORELEVEL"
for(v in vars){
  # if(c != v){
  heatPlot(t, c, v, "GRADELEVEL")
  # }
}



train[,10] <- ""
colnames(train)[10] <- "FSCORE"
train$FSCORE <- 0

train$FSCORE[train$PARTICIPATION > 0.5] <- (train$FSCORE[train$PARTICIPATION > 0.5]) + 2

train$FSCORE[train$ASKS_QUESTIONS == "always"] <- (train$FSCORE[train$ASKS_QUESTIONS == "always"]) + 1

train$FSCORE[train$LEAVES_EARLY == "never"] <- (train$FSCORE[train$LEAVES_EARLY == "never"]) + 1

train$FSCORE[train$SCORE > 80] <- train$FSCORE[train$SCORE > 80] + 3
train$FSCORE[train$SCORE > 60] <- train$FSCORE[train$SCORE > 60] + 2
train$FSCORE[train$SCORE > 40] <- train$FSCORE[train$SCORE > 40] + 1
train$FSCORE[train$SCORE > 20] <- train$FSCORE[train$SCORE > 20] + 2

train[,11] <- ""
colnames(train)[11] <- "NGRADE"
train$NGRADE <- "F"
train$NGRADE[train$FSCORE > 2] <- "D"
train$NGRADE[train$FSCORE > 4] <- "C"
train$NGRADE[train$FSCORE > 6] <- "B"
train$NGRADE[train$FSCORE > 8] <- "A"

train[,12] <- ""
colnames(train)[12] <- "SAME"
train$SAME <- "False"
train$SAME[train$GRADE == train$NGRADE] <- "True"


out <- read.csv("M2017_test_students.csv")

out[,6] <- ""
colnames(out)[6] <- "FSCORE"
out$FSCORE <- 0

out$FSCORE[out$PARTICIPATION > 0.5] <- (out$FSCORE[out$PARTICIPATION > 0.5]) + 2

out$FSCORE[out$ASKS_QUESTIONS == "always"] <- (out$FSCORE[out$ASKS_QUESTIONS == "always"]) + 1

out$FSCORE[out$LEAVES_EARLY == "never"] <- (out$FSCORE[out$LEAVES_EARLY == "never"]) + 1

out$FSCORE[out$SCORE > 80] <- out$FSCORE[out$SCORE > 80] + 3
out$FSCORE[out$SCORE > 60] <- out$FSCORE[out$SCORE > 60] + 2
out$FSCORE[out$SCORE > 40] <- out$FSCORE[out$SCORE > 40] + 1
out$FSCORE[out$SCORE > 20] <- out$FSCORE[out$SCORE > 20] + 2


out[,7] <- ""
colnames(out)[7] <- "GRADE"
out$GRADE <- "F"
out$GRADE[out$FSCORE > 2] <- "D"
out$GRADE[out$FSCORE > 4] <- "C"
out$GRADE[out$FSCORE > 6] <- "B"
out$GRADE[out$FSCORE > 8] <- "A"

# TO File

submission<-read.csv("M2017_sample_submission.csv")
submission$GRADE<-out$GRADE
write.csv(submission, 'submission.csv', row.names= FALSE)