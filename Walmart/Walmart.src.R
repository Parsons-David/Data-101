# David Parsons
# Data 101 - Rutgers University
# 198:142:01

library(ggplot2)
install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
options(scipen=999)

ands <- data.frame(cols)

allCols <- c()
for(c in cols){
  newCol <- c()
  for(x in cols){
    subwal <- subset(wal, wal[,c] == 1)
    subwal <- subset(subwal, subwal[,x] == 1)
    # print(c(x, " & ", c, " -> ", nrow(subwal)))
    newCol <- c(newCol, nrow(subwal))
  }
  # print(newCol)
  ands[, c] <- newCol
  allCols <- c(allCols, newCol)
}

probs <- data.frame(cols)


getChange <- function(x, c, frame){
  subframe <- subset(frame, frame[,c] == 1)
  subframe <- subset(subframe, subframe[,x] == 1)
  # print(c(x, " & ", c, " -> ", nrow(subframe)))
  tot <- nrow(frame)
  # P(X & C)
  numBoth <- nrow(subframe)
  probBoth <- 100 * (numBoth / tot)
  # P(C)
  numC <- nrow(subset(frame, frame[,c] == 1))
  probC <- 100 * (numC / tot)
  # P(X | C)
  probXgC <- 100 * (probBoth / probC)
  # P(X)
  numX <- nrow(subset(frame, frame[,x] == 1))
  probX <- 100 * (numX / tot)
  # Change P(X | C) - P(X)
  change = probXgC - probX
  return(change)
}

allCols <- c()
for(c in cols){
  newCol <- c()
  for(x in cols){
    change <- getChange(x, c, wal)
    newCol <- c(newCol, change)
  }
  # print(newCol)
  probs[, c] <- newCol
  allCols <- c(allCols, newCol)
}
pairs <- ""
for(c in cols){
  for(x in cols){
    if(x != c){
      change <- round(getChange(x, c, wal), digits = 2)
      if(change > 1){
        pVal <- PermutationTestSecond::Permutation(wal, c, x,10000,0, 1)
        pairs <- c(pairs, c(x, c))
        incDec <- if(change > 0) "increase" else "decrease"
        cat(paste(change, "% ", incDec," in probability of buying ", x, " if they bought ", c, "\n\tWith a P-Value of: ", pVal, "\n"))
      }
    }
  }
}