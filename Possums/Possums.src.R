# David Parsons
# Data 101 - Rutgers University
# 198:142:01


knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
options(scipen=999)

pos <- read.csv("possum.csv")
summary(pos)

cols <- colnames(pos)

scatPlot <- function(x, y, frm){
  
  title = paste(x,y, sep = " vs ")
  plot(frm[,x], frm[,y], col = rainbow(8), main = title, las = 2, xlab = x, ylab = y)
}

nos <- c("X", "case", "Pop", "site")
for(x in cols){
  if(x %in% nos){
    next
  }
  for(y in cols){
    if(y %in% nos){
      next
    }
    if(y != x){
      scatPlot(x, y, pos)
    }
  }
}

commons <- c("belly", "chest", "footlgth", "totlngth", "skullw", "hdlngth")

x = "belly"

for(y in commons){
  if(x != y){
    scatPlot(x, y, pos)
  }
}

x = "earconch"

for(y in cols){
  if(x != y && y != "X" && y != "case" && y != "site" && y != "Pop"){
    scatPlot(x, y, pos)
  }
}

scatPlot("earconch", "site", pos)

x = "age"

for(y in c("hdlngth", "skullw", "chest")){
  scatPlot(y, x, pos)
}

x = "sex"

for(y in commons){
  if(x != y && y != "X" && y != "case" && y != "site" && y != "Pop"){
    scatPlot(x, y, pos)
  }
}

pos[, 16] <- ""
colnames(pos)[16] <- "isFemale"
pos$isFemale <- (pos$sex == "f")

for(y in commons){
  if(y != "X" && y != "case" && y != "site" && y != "Pop" && y != "footlgth"){
    print(PermutationTestSecond::Permutation(pos, "isFemale", y, 50000, FALSE, TRUE))
  }
}