
census <- read.csv("CENSUSNEW.csv")
summary(census)

# Creates Dictionary of Numerics
nums <- c("AGE", "YEARS", "CAPITALNET")
# Creates Dictionary of Elements
elems <- levels(census$ELEMENTS)
# Non Element cats
cats <- c("AGE", "STATUS", "EDUCATION", "PROFESSION", "NATIVE")
# Creates Net Capital Variable
census$CAPITALNET = census$CAPITALGAINS - census$CAPITALLOSS
# Summary with CAPITALNET
summary(census)


bplotAll <- function(subCat){
  
  subCen <- census
  
  if(subCat == "NATIVE"){
    subCen <- subset(subCen, NATIVE != "United-States")
  }
  
  tmpTab <- table(subCen$ELEMENTS, subCen[,subCat])
  
  barplot(tmpTab, main=c("Elements vs ", subCat), xlab = subCat, ylab = "Count", col = rainbow(4), las = 2, cex.names = 0.75, beside = FALSE, horiz = FALSE)
}

for(cat in cats){
  bplotAll(cat)
}

### NATIVE Plot With US

barplot(tmpTab <- table(census$ELEMENTS, census$NATIVE), main="Elements vs NATIVE (w/US)", xlab = "NATIVE", ylab = "Count", col = rainbow(4), las = 2, cex.names = 0.75, beside = FALSE, horiz = FALSE)

plotBar <- function(e) {
  subCen <- subset(census, ELEMENTS == e)# & NATIVE != "India")
  
  subCen.meanCapG <- tapply(subCen$CAPITALGAINS, subCen$NATIVE, mean)
  barplot(subCen.meanCapG, las = 2, main=c(e," Captial Gains by Origin Country"), xlab = "Country", ylab = "Gains", col = rainbow(6), las = 2)
  
  subCen.meanCapL <- tapply(subCen$CAPITALLOSS, subCen$NATIVE, mean)
  barplot(subCen.meanCapL, las = 2, main=c(e," Captial Loss by Origin Country"), xlab = "Country", ylab = "Loss", col = rainbow(6), las = 2)
  
  subCen$CAPITALNET <- (subCen$CAPITALGAINS - subCen$CAPITALLOSS)
  subCen.meanNet <- tapply(subCen$CAPITALNET, subCen$NATIVE, mean)
  barplot(subCen.meanNet, las = 2, main=c(e," Net Capital by Origin Country"), xlab = "Country", ylab = "Net", col = rainbow(6), las = 2)
  
}

for (elem in elems){
  plotBar(elem)
}


plotScat <- function(e){
  subCen <- subset(census, NATIVE == e)
  
  subCen$CAPITALNET <- (subCen$CAPITALGAINS - subCen$CAPITALLOSS)
  plot(subCen$CAPITALNET, subCen$AGE, col = rainbow(8), main = c("Net Capital vs Age", e), las = 2, xlab = "Net Capital", ylab = "Age")
  
}

for(country in levels(census$NATIVE)){
  plotScat(country)
}

heatPlot <- function(yAxis, xAxis, heat) {
  subCen <- subset(census)#, CAPITALNET > 200000)
  # print(summary(tapply(subCen[,yAxis])))
  print(ggplot(data = subCen, aes(x = subCen[,xAxis], y = subCen[,yAxis]), las = 2)+ labs(x = xAxis, y = yAxis, las = 2) + geom_tile(aes(fill = census[,heat]))+
          scale_fill_gradient2(low="darkblue", high="red", guide="colorbar") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}
for(var in cats){
  for(val in nums){
    for(fin in c("ELEMENTS")){
      heatPlot(var, fin, val)
    }
  }
}
for(var in cats){
  for(val in nums){
    for(fin in cats){
      heatPlot(var, fin, val)
    }
  }
}
