library(ggplot2)
options(scipen=999)

hap <- read.csv("HAPPINESS2017.csv")

hap[,8] <- ""
colnames(hap)[8] <- "GEN"
hap$GEN <-  cut(hap$AGE, 5, c("Young", "Adult", "Middle Adult", "Older Adult", "Elderly"))

hap[,9] <- ""
colnames(hap)[9] <- "INCLEVEL"
classes = c("8th", "7th", "6th", "5th", "4th", "3rd", "2nd", "1st")
hap$INCLEVEL <-  cut(hap$INCOME, breaks = c(0, 36000, 47999, 59999, 72000, 84000, 96000, 108000, 120000), classes)


hap[,10] <- ""
colnames(hap)[10] <- "HAPLEVEL"
classes = c("8th", "7th", "6th", "5th", "4th", "3rd", "2nd", "1st")
hap$HAPLEVEL <-  cut(hap$HAPPINESS, breaks = c(0, 3, 4, 5, 6, 7, 8, 9, 10), classes)

summary(hap)

vars = colnames(hap)

scatPlot <- function(x, y, frm){
  
  title = paste(x,y, sep = " vs ")
  plot(frm[,x], frm[,y], col = rainbow(8), main = title, las = 2, xlab = x, ylab = y)
}

scatPlot("AGE", "HAPPINESS", hap)
scatPlot("INCOME", "HAPPINESS", hap)
scatPlot("INCOME", "HAPPINESS", subset(hap, IMMIGRANT != 1))

print(nrow(subset(hap, IMMIGRANT == 1)))

plotBar <- function(lev){
  subHap <- subset(hap, INCLEVEL == lev)# & NATIVE != "India")
  
  subHap.meanHap <- tapply(subHap$HAPPINESS, subHap$GENDER, mean)
  barplot(subHap.meanHap, las = 2, main=" Happiness by Gender", xlab = "Country", ylab = "Gains", col = rainbow(6), las = 2)
}

for(gen in classes){
  plotBar(gen)
}
for(var in vars){
  if(var != "HAPPINESS" & var != "IDN"){
  }
}

hap.meanHapInc <- tapply(hap$HAPPINESS, hap$INCLEVEL, mean)
barplot(hap.meanHapInc, las = 2, main="Happiness by Income Level", xlab = "Income Level", ylab = "Mean Happiness", col = rainbow(6), las = 2)

hap.meanIncHap <- tapply(hap$INCOME, hap$HAPLEVEL, mean)
barplot(hap.meanIncHap, las = 2, main=" Income by Gender", xlab = "Country", ylab = "Gains", col = rainbow(6), las = 2)

hap.meanHapInc <- tapply(hap$HAPPINESS, hap$INCOME, mean)
barplot(hap.meanHapInc, las = 2, main=" Happiness by Gender", xlab = "Country", ylab = "Gains", col = rainbow(6), las = 2)


heatPlot <- function(frm, yAxis, xAxis, heat) {
  subCen <- subset(frm)
  print(ggplot(data = subCen, aes(x = subCen[,xAxis], y = subCen[,yAxis]), las = 2)+ labs(x = xAxis, y = yAxis, las = 2) + geom_tile(aes(fill = subCen[,heat]))+
          scale_fill_gradient2(low="darkblue", high="red", guide="colorbar") + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

# heatPlot("AGE", "GENDER", "HAPPINESS")

for(var in vars){
  for(va in vars){
    if(var == "INCLEVEL" & va != "HAPPINESS" & va != "IDN"){
      heatPlot(hap, var, va, "HAPPINESS")
    }
  }
}


hap.outliers = subset(hap, INCLEVEL != HAPLEVEL)
print(nrow(hap))
print(nrow(hap.outliers))
summary(hap.outliers)

scatoPlot <- function(x, y){
  title = paste(x,y, sep = " vs ")
  plot(hap.outliers[,x], hap.outliers[,y], col = rainbow(8), main = title, las = 2, xlab = x, ylab = y)
}

scatoPlot("AGE", "HAPPINESS")
scatoPlot("INCOME", "HAPPINESS")

plotDistr <- function (val, frm){
  table <- table(frm[,val])
  barplot(table, main=paste(val, " Distribution"), xlab="Count", col = rainbow(8), las = 2)
}

for(vart in vars){
  plotDistr(vart, hap.outliers)
}

imgrts <- subset(hap, IMMIGRANT == 1)

imgrts[,9] <- ""
colnames(imgrts)[9] <- "INCLEVEL"
classes = c(8:1)
imgrts$INCLEVEL <-  cut(imgrts$INCOME, breaks = c(0, 36000, 47999, 59999, 72000, 84000, 96000, 108000, 120000), classes)

imgrts[,10] <- ""
colnames(imgrts)[10] <- "HAPLEVEL"
classes = c(8:1)
imgrts$HAPLEVEL <-  cut(imgrts$HAPPINESS, breaks = c(0, 3, 4, 5, 6, 7, 8, 9, 10), classes)

imgrts[,11] <- ""
colnames(imgrts)[11] <- "FIT"
imgrts$FIT <- (as.numeric(imgrts$HAPLEVEL) == as.numeric(imgrts$INCLEVEL))

plotDistr("FIT", imgrts)

for(vart in vars){
  plotDistr(vart, imgrts)
  # plotDistr(vart, subset(imgrts, FIT == FALSE))
  # plotDistr(vart, subset(imgrts, FIT == TRUE))
}

for(var in vars){
  for(va in vars){
    if(var != "HAPPINESS" & va != "HAPPINESS" & var != "HAPLEVEL" & va != "HAPLEVEL" & var != "IDN" & va != "IDN" & var != va){
      heatPlot(imgrts, var, va, "HAPPINESS")
    }
  }
  scatPlot(var, "HAPPINESS", imgrts)
}


imgrts.age <- tapply(imgrts$HAPPINESS, imgrts$GEN, mean)
barplot(imgrts.age, las = 2, main=" Happiness by Gender", xlab = "Country", ylab = "Gains", col = rainbow(6), las = 2)

# heatPlot(imgrts, "FIT", "INCLEVEL", "HAPPINESS")

