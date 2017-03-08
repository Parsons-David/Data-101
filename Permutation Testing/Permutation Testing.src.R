# David Parsons
# Data 101 - Rutgers University
# 198:142:01


knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
options(scipen=999)
census <- read.csv("CENSUSNEW.csv")

census[, 11] <- ""
colnames(census)[11] <- "isDoc"
census$isDoc <- (census$EDUCATION == "Doctorate")

# Creates Dictionary of Numerics
nums <- c("AGE", "YEARS", "CAPITALNET")
# Creates Dictionary of Elements
elems <- levels(census$ELEMENTS)
# Non Element cats
cats <- c("AGE", "YEARS", "CAPITALGAINS", "CAPITALLOSS", "CAPITALNET")
# Creates Net Capital Variable
census$CAPITALNET = census$CAPITALGAINS - census$CAPITALLOSS
# Permuatation testing for Age and Net Capital
print(PermutationTestSecond::Permutation(census, "isDoc", "AGE", 50000, FALSE, TRUE))
print(PermutationTestSecond::Permutation(census, "isDoc", "CAPITALNET", 50000, FALSE, TRUE))