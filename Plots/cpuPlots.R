library(ggplot2)
# Read's data from csv file in local directory, and places it in a data frame
cpu <- read.csv("cpus.csv")
# Prints the structure of the data frame

# Sets Variables for readability
memMin <- cpu$mmin
memMax <- cpu$mmax
cycleT <- cpu$syct
channelMin <- cpu$chmin
channelMax <- cpu$chmax
cache <- cpu$cach
# Factor of the occurance of each cache Size
cacheRanges <- as.factor(cache)
perf <- cpu$perf

# Box Plots

perfBox <- boxplot(perf, pch = 19)

# End Box Plots


# Scatter Plots

# Cyle Time vs Performance of CPUs - shows that maybe cycle time isn't the biggest contributing factor for performance
cycleOnPerf <- plot(cycleT, perf, main="CPU Cycle Time vs. Performance", xlab = "Cycle Time in Seconds", ylab = "Benchmark Performance", pch = 19)

memMinOnPerf <- plot(memMin, perf, main="CPU Memory (min) vs. Performance", xlab = "Minimum Memory in Kilobytes", ylab = "Benchmark Performance", pch = 19)

memMaxOnPerf <- plot(memMax, perf, main="CPU Memory (max) vs. Performance", xlab = "Max Memory in Kilobytes", ylab = "Benchmark Performance", pch = 19)

cacheOnPerf <- plot(cache, perf, main="CPU Cache vs. Performance", xlab = "Cache in Kilobytes", ylab = "Benchmark Performance", pch = 19)

# End Scatter Plots

# Bar Plots

barplot(table(cacheRanges), main = "Cache Size Distribution", xlab = "Size of Cache in KB")

# End Bar Plots

# Frame without cache bottle neck
bottleFree <- cpu[cpu$cach != 0, ]

botCacheOnPef <- plot(bottleFree$cach, bottleFree$perf,main="CPU Cache vs. Performance", xlab = "Cache in Kilobytes", ylab = "Benchmark Performance", pch = 19)


botPefBox <- boxplot(bottleFree$perf, pch = 19)
# End Frame without bottle neck

str(cpu)
# Prints Summary of Data
summary(cpu)
summary(bottleFree)
# Creates a simple plot of cache vs performance
plot(cpu$cach, cpu$perf)
plot(cpu$syct, cpu$perf)

boxplot(cpu$perf)

cpu_matrix <- data.matrix(cpu)

cpu_heatmap <- heatmap(cpu_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
