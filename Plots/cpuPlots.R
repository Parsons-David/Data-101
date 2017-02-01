library(ggplot2)
# Read's data from csv file in local directory, and places it in a data frame
cpu <- read.csv("cpus.csv")
# Prints the structure of the data frame
summary(cpu)

perf <- cpu$perf

cycleOnPerf <- plot(cpu$syct, perf, main="CPU Cycle Time vs. Performance", xlab = "Cycle Time in Seconds", ylab = "Benchmark Performance", xlim = rev(range(cpu$syct)), col = "blue")

memMaxOnPerf <- plot(cpu$mmax, perf, main="CPU Memory (max) vs. Performance", xlab = "Max Memory in Kilobytes", ylab = "Benchmark Performance", col = "red")

memMinOnPerf <- plot(cpu$mmin, perf, main="CPU Memory (min) vs. Performance", xlab = "Minimum Memory in Kilobytes", ylab = "Benchmark Performance", col = "red")

chanMaxOnPerf <- plot(cpu$chmax, perf, main="Channels (max) vs. Performance", xlab = "Max Channels", ylab = "Benchmark Performance", col = "darkgreen")

chanMinOnPerf <- plot(cpu$chmin, perf, main="Channels (min) vs. Performance", xlab = "Min Channels", ylab = "Benchmark Performance", col = "darkgreen")

cacheOnPerf <- plot(cpu$cach, perf,main="CPU Cache vs. Performance", xlab = "Cache in Kilobytes", ylab = "Benchmark Performance", col = "orange")

cycleTBox <- boxplot(cpu$syct, data = cpu, xlab = "Cycle Time in Nanoseconds",col = "lightblue", medcol = "orange", bg = "blue", range = 4, cex.main=2,cex.lab=1.5, horizontal = TRUE, varwidth = TRUE, pch = 19)

upperNinty = as.numeric(quantile(cpu$perf, 0.9))

ggplot(cpu,aes(syct, fill = (perf > upperNinty)))+geom_density()+scale_x_continuous(name = "Cycle Time in Nanoseconds")+scale_y_continuous(name = "Density")

cycleTBox <- boxplot(cpu$mmax, data = cpu, xlab = "Memory (max) in Kilobytes",col = "lightgreen", medcol = "blue", bg = "blue", range = 4, cex.main=2,cex.lab=1.5, horizontal = TRUE, varwidth = TRUE, pch = 19)

upperNinty = as.numeric(quantile(cpu$perf, 0.9))

ggplot(cpu,aes(mmax, fill = (perf > upperNinty)))+geom_density()+geom_density()+scale_x_continuous(name = "Memory (max) in Kilobytes")+scale_y_continuous(name = "Density")

cacheBox <- boxplot(cpu$cach, data = cpu, xlab = "Cache in Kilobytes",col = "grey", medcol = "red", bg = "blue", range = 4, cex.main=2,cex.lab=1.5, horizontal = TRUE, varwidth = TRUE, pch = 19)

upperNinty = as.numeric(quantile(cpu$perf, 0.9))

ggplot(cpu,aes(cach, fill = (perf > upperNinty)))+geom_density()+geom_density()+scale_x_continuous(name = "Cache in Kilobytes")+scale_y_continuous(name = "Density")
