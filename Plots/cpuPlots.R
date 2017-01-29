data <- read.csv("cpus.csv")
str(data)
plot(data$cach, data$perf)
plot(data$syct, data$perf)
