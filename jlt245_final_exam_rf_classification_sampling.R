
setwd("~/final-exam")
library(randomForest)

# Import the data
ocp_csv <- "jlt245_final_exam_occupancy_dataset.csv"

ocp_data <- read.csv(ocp_csv)

set.seed(101)

sampling <- function(r_data, splitRatio, rep) {
  data <- r_data
  xy <- list()
  
  for (i in 1:rep) {
    sample = sample.split(data$Occupancy, SplitRatio = splitRatio)
    xy[[i]] <- subset(data, sample == TRUE)
    data <- subset(data, sample == FALSE)
  }
  
  return (list(train=xy, test=data))
}


# test on sample size dependency of bias or variance.
sratio <- seq(0.01, 0.6, 0.01)
variance <- bias_p2 <- s_size <- list()
ntree <- 20

for (i in 1:length(sratio)) {
  s <- sampling(ocp_data, sratio[i], 1)
  xy <- s$train
  xy_test <- s$test
  xy[[1]]$Occupancy <- as.factor(xy[[1]]$Occupancy)
  sfun <- randomForest(Occupancy~., xy[[1]], maxnodes = 30, ntree = ntree)
  pred <- predict(sfun, xy_test)
  s_size[[i]] <- nrow(xy[[1]]) 
  bias_p2[[i]] <- mean(mean(as.numeric(pred)) - xy_test$Occupancy)^2
  variance[[i]] <- mean(var(as.numeric(pred)))
}

# Plot the bias^2 and varianceerrors
plot(as.numeric(s_size), as.numeric(bias_p2), type = "l", lwd = 2, col = "blue", ylab = "Bias^2", 
     xlab = "Sample Size", ylim = c(0, 1.5))

plot(as.numeric(s_size), as.numeric(variance), type = "l", lwd = 2, col = "red", ylab = "Variance", 
     xlab = "Sample Size", ylim = c(0, 1))


n_rep <- seq(1, 6)

variance <- bias_p2 <- list()
for (i in 1:length(n_rep)) {
  s <- sampling(ocp_data, 0.12, n_rep[i])
  xy <- s$train
  xy_test <- s$test

  b2 <- v <- list()
  for (j in 1:i) {
    xy[[j]]$Occupancy <- as.factor(xy[[j]]$Occupancy)
    
    sfun <- randomForest(Occupancy~., xy[[j]], maxnodes = 30, ntree = ntree)
    pred <- predict(sfun, xy_test)
    b2[[j]] <- mean(mean(as.numeric(pred)) - xy_test$Occupancy)^2
    v[[j]] <- mean(var(as.numeric(pred)))
  }
  
  bias_p2[[i]] <- mean(as.numeric(b2))
  variance[[i]] <- mean(as.numeric(v))
}

# Plot the bias^2 and varianceerrors
plot(n_rep, as.numeric(bias_p2), type = "l", lwd = 2, col = "blue", ylab = "Bias^2", 
     xlab = "partition", ylim = c(0, 1.5))

plot(n_rep, as.numeric(variance), type = "l", lwd = 2, col = "red", ylab = "Variance", 
     xlab = "partition", ylim = c(0, 0.5))







