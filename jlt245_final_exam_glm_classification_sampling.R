
setwd("~/final-exam")

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
sratio <- seq(0.1, 0.6, 0.01)
variance <- bias_p2 <- s_size <- list()
ntree <- 20

for (i in 1:length(sratio)) {
  s <- sampling(ocp_data, sratio[i], 1)
  xy <- s$train
  xy_test <- s$test
  sfun <- glm(Occupancy~., xy[[1]], family = binomial())
  pred <- predict(sfun, xy_test)
  s_size[[i]] <- nrow(xy[[1]]) 
  bias_p2[[i]] <- mean(mean(pred) - xy_test$Occupancy)^2
  variance[[i]] <- mean(var(pred))
}

# Plot the bias^2 and varianceerrors
plot(as.numeric(s_size), as.numeric(bias_p2), type = "l", lwd = 2, col = "blue", ylab = "Value", 
     xlab = "Sample Size", ylim = c(0, 35))

lines(as.numeric(s_size), as.numeric(variance), lwd = 2, col = "red")
legend(x = "bottom", legend = c("Bias^2", "Variance"), lwd = rep(2, 2), 
       col = c("red", "blue"), text.width = 0.3, cex = 0.85)



n_rep <- seq(1, 6)

variance <- bias_p2 <- list()
for (i in 1:length(n_rep)) {
  s <- sampling(ocp_data, 0.12, n_rep[i])
  xy <- s$train
  xy_test <- s$test

  b2 <- v <- list()
  for (j in 1:i) {
    sfun <- glm(Occupancy~., xy[[j]], family = binomial())
    pred <- predict(sfun, xy_test)
    b2[[j]] <- mean(mean(pred) - xy_test$Occupancy)^2
    v[[j]] <- mean(var(pred))
  }
  
  bias_p2[[i]] <- mean(as.numeric(b2))
  variance[[i]] <- mean(as.numeric(v))
}

# Plot the bias^2 and varianceerrors
plot(n_rep, as.numeric(bias_p2), type = "l", lwd = 2, col = "blue", ylab = "Value", 
     xlab = "Partition", ylim = c(0, 35))

lines(n_rep, as.numeric(variance), lwd = 2, col = "red")
legend(x = "bottom", legend = c("Bias^2", "Variance"), lwd = rep(2, 2), 
       col = c("red", "blue"), text.width = 0.3, cex = 0.85)






