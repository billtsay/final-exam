
setwd("~/final-exam")
library(randomForest)

# Import the data
occupancy <- "jlt245_final_exam_occupancy_dataset.csv"

ocp_data <- read.csv(occupancy)

require(caTools)
set.seed(101)

n_rep <- 2
v_sratio <- 0.3

res <- xy <- list()

n_df <- 20
df <- 1:n_df

data <- ocp_data

for (i in 1:n_rep) {
  sample = sample.split(data$Occupancy, SplitRatio = v_sratio)
  xy[[i]] <- subset(data, sample == TRUE)
  # to enforce rf to do classification.
  xy[[i]]$Occupancy <- as.factor(xy[[i]]$Occupancy)
  res[[i]] <- lapply(df, function(degf) randomForest(Occupancy ~ ., data = xy[[i]], maxnodes = 30, ntree = degf))
  data <- subset(data, sample == FALSE)
}

xy_test <- data

# Compute the training and test errors for each model
pred <- t_pred <- list()
mse <- te <- bias_p2 <- variance <- matrix(NA, nrow = n_df, ncol = n_rep)

for (i in 1:n_rep) {
  # to enforce rf to do classification.
  t_pred[[i]] <- mapply(function(obj) predict(obj, xy[[i]]), res[[i]])
  t_dat <- as.list(data.frame(t_pred[[i]]))
  mse[, i] <- sapply(t_dat, 
                     function(y_hat) mean((as.numeric(xy[[i]]$Occupancy) - as.numeric(y_hat))^2))
  pred[[i]] <- mapply(function(obj) predict(obj, xy_test), res[[i]])
  p_dat <- as.list(data.frame(pred[[i]]))
  te[, i] <- sapply(p_dat, 
                    function(y_hat) mean((as.numeric(xy_test$Occupancy) - as.numeric(y_hat))^2))
  bias_p2[, i] <- sapply(p_dat, function(y_hat) mean(mean(as.numeric(y_hat)) - as.numeric(xy_test$Occupancy))^2)
  variance[, i] <- sapply(p_dat, function(y_hat) mean(var(as.numeric(y_hat))))
}

# Compute the average training and test errors
av_mse <- rowMeans(mse)
av_te <- rowMeans(te)
av_bias_p2 <- rowMeans(bias_p2)
av_var <- rowMeans(variance)

# Plot the errors
plot(df, av_mse, type = "l", lwd = 2, col = gray(0.4), ylab = "Training and Test MSEs", 
     xlab = "Model Complexity", ylim = c(0, 1), log = "x")

abline(h = sigma_eps, lty = 2, lwd = 0.5)

for (i in 1:n_rep) {
  lines(df, te[, i], col = "lightpink")
}

for (i in 1:n_rep) {
  lines(df, mse[, i], col = gray(0.8))
}

lines(df, av_mse, lwd = 2, col = gray(0.4))
lines(df, av_te, lwd = 2, col = "darkred")
points(df[1], av_mse[1], col = "palegreen3", pch = 17, cex = 1.5)
points(df[1], av_te[1], col = "palegreen3", pch = 17, cex = 1.5)
points(df[which.min(av_te)], av_mse[which.min(av_te)], col = "darkorange", pch = 16, cex = 1.5)
points(df[which.min(av_te)], av_te[which.min(av_te)], col = "darkorange", pch = 16, cex = 1.5)
legend(x = "top", legend = c("Training MSE", "Test MSE"), lwd = rep(2, 2), 
       col = c(gray(0.4), "darkred"), text.width = 0.3, cex = 0.85)


# Plot the bias^2 and varianceerrors
plot(df, av_bias_p2, type = "l", lwd = 2, col = gray(0.4), ylab = "Bias and Variance", 
     xlab = "Model Complexity", ylim = c(0, 2), log = "x")
lines(df, av_var, lwd = 2, col = "darkred")
lines(df, av_bias_p2+av_var, lwd = 2, col = "darkblue")
legend(x = "top", legend = c("av_bias_p2", "av_var", "sum of them"), lwd = rep(2, 2), 
       col = c(gray(0.4), "darkred", "darkblue"), text.width = 0.3, cex = 0.85)

