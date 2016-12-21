
setwd("~/final-exam")
library(e1071)


# Import the data
loan_csv <- "jlt245_final_exam_occupancy_dataset.csv"

loan_data <- read.csv(loan_csv)
class(loan_data)

require(caTools)
set.seed(101)

n_rep <- 2
v_sratio <- 0.3

res <- xy <- list()
kernels <- c("linear", "polynomial", "radial", "sigmoid")

n_df <- length(kernels)

data <- loan_data

for (i in 1:n_rep) {
  sample = sample.split(data$Occupancy, SplitRatio = v_sratio)
  xy[[i]] <- subset(data, sample == TRUE)
  res[[i]] <- lapply(kernels, function(kernel) svm(Occupancy~., xy[[i]], kernel = kernel))
  data <- subset(data, sample == FALSE)
}

xy_test <- data

# Compute the training and test errors for each model
pred <- t_pred <- list()
mse <- te <- bias_p2 <- variance <- matrix(NA, nrow = n_df, ncol = n_rep)


for (i in 1:n_rep) {
  t_pred[[i]] <- mapply(function(obj) predict(obj, xy[[i]]), res[[i]])
  t_dat <- as.list(data.frame(t_pred[[i]]))
  mse[, i] <- sapply(t_dat, function(y_hat) mean((xy[[i]]$Occupancy - y_hat)^2))
  pred[[i]] <- mapply(function(obj) predict(obj, xy_test), res[[i]])
  p_dat <- as.list(data.frame(pred[[i]]))
  te[, i] <- sapply(p_dat, function(y_hat) mean((xy_test$Occupancy - y_hat)^2))
  bias_p2[, i] <- sapply(p_dat, function(y_hat) mean(mean(y_hat) - xy_test$Occupancy)^2)
  variance[, i] <- sapply(p_dat, function(y_hat) mean(var(y_hat)))
}


# Compute the average training and test errors
av_mse <- rowMeans(mse)
av_te <- rowMeans(te)
av_bias_p2 <- rowMeans(bias_p2)
av_var <- rowMeans(variance)

# Plot MSE values for each family type.
dat <- cbind(av_mse, av_te, av_bias_p2, av_var)
m_dat <- as.matrix(t(dat))

colnames(m_dat) <- kernels

barplot(m_dat, xlab= "Family", beside=TRUE, col=rainbow(4), log="y")
legend("topleft", c("Training MSE","Test MSE","Bias^2","Variance"), cex=1, bty="n", fill=rainbow(4))


