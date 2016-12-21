setwd("~/final-exam")
seed <- 1809
set.seed(seed)

# create the model of true values
true_data <- function(n, beta) {
  x <- sort(runif(n, 0, 100))
  X <- cbind(1, poly(x, degree = (length(beta) - 1), raw = TRUE))
  y <- as.numeric(X %*% beta)
  
  return(data.frame(x = x, y = y))
}


# generate training dataset with noise.
observed_data <- function(n, beta, sigma_eps) {
  eps <- rnorm(n, 0, sigma_eps)
  t_dat <- true_data(n, beta)
  y <- t_dat$y + eps
  
  return(data.frame(x = t_dat$x, y = y))
}

# An example of true model and noisy dataset
require(splines)
n_rep <- 100
beta <- c(5, -0.1, 0.004, -3e-05)

n_train <- 50
sigma_eps <- 0.5

n_test <- 10000
t_dat <- true_data(n_test, beta)

# fit the model with glm.
n_df <- 30
df <- 1:n_df
xy <- res <- list()

# using n_rep dataset to train model
for (i in 1:n_rep) {
  xy[[i]] <- observed_data(n_train, beta, sigma_eps)
  x <- xy[[i]][, "x"]
  y <- xy[[i]][, "y"]
  res[[i]] <- apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
}

# plot true model and training dataset
x <- xy[[1]]$x
y <- xy[[1]]$y

plot(y ~ x, col = "gray", lwd = 2)
lines(t_dat$x, t_dat$y, lwd = 3, col = "black")

# Plot the data
plot(y ~ x, col = "gray", lwd = 2)
lines(t_dat$x, t_dat$y, lwd = 3, col = "black")
lines(x, fitted(res[[1]][[1]]), lwd = 3, col = "palegreen3")
lines(x, fitted(res[[1]][[3]]), lwd = 3, col = "grey")
lines(x, fitted(res[[1]][[4]]), lwd = 3, col = "green")
lines(x, fitted(res[[1]][[5]]), lwd = 3, col = "blue")
lines(x, fitted(res[[1]][[6]]), lwd = 3, col = "orange")
lines(x, fitted(res[[1]][[25]]), lwd = 3, col = "steelblue")
legend(x = "topleft", legend = c("True function", "Linear fit (df = 1)", "Better model (df = 3)", "Better model (df = 4)", 
                                 "Best model (df = 5)", "Better model (df = 6)", "Overfitted model (df = 25)"), 
       lwd = rep(3, 4), col = c("black", "palegreen3", "grey", "green", "blue", "orange",  "steelblue"), 
       text.width = 32, cex = 0.85)


# test dataset for calculating MSE, bias and variance
xy_test <- observed_data(n_test, beta, sigma_eps) 

# Compute the training and test errors for each model
pred <- list()
mse <- te <- bias_p2 <- variance <- matrix(NA, nrow = n_df, ncol = n_rep)

for (i in 1:n_rep) {
  mse[, i] <- sapply(res[[i]], function(obj) deviance(obj)/nobs(obj))
  pred[[i]] <- mapply(function(obj, degf) predict(obj, data.frame(x = xy_test$x)), res[[i]], df)
  p_dat <- as.list(data.frame(pred[[i]]))
  te[, i] <- sapply(p_dat, function(y_hat) mean((xy_test$y - y_hat)^2))
  bias_p2[, i] <- sapply(p_dat, function(y_hat) mean(mean(y_hat) - xy_test$y)^2)
  variance[, i] <- sapply(p_dat, function(y_hat) mean(var(y_hat)))
}


# Compute the average training and test errors
av_mse <- rowMeans(mse)
av_te <- rowMeans(te)
av_bias_p2 <- rowMeans(bias_p2)
av_var <- rowMeans(variance)


# Plot the errors
plot(df, av_mse, type = "l", lwd = 2, col = gray(0.4), ylab = "Training and Test MSEs", 
     xlab = "Model Complexity (spline's degrees of freedom [log scaled])", ylim = c(0, 1), log = "x")

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
points(df[which.min(av_te)], av_mse[which.min(av_te)], col = "darkorange", pch = 16, 
       cex = 1.5)
points(df[which.min(av_te)], av_te[which.min(av_te)], col = "darkorange", pch = 16, 
       cex = 1.5)
points(df[25], av_mse[25], col = "steelblue", pch = 15, cex = 1.5)
points(df[25], av_te[25], col = "steelblue", pch = 15, cex = 1.5)
legend(x = "top", legend = c("Training MSE", "Test MSE"), lwd = rep(2, 2), 
       col = c(gray(0.4), "darkred"), text.width = 0.3, cex = 0.85)


# Plot the bias^2 and varianceerrors
plot(df, av_bias_p2, type = "l", lwd = 2, col = gray(0.4), ylab = "Bias and Variance", 
     xlab = "Model Complexity (spline's degrees of freedom [log scaled])", ylim = c(0, 2), log = "x")
lines(df, av_var, lwd = 2, col = "darkred")
lines(df, av_bias_p2+av_var, lwd = 2, col = "darkblue")
legend(x = "top", legend = c("av_bias_p2", "av_var", "sum of them"), lwd = rep(2, 2), 
       col = c(gray(0.4), "darkred", "darkblue"), text.width = 0.3, cex = 0.85)

