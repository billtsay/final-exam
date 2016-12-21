
setwd("~/final-exam")

# Import the data
loan_csv <- "jlt245_final_exam_occupancy_dataset.csv"

loan_data <- read.csv(loan_csv)
class(loan_data)

require(caTools)
set.seed(101)

n_rep <- 2
v_sratio <- 0.3

res <- xy <- list()
families <- list(binomial(), gaussian(), poisson(), quasi(), quasibinomial(), quasipoisson())
fnames <- c("binomial", "gaussian", "poisson", "quasi", "quasibinomial", "quasipoisson")

n_df <- length(families)


# glm(formula, family=familytype(link=linkfunction), data=)
# Family	Default Link Function
# binomial	(link = "logit")
# gaussian	(link = "identity")
# Gamma	(link = "inverse")
# inverse.gaussian	(link = "1/mu^2")
# poisson	(link = "log")
# quasi	(link = "identity", variance = "constant")
# quasibinomial	(link = "logit")
# quasipoisson	(link = "log")

data <- loan_data

for (i in 1:n_rep) {
  sample = sample.split(data$Occupancy, SplitRatio = v_sratio)
  xy[[i]] <- subset(data, sample == TRUE)
  res[[i]] <- lapply(families, function(family) glm(Occupancy~., xy[[i]], family = family))
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

colnames(m_dat) <- fnames

barplot(m_dat, xlab= "Family", beside=TRUE, col=rainbow(4))
legend("topleft", c("Training MSE","Test MSE","Bias^2","Variance"), cex=1, bty="n", fill=rainbow(4))


