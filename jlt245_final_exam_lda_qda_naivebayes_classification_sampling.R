library(e1071)
library(caret)

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
sratio <- seq(0.05, 0.6, 0.02)
lda_var <- qda_var <- nb_var <- lda_biasP2 <- qda_biasP2 <- nb_biasP2 <- s_size <- list()

for (i in 1:length(sratio)) {
  s <- sampling(ocp_data, sratio[i], 1)
  xy <- s$train
  xy_test <- s$test
  xy[[1]]$Occupancy <- as.factor(xy[[1]]$Occupancy)
  
  lda_fun <- train(Occupancy ~ ., method="lda", preProcess = c('scale', 'center'), data=xy[[1]])
  qda_fun <- train(Occupancy ~ ., method="qda", preProcess = c('scale', 'center'), data=xy[[1]])
  nb_fun <- naiveBayes(Occupancy ~ ., data=xy[[1]])
  
  lda_pred <- predict(lda_fun, xy_test)
  qda_pred <- predict(qda_fun, xy_test)
  nb_pred <- predict(nb_fun, xy_test)
  
  s_size[[i]] <- nrow(xy[[1]]) 
  lda_biasP2[[i]] <- mean(mean(as.numeric(lda_pred)) - xy_test$Occupancy)^2
  lda_var[[i]] <- mean(var(as.numeric(lda_pred)))
  qda_biasP2[[i]] <- mean(mean(as.numeric(qda_pred)) - xy_test$Occupancy)^2
  qda_var[[i]] <- mean(var(as.numeric(qda_pred)))
  nb_biasP2[[i]] <- mean(mean(as.numeric(nb_pred)) - xy_test$Occupancy)^2
  nb_var[[i]] <- mean(var(as.numeric(nb_pred)))
}

# Plot the bias^2 and varianceerrors
plot(as.numeric(s_size), as.numeric(lda_biasP2), type = "l", lwd = 2, col = "blue", ylab = "Value", 
     xlab = "Sample Size", ylim = c(0, 2))
lines(as.numeric(s_size), as.numeric(qda_biasP2), lwd = 2, col = "red")
lines(as.numeric(s_size), as.numeric(nb_biasP2), lwd = 2, col = "green")
lines(as.numeric(s_size), as.numeric(lda_var), lwd = 2, col = "darkblue")
lines(as.numeric(s_size), as.numeric(qda_var), lwd = 2, col = "darkgreen")
lines(as.numeric(s_size), as.numeric(nb_var), lwd = 2, col = "darkred")

legend(x = "top", legend = c("LDA_Bias^2", "QDA_Bias^2", "NB_Bias^2", "LDA_Variance", "QDA_Variance", "NB_Variance"),
       lwd = rep(2, 2), 
       col = c("blue", "red", "green", "darkblue", "darkgreen", "darkred"), text.width = 0.3, cex = 0.85)


n_rep <- seq(1, 6)

lda_var <- lda_biasP2 <- qda_var <- qda_biasP2 <- nb_var <- nb_biasP2 <- list()

for (i in 1:length(n_rep)) {
  s <- sampling(ocp_data, 0.12, n_rep[i])
  xy <- s$train
  xy_test <- s$test
  
  lda_b2 <- qda_b2 <- nb_b2 <- lda_v <- qda_v <- nb_v <- list()
  for (j in 1:i) {
    xy[[j]]$Occupancy <- as.factor(xy[[j]]$Occupancy)
    
    lda_fun <- train(Occupancy ~ ., method="lda", preProcess = c('scale', 'center'), data=xy[[j]])
    qda_fun <- train(Occupancy ~ ., method="qda", preProcess = c('scale', 'center'), data=xy[[j]])
    nb_fun <- naiveBayes(Occupancy ~ ., data=xy[[j]])

    lda_pred <- predict(lda_fun, xy_test)
    qda_pred <- predict(qda_fun, xy_test)
    nb_pred <- predict(nb_fun, xy_test)
    
    lda_b2[[j]] <- mean(mean(as.numeric(lda_pred)) - xy_test$Occupancy)^2
    lda_v[[j]] <- mean(var(as.numeric(lda_pred)))
    qda_b2[[j]] <- mean(mean(as.numeric(qda_pred)) - xy_test$Occupancy)^2
    qda_v[[j]] <- mean(var(as.numeric(qda_pred)))
    nb_b2[[j]] <- mean(mean(as.numeric(nb_pred)) - xy_test$Occupancy)^2
    nb_v[[j]] <- mean(var(as.numeric(nb_pred)))
  }
  
  lda_biasP2[[i]] <- mean(as.numeric(lda_b2))
  lda_var[[i]] <- mean(as.numeric(lda_v))
  qda_biasP2[[i]] <- mean(as.numeric(qda_b2))
  qda_var[[i]] <- mean(as.numeric(qda_v))
  nb_biasP2[[i]] <- mean(as.numeric(nb_b2))
  nb_var[[i]] <- mean(as.numeric(nb_v))
}


# Plot the bias^2 and variance
plot(n_rep, as.numeric(lda_biasP2), type = "l", lwd = 2, col = "blue", ylab = "Value", 
     xlab = "Partition", ylim = c(0, 2))
lines(n_rep, as.numeric(lda_var), lwd = 2, col = "red")
lines(n_rep, as.numeric(qda_var), lwd = 2, col = "green")
lines(n_rep, as.numeric(nb_var), lwd = 2, col = "darkred")
lines(n_rep, as.numeric(qda_biasP2), lwd = 2, col = "darkblue")
lines(n_rep, as.numeric(nb_biasP2), lwd = 2, col = "darkgreen")

legend(x = "top", legend = c("LDA_Bias^2", "LDA_Variance", "QDA_Bias^2", "QDA_Variance", "NB_Bias^2", "NB_Variance"),
       lwd = rep(2, 2), 
       col = c("blue", "red", "darkblue", "green", "darkgreen", "darkred"), text.width = 0.3, cex = 0.85)


