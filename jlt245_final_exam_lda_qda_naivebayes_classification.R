
setwd("~/final-exam")
library(e1071)
library(caret)

# Import the data
ocp_csv <- "jlt245_final_exam_occupancy_dataset.csv"
ocp_data <- read.csv(ocp_csv)

require(caTools)
set.seed(101)

n_rep <- 2
v_sratio <- 0.3

res <- xy <- list()

n_df <- 2

data <- ocp_data

for (i in 1:n_rep) {
  sample = sample.split(data$Occupancy, SplitRatio = v_sratio)
  xy[[i]] <- subset(data, sample == TRUE)
  xy[[i]]$Occupancy <- as.factor(xy[[i]]$Occupancy)
  res[[i]] <- list(
    train(Occupancy ~ ., method="lda", preProcess = c('scale', 'center'), data=xy[[i]]),
    train(Occupancy ~ ., method="qda", preProcess = c('scale', 'center'), data=xy[[i]]),
    naiveBayes(Occupancy ~ ., data=xy[[i]])
    )
  data <- subset(data, sample == FALSE)
}


xy_test <- data

print(res[[1]][[1]], showSD = TRUE)
print(res[[1]][[2]], showSD = TRUE)

cm_lda <- confusionMatrix(xy_test$Occupancy, predict(res[[1]][[1]], xy_test))
cm_qda <- confusionMatrix(xy_test$Occupancy, predict(res[[1]][[2]], xy_test))

cm_lda
cm_qda


lda_mse <- qda_mse <- t_lda_mse <- t_qda_mse <- nb_mse <- t_nb_mse <- list()
lda_biasP2 <- qda_biasP2 <- lda_var <- qda_var <- nb_biasP2 <- nb_var <- list()

for (i in 1:n_rep) {
  p_lda = predict(res[[i]][[1]], xy[[i]])
  p_qda = predict(res[[i]][[2]], xy[[i]])
  p_nb = predict(res[[i]][[3]], xy[[i]])
  tp_lda = predict(res[[i]][[1]], xy_test)
  tp_qda = predict(res[[i]][[2]], xy_test)
  tp_nb = predict(res[[i]][[3]], xy_test)
  
  lda_mse[[i]] <- mean((as.numeric(xy[[i]]$Occupancy) - as.numeric(p_lda))^2) 
  qda_mse[[i]] <- mean((as.numeric(xy[[i]]$Occupancy) - as.numeric(p_qda))^2) 
  nb_mse[[i]] <- mean((as.numeric(xy[[i]]$Occupancy) - as.numeric(p_nb))^2)
  
  t_lda_mse[[i]] <- mean((as.numeric(xy_test$Occupancy) - as.numeric(tp_lda))^2) 
  t_qda_mse[[i]] <- mean((as.numeric(xy_test$Occupancy) - as.numeric(tp_qda))^2) 
  t_nb_mse[[i]] <- mean((as.numeric(xy_test$Occupancy) - as.numeric(tp_nb))^2) 
  
  lda_biasP2[[i]] <- mean((as.numeric(xy_test$Occupancy) - mean(as.numeric(tp_lda)))^2) 
  qda_biasP2[[i]] <- mean((as.numeric(xy_test$Occupancy) - mean(as.numeric(tp_qda)))^2) 
  nb_biasP2[[i]] <- mean((as.numeric(xy_test$Occupancy) - mean(as.numeric(tp_nb)))^2) 
  
  lda_var[[i]] <- mean((as.numeric(tp_lda) - mean(as.numeric(tp_lda)))^2) 
  qda_var[[i]] <- mean((as.numeric(tp_qda) - mean(as.numeric(tp_qda)))^2) 
  nb_var[[i]] <- mean((as.numeric(tp_nb) - mean(as.numeric(tp_nb)))^2) 
}

# Compute the average training and test errors
av_mse <- list(mean(as.numeric(lda_mse)), mean(as.numeric(qda_mse)), mean(as.numeric(nb_mse)))
av_te <- list(mean(as.numeric(t_lda_mse)), mean(as.numeric(t_qda_mse)), mean(as.numeric(t_nb_mse)))
av_biasP2 <- list(mean(as.numeric(lda_biasP2)), mean(as.numeric(qda_biasP2)), mean(as.numeric(nb_biasP2)))
av_var <- list(mean(as.numeric(lda_var)), mean(as.numeric(qda_var)), mean(as.numeric(nb_var)))

# Plot MSE values for each family type.
dat <- cbind(as.numeric(av_mse), as.numeric(av_te), as.numeric(av_biasP2), as.numeric(av_var))
m_dat <- as.matrix(t(dat))

colnames(m_dat) <- c("LDA", "QDA", "NaiveBayes")

barplot(m_dat, xlab= "Algorithm", beside=TRUE, col=rainbow(4))
legend("topleft", c("Training MSE","Test MSE","Bias^2","Variance"), cex=1, bty="n", fill=rainbow(4))





