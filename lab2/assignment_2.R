library(tree)
library(rpart)
library(pROC)
data <- read.csv("lab2/data/bank-full.csv", header = TRUE, sep = ";")
data <- data[,-12]
data$y <- as.factor(data$y)
characer_col <- c()
for (i in 1:15) {
  if(is.character(data[,i])==TRUE){
    characer_col <- c(characer_col,i)
  }
}
for (col in characer_col) {
  data[,col] <- as.factor(data[,col])
}

n <- nrow(data)
set.seed(12345)
id <- sample(1:n,floor(n*0.4))
train <- data[id,]

valid_test <- data[-id,]
id_2 <- sample(1:nrow(valid_test),floor(nrow(valid_test)*0.5))
valid <- valid_test[id_2,]
test <- valid_test[-id_2,]

# default setting
bank_tree <- tree(y ~ .,data = train)

pred_default_train <- predict(bank_tree, train, type = "class")
error_default_train <- mean(pred_default_train != train$y) # Misclassfication rate of train
table(train$y,pred_default_train)

pred_default_valid <- predict(bank_tree, valid, type = "class")
error_default_valid <- mean(pred_default_valid != valid$y) # Misclassfication rate of valid
table(valid$y,pred_default_valid)

# Decision Tree with smallest allowed node size equal to 7000
bank_tree_2 <- tree(y ~ ., data = train, control = tree.control(nobs = nrow(train), minsize = 7000))

pred_2_train <- predict(bank_tree_2, train, type = "class")
error_2_train <- mean(pred_2_train != train$y) # Misclassfication rate of train
table(train$y,pred_2_train)

pred_2_valid <- predict(bank_tree_2, valid, type = "class")
error_2_valid <- mean(pred_2_valid != valid$y) # Misclassfication rate of valid
table(valid$y,pred_2_valid)

# Decision trees minimum deviance to 0.0005
bank_tree_3 <- tree(y ~ ., data = train, control = tree.control(nobs = nrow(train), mindev = 0.0005))

pred_3_train <- predict(bank_tree_3, train, type = "class")
error_3_train <- mean(pred_3_train != train$y) # Misclassfication rate of train
table(train$y,pred_2_train)

pred_3_valid <- predict(bank_tree_3, valid, type = "class")
error_3_valid <- mean(pred_3_valid != valid$y) # Misclassfication rate of valid
table(valid$y,pred_3_valid)


# task 3


trainScore = rep(0, 50)
testScore = rep(0, 50)

for (i in 2:50) {
  prunedTree <- prune.tree(bank_tree_3, best = i)
  # Predict on training and validation data
  pred_train <- predict(prunedTree, newdata = train,type = "tree")
  pred_valid <- predict(prunedTree, newdata = valid,type = "tree")
  
  # Compute misclassification rates
  #trainScore[i - 1] <- mean(pred_train != train$y)
  #testScore[i - 1] <- mean(pred_valid != valid$y)
  trainScore[i - 1] <- deviance(prunedTree)
  testScore[i - 1] <- deviance(pred_valid)

}

plot(2:50, trainScore[2:50], type="b", col="red", 
     ylim=c(7800,11850))
points(2:50, testScore[2:50], type="b", col="blue")

# find optimal size

best_size_index <- which(testScore==min(testScore))
best_size <- best_size_index + 2
optimal_tree <- prune.tree(bank_tree_3, best = best_size)

summary(optimal_tree)

# task 4
optimal_pred <- predict(optimal_tree, newdata = test, type = "tree")

# confusion matrix

conf_matrix <- table( Observed = test$y,Predicted = optimal_pred)
print(conf_matrix)

# accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# F1-score
TP <- conf_matrix["yes", "yes"]
FP <- conf_matrix["no", "yes"]
FN <- conf_matrix["yes", "no"]

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-score:", f1_score))

# task 5
loss_matrix <- matrix(c(0, 5, 1, 0), nrow = 2)
colnames(loss_matrix) <- c("no", "yes")
rownames(loss_matrix) <- c("no", "yes")

tree_with_loss <- rpart(y ~ ., data = train, parms = list(loss = loss_matrix))
pred_with_loss <- predict(tree_with_loss, newdata = test, type = "class")


conf_matrix_loss <- table(Observed = test$y,Predicted = pred_with_loss)
print(conf_matrix_loss)

accuracy_loss <- sum(diag(conf_matrix_loss)) / sum(conf_matrix_loss)

#F1-score with loss
TP_l <- conf_matrix_loss["yes", "yes"]
FP_l <- conf_matrix_loss["no", "yes"]
FN_l <- conf_matrix_loss["yes", "no"]

precision_l <- TP_l / (TP_l + FP_l)
recall_l <- TP_l / (TP_l + FN_l)
f1_score_loss <- 2 * (precision_l * recall_l) / (precision_l + recall_l)


# task 6

# set models
tree_pred_prob <- predict(optimal_tree, newdata = test, type = "vector")
tree_probs_yes <- tree_pred_prob[, "yes"]

log_model <- glm(y ~ ., data = train, family = "binomial")
log_pred_prob <- predict(log_model, newdata = test, type = "response")

thresholds <- seq(0.05, 0.95, by = 0.05)

tree_tpr <- tree_fpr <- log_tpr <- log_fpr <-precision_tree <- recall_tree <-  precision_log <-recall_log <-   numeric(length(thresholds)) # initiate value

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  
  tree_pred <- ifelse(tree_probs_yes > threshold, "yes", "no")
  tree_cm <- table(Observed = test$y, Predicted = factor(tree_pred, levels = c("no", "yes")))
  
  # compute TPR and FPR 
  tree_tpr[i] <- tree_cm["yes", "yes"] / sum(tree_cm["yes", ])
  tree_fpr[i] <- tree_cm["no", "yes"] / sum(tree_cm["no", ])
  
  # compute precision and recall
  precision_tree[i] <- tree_cm["yes", "yes"] / (tree_cm["yes", "yes"] + tree_cm["no", "yes"])
  recall_tree[i] <- tree_cm["yes", "yes"] / (tree_cm["yes", "yes"] + tree_cm["yes", "no"])
  
  # result of logistic
  log_pred <- ifelse(log_pred_prob > threshold, "yes", "no")
  log_cm <- table(Observed = test$y,Predicted = factor(log_pred, levels = c("no", "yes")))
  
  # compute TPR and FPR (logistic)
  log_tpr[i] <- log_cm["yes", "yes"] / sum(log_cm["yes", ])
  log_fpr[i] <- log_cm["no", "yes"] / sum(log_cm["no",])
  
  # compute precision and recall (logistic)
  precision_log[i] <- log_cm["yes", "yes"] / (log_cm["yes", "yes"] + log_cm["no", "yes"])
  recall_log[i] <- log_cm["yes", "yes"] / (log_cm["yes", "yes"] + log_cm["yes", "no"])
}

# Plot ROC curve for the Optimal Decision Tree
plot(tree_fpr, tree_tpr, type = "b", col = "blue", pch = 16,
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     main = "ROC Curves for Decision Tree and Logistic Regression")

# Add ROC curve for Logistic Regression
lines(log_fpr, log_tpr, type = "b", col = "red", pch = 17)

# Add legend
legend("bottomright", legend = c("Decision Tree", "Logistic Regression"),
       col = c("blue", "red"), pch = c(16, 17))


# precision-recall curve
plot(recall_tree, precision_tree, type = "b", col = "blue", pch = 16,
     xlab = "Recall", ylab = "Precision",
     main = "Precision-Recall Curves")

# Add ROC curve for Logistic Regression
lines(recall_log, precision_log, type = "b", col = "red", pch = 17)

# Add legend
legend("bottomright", legend = c("Decision Tree", "Logistic Regression"),
       col = c("blue", "red"), pch = c(16, 17))





