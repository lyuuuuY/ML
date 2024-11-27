library(tree)
data <- read.csv("data/bank-full.csv", header = TRUE, sep = ";")
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

for (i in 2:51) {
  prunedTree <- prune.tree(bank_tree_3, best = i, method = "misclass")
  # Predict on training and validation data
  pred_train <- predict(prunedTree, newdata = train, type = "class")
  pred_valid <- predict(prunedTree, newdata = valid, type = "class")
  # Compute misclassification rates
  trainScore[i - 1] <- mean(pred_train != train$y)
  testScore[i - 1] <- mean(pred_valid != valid$y)
}
plot(2:50, trainScore[2:50], type="b", col="red", 
     ylim=c(0.094,0.11))
points(2:50, testScore[2:50], type="b", col="blue")

# find optimal size
best_size <- which(testScore==min(testScore))[2]
prunedTree <- prune.tree(bank_tree_3, best = best_size, method = "misclass")

summary(prunedTree)
