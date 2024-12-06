data = read.csv("data/tecator.csv")
head(data)

dataProcessing = function(data, colnames){
  for(i in colnames)
    data[[i]] = NULL
  return(data)
}

dataSplit = function(data, proportion){
  set.seed(12345)
  n = dim(data)[1]
  id = sample(1:n,floor(n*0.5))
  train_data = data[id,]
  test_data = data[-id,]
  data_list = list()
  data_list$train = train_data
  data_list$test = test_data
  return(data_list)
}

computeMSE = function(model ,data, lambda = NULL){
  if(!is.null(lambda)){
    x = as.matrix(data[, -which(names(data) == "Fat")]) 
    y = data$Fat 
    pred = predict(model, newx = x, s = lambda)
    mse = mean((y - pred) ** 2)
  }
  else{
    pred = predict(model, data)
    mse = mean((data$Fat - pred) ** 2)
    return(mse)
  }
}

data = dataProcessing(data, c("Sample", "Protein", "Moisture"))
data_list = dataSplit(data, proportion = 0.5)
train_data = data_list$train
test_data = data_list$test

lr = lm(Fat ~ ., data = train_data)
trainMSE = computeMSE(lr, train_data)
testMSE = computeMSE(lr, test_data)

library(glmnet)
x_train = as.matrix(train_data[, -which(names(train_data) == "Fat")]) 
y_train = train_data$Fat  
lasso_model = glmnet(x_train, y_train, alpha = 1)

# Plot the coefficient paths
png("fig/p1_img1.png", width = 1800, height = 1200, res = 300)
plot(lasso_model, xvar = "lambda", label = TRUE)
axis(1, at = log(lasso_model$lambda), labels = floor(lasso_model$lambda)) 

plot(lasso_model, xvar = "lambda", label = TRUE)
title(main = "LASSO Coefficient Paths")

# The value of lambda that results in only three non-zero features.
for (i in seq_along(lasso_model$lambda)) {
  coefs <- coef(lasso_model, s = lasso_model$lambda[i])
  non_zero_count <- sum(coefs != 0) - 1  
  if (non_zero_count == 3) {
    lambda_selected <- lasso_model$lambda[i]
    cat("Lambda with 3 non-zero coefficients:", lambda_selected, "\n")
    break
  }
}

coefs <- coef(lasso_model, s = lambda_selected)
coefs_selected_list <- which(coefs!=0)
coefs_selected_list <- coefs_selected_list[-1] #without intercept
coefs_selected <- coefs[coefs_selected_list,]
cat("The remaining features are:", "\n")
print(coefs_selected)

ridge_model = glmnet(x_train, y_train, alpha = 0)

plot(ridge_model, xvar = "lambda", label = TRUE)
axis(1, at = log(ridge_model$lambda), labels = floor(ridge_model$lambda)) 

cv_lasso = cv.glmnet(x_train, y_train, alpha = 1)
cv_scores = cv_lasso$cvm
# Load necessary libraries
library(ggplot2)

# Extract data for plotting
plot_data = data.frame(
  log_lambda = log(cv_lasso$lambda),  # Log of lambda values
  cv_scores = cv_lasso$cvm           # CV scores (MSE)
)

# Create ggplot
p3 = ggplot(plot_data, aes(x = log_lambda, y = cv_scores)) +
  geom_line(color = "black") +         # Line connecting the points
  geom_point(size = 2, color = "black") +  # Points for CV scores
  geom_vline(xintercept = log(cv_lasso$lambda.min), color = "red", linetype = "dashed", size = 1) +  # Optimal lambda line
  geom_vline(xintercept = log(cv_lasso$lambda.1se), color = "blue", linetype = "dashed", size = 1) + # 1 SE lambda line
  labs(
    title = "Dependence of CV Score on log(lambda)",
    x = "log(lambda)",
    y = "CV Score (Mean Squared Error)"
  ) +
  annotate("text", x = log(cv_lasso$lambda.min), y = min(cv_lasso$cvm)+10, 
           label = "lambda.min", color = "red", hjust = -0.2, vjust = -0.5) +
  annotate("text", x = log(cv_lasso$lambda.1se), y = min(cv_lasso$cvm)+20, 
           label = "lambda.1se", color = "blue", hjust = -0.2, vjust = -0.5) +
  theme_minimal()
ggsave("fig/p1_pic3.png", plot = p3, width = 6, height = 4, dpi = 300)

optimal_lambda = cv_lasso$lambda.min
cat("the optimal lambda: ", optimal_lambda,"\n")
coef = coef(cv_lasso, s = optimal_lambda)
var_num = sum(coef != 0) - 1
cat("the number of variables: ", var_num,"\n")

mse_optimal_lam = computeMSE(cv_lasso, test_data, lambda = optimal_lambda)
logminus4_lambda = cv_lasso$lambda[which.min(abs(log(cv_lasso$lambda) + 4))]
mse_certain_lam = computeMSE(cv_lasso, test_data, lambda = logminus4_lambda)
cat("lambda = -4, mse = ", mse_certain_lam, "\n")
cat("lambda = optimal lambda, mse = ", mse_optimal_lam, "\n")

cat("the optimal lambda: ", optimal_lambda,"\n")
coef = coef(cv_lasso, s = optimal_lambda)
var_num = sum(coef != 0) - 1
cat("the number of variables: ", var_num,"\n")

mse_optimal_lam = computeMSE(cv_lasso, test_data, lambda = optimal_lambda)
logminus4_lambda = cv_lasso$lambda[which.min(abs(log(cv_lasso$lambda) + 4))]
mse_certain_lam = computeMSE(cv_lasso, test_data, lambda = logminus4_lambda)
cat("log_lambda = -4, mse = ", mse_certain_lam, "\n")
cat("lambda = ",optimal_lambda, "mse = ", mse_optimal_lam, "\n")

y_test = test_data$Fat
x_test = test_data[, -which(names(test_data) == "Fat")]
y_pred = predict(cv_lasso, s = optimal_lambda, newx = as.matrix(x_test))

p4 = ggplot(data = cbind(y_test, y_pred), mapping = aes(x = y_test, y = y_pred)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Labels in Test vs Predicted Labels",
       x = "Labels in Test",
       y = "Predicted Labels") +
  theme_minimal()

