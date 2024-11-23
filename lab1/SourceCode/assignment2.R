parkinsons <- read.csv("parkinsons.csv",header=TRUE)

parkinsons <- parkinsons[,!names(parkinsons)%in%c("subject.","age","sex","test_time","total_UPDRS")]

library(caret)
#data scaling
set.seed(12345)
n <- nrow(parkinsons)
id <- sample(1:n,floor(n*0.6))
train <- parkinsons[id,]
test <- parkinsons[-id,]

library(caret)
scaler <- preProcess(train)    
trainS <- predict(scaler,train)
testS <- predict(scaler,test)

#fit the model 
fit <- lm(motor_UPDRS~.-1,data=trainS)
summary(fit)

#using model to predict traing data

predictions_train <- predict(fit,newdata=trainS)

predictions_test <- predict(fit, newdata = testS)

#compute MSE for trainning and test data
MSE_trainning <- mean((trainS$motor_UPDRS-predictions_train)^2)
MSE_test <- mean((testS$motor_UPDRS-predictions_test)^2)
cat("MSE for the training data is :",MSE_trainning,"\n")
cat("MSE for the test data is :",MSE_test,"\n")

#
#The coefficient`Jitter.Abs.`,`Shimmer.APQ5`,`Shimmer.APQ11`,`NHR`,`HNR`,`DFA`,`PPE`
#is highly significant, as their P-value < 0.001,which suggests that they have a strong 
#and statistically reliable impact on the response variable.

#The coefficient for `Shimmer.dB.` is statistically significant, as its P-value < 0.01. 
#This suggests that `Shimmer.dB.` has a meaningful and reliable impact on the response variable.


#log_likelihood
Loglikelihood <- function(theta,sigma,X,Y){
  n <- length(Y)
  predicted_Y <- X%*%theta
  squared_error <- (Y-predicted_Y)^2
  log_likelihood <- -0.5*n*log(2*pi*sigma^2)-0.5/sigma^2*sum(squared_error)
  return(log_likelihood)
}

#compute the loglikelihood for the fitting model

 #extract theta
theta <- coef(fit)
length(theta)

 #extract sigma
residual_train <- trainS$motor_UPDRS-predictions_train
sigma <- sqrt(mean(residual_train^2)) # 先平方再求均值，再开方

 #extract X as feature matrix for the training data
X_train <- as.matrix(trainS[,-1])
 #extract Y
Y_train <- as.numeric(trainS$motor_UPDRS)

 #compute loglikelihood for the training data
Loglikelihood(theta,sigma,X_train,Y_train)
predictions_train
#Ridge 
Ridge <- function(theta,sigma,lambda,X,Y){
  penalty <- lambda*sum(theta^2) #penalty 
  loglikelihood <- Loglikelihood(theta,sigma,X,Y)
  ridge_loglikelihood <- -loglikelihood+penalty #最大化负数对数似然
  return(ridge_loglikelihood)
}

#RidgeOpt

RidgeOpt <- function(lambda,X,Y){
  
  #object function
  objfun <- function(params){
    #extract theta and sigma
    theta <- params[1:ncol(X)]
    sigma <- params[ncol(X)+1]
    #ensure sigma is non-negative
    if(sigma<=0)
      return(Inf)
    
    #compute ridge-loglikelihood
    RidgeLikelihood <- Ridge(theta,sigma,lambda,X,Y)
    return(RidgeLikelihood) 
  }
  #initialize theta and sigma
  initialized_theta <- rep(0,ncol(X)) #theta to be 0
  initialized_sigma <- sd(Y)    #初始化sigma为目标变量标准差？why
  
  #combine theta with sigma
  initialized_params <- c(initialized_theta,initialized_sigma)
  
  #using optim function to optimize
  result <- optim(par=initialized_params,
                  fn=objfun,
                  method = "BFGS",
                  control = list(maxit=1000))
   if(result$convergence!=0){
     warning("Optimization did not converge.")
   }                
  optimezed_params <- result$par
  return(list(theta=optimezed_params[1:ncol(X)],
              sigma=optimezed_params[ncol(X)+1],
              value=-result$value, 
              convergence=result$convergence)) 
  
}

#degree of freedom

DF <- function(lambda,X){
  XtX <- t(X)%*%X                              #t(X)*x
  XtX_lambda <- XtX+lambda*diag(ncol(X))       #t(X)*x+lambda*I
  Hat_matrix <- X%*%solve(XtX_lambda)%*%t(X)
  df <- sum(diag(Hat_matrix))                           #trace of the hat matrix
  return(df)
}

X_test <- as.matrix(testS[,-1])
Y_test <- as.numeric(testS$motor_UPDRS)

#result and predicted values for training and testing data  and MSE value
#lambda=1
result1 <- RidgeOpt(1, X_train, Y_train)
theta1 <- result1$theta
sigma <- result1$sigma
predicted_1_Y_train <- X_train%*%theta1
predicted_1_Y_test <- X_test%*%theta1
MSE_1_trian <- mean((Y_train-predicted_1_Y_train)^2)
MSE_1_test <- mean((Y_test-predicted_1_Y_test)^2)
df_1 <- DF(1,X_train)
cat("Training data MSE when lambda=1:",MSE_1_trian,"\n")
cat("Test data MSE when lambda=1:",MSE_1_test,"\n")
cat("Degree of freedom when lambda=1:",df_1,"\n")


#lambda=100
result2 <- RidgeOpt(100, X_train, Y_train)
theta2 <- result2$theta
sigma2 <- result2$sigma
predicted_100_Y_train <- X_train%*%theta2
predicted_100_Y_test <- X_test%*%theta2
MSE_100_trian <- mean((Y_train-predicted_100_Y_train)^2)
MSE_100_test <- mean((Y_test-predicted_100_Y_test)^2)
df_100 <- DF(100,X_train)
cat("Training data MSE when lambda=100:",MSE_100_trian,"\n")
cat("Test data MSE when lambda=100:",MSE_100_test,"\n")
cat("Degree of freedom when lambda=100:",df_100,"\n")

#lambda=1000
result3 <- RidgeOpt(1000, X_train, Y_train)
theta3 <- result3$theta
sigma3 <- result3$sigma
predicted_1000_Y_train <- X_train%*%theta3
predicted_1000_Y_test <- X_test%*%theta3
MSE_1000_trian <- mean((Y_train-predicted_1000_Y_train)^2)
MSE_1000_test <- mean((Y_test-predicted_1000_Y_test)^2)
df_1000 <- DF(1000,X_train)
cat("Training data MSE when lambda=1000:",MSE_1000_trian,"\n")
cat("Test data MSE when lambda=1000:",MSE_1000_test,"\n")
cat("Degree of freedom when lambda=1000:",df_1000,"\n")


