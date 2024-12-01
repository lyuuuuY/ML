communities <- read.csv("data/communities.csv")

#task 1
#scale data except of `ViolentCrimesPerPop`
scaled <- scale(communities[,-which(names(communities)=="ViolentCrimesPerPop")],center=TRUE,scale=TRUE)

#compute eigenvectors and eigenvalues
covariance<- cov(scaled)

#extract eigenvectors and eigenvalues
eigen <- eigen(covariance)
eigen_values <- eigen$values
eigen_vectors <- eigen$vectors
eigen_values
eigen_vectors

#compute total variance
variance <- sum(eigen_values)

#compute contribution of each component
propotion_variance <- eigen_values/variance
propotion_variance

#compute cumulative variance proportion
cumulative_var <- cumsum(propotion_variance)
cumulative_var

#numbers of components to obtain 95% variance
index <-min(which(cumulative_var>0.95))
cat("At least ",index,"components to obtain a 95% of variance in the data.")

#the proportion of variance explained by each of the first two principal component.
first_com_proportion <- propotion_variance[1]
second_com_proportion <- propotion_variance[2]
cat("The proportion of variance of explained by each of the two principal components:\n")
cat("The first components: ",first_com_proportion ,"\n")
cat("The second components: ",second_com_proportion,"\n")




#task 2
library(ggplot2)

#using princomp
prin <- princomp(scaled)

#extract the first principle component
first_com_prin <- prin$scores[,1]

#make the trace plot of the first principle component
df_first <- data.frame(index=1:length(first_com_prin),
                       first_principle_component=first_com_prin)
ggplot(df_first,aes(x=index,y=first_principle_component))+
        geom_line()+
        labs(title = "The trace of the first principle component",x="Index",y="The First Principle Component")+
        theme_minimal()

scree_data <- data.frame(
  Component = 1:length(eigen_values),
  Variance_Explained = propotion_variance
)

ggplot(scree_data, aes(x = Component, y = Variance_Explained)) +
  geom_line() +
  geom_point() +
  ggtitle("Scree Plot") +
  xlab("Principal Component") +
  ylab("Variance Explained (%)") +
  theme_minimal()

#Do many features have a notable contribution to this component

#Top 5 features contribute mostly
first_com_contribute <- prin$loadings[,1]
first_com_contribute_df <- data.frame(
  Feature=names(first_com_contribute),
  contribution=first_com_contribute,
  contribution_Abs=abs(first_com_contribute)
)
#extract top 5 features 
top_5_features <- first_com_contribute_df$Feature[order(first_com_contribute_df$contribution_Abs,decreasing = TRUE)[1:5]]
cat("Top 5 features that contributed mostly to the first principle component: ",top_5_features,".\n")


#Relationship to crime level

#medFamInc:median family income (differs from household income for non-family households
#medIncome:median household income 
#PctKids2Par: percentage of kids in family housing with two parents
#pctWInvInc:percentage of households with investment / rent income in 1989
#PctPopUnderPov:percentage of people under the poverty level

# Conclusion: These five features mainly describes the following aspects: 1.Median family income;2.Median household income;3.Percentage of kids in family housing with two parents;4.Percentage of households with investment / rent income;5.Percentage of people under the poverty level.
#All five features are related to family property and household wealth levels.The logical relationship to  crime levels is that  families with fewer financial resources or  more children(which increases financial budget)may be more likely to resort to illegal activities to obtain money.Financial stress within household may be a significant factor contributing to crime level. 



#Plot the pc scores in the coordinates(PC1,PC2)
second_com_prin <- prin$scores[,2]
fir_sec_prin_df <- data.frame(
  PC1=first_com_prin,
  PC2=second_com_prin,
  ViolentCrimesPerPop=communities$ViolentCrimesPerPop
)
ggplot(fir_sec_prin_df,aes(x=PC1,y=PC2,color=ViolentCrimesPerPop))+
  geom_point()+
  scale_color_viridis_c()+
  labs(tittle="PC scores in the coordinates(PC1,PC2)",X="PC1",y="PC2",color="ViolentCrimesPerPop")+
  theme_minimal()
  
#task3
#split the data(50/50)
library(caret)
set.seed(12345)
n <- nrow(communities)
id <- sample(1:n,floor(n*0.5))
train <- communities[id,]
test <- communities[-id,]

scaler <- preProcess(train)
trainS <- predict(scaler,train)
testS <- predict(scaler,test)

#fit the model using scaled training data
fit <- lm(ViolentCrimesPerPop~.-1,data=trainS)

predictions_train <- predict(fit,newdata=trainS)
predictions_test <- predict(fit,newdata=testS)

#MSE for training and test data
MSE_training <- mean((trainS$ViolentCrimesPerPop-predictions_train)^2)
MSE_test <- mean((testS$ViolentCrimesPerPop-predictions_test)^2)
cat("MSE for the training data is :",MSE_training,"\n")
cat("MSE for the test data is :",MSE_test,"\n")

#Comment on the quality of model
R2 <- summary(fit)$r.squared
cat("The R^2 value for the model is: ",R2,"\n")

#Comment:The Mean Squared Error (MSE) for the training data is 0.2752071, indicating that the model performs well on the training set. However, the MSE for the test data is 0.4248011, which is higher than the training data MSE. This suggests that the model may be overfitting, as it performs worse and may not generalize as well on unseen data . The R² value is 0.7245166, meaning that the model explains approximately 72.4% of the variance in the target variable. 

#task4
#implement cost function without intercept
costfunction <- function(theta,training_X,training_y,test_X,test_y){
  #compute the mean squared error
  train_error<- mean((training_X%*%theta-training_y)^2)
  test_error <- mean((test_X%*%theta-test_y)^2)
  return(c(train_error,test_error))
}

training_X <- as.matrix(trainS[,-101])
training_y <- trainS$ViolentCrimesPerPop

test_X <- as.matrix(testS[,-101])
test_y <- testS$ViolentCrimesPerPop

#optimize theta and compute mse
theta_initial <- rep(0,ncol(training_X))


#create a df to store errors each iteration
errors_iteration_df <- data.frame(
  iteration=integer(),
  train_errors=numeric(),
  test_errors=numeric()
)

optim_theta <- optim(par=theta_initial,
                     fn=function(theta){
                       #compute training and test errors for each iteration
                       errors <- costfunction(theta,training_X,training_y,test_X,test_y)
                       
                       #store the errors 
                       errors_iteration_df <<- rbind(errors_iteration_df,data.frame(
                         iteration=nrow(errors_iteration_df)+1,
                         train_errors=errors[1],
                         test_errors=errors[2]
                       ))
                       
                       #return training errors since `optim()` uses training error 
                       return(errors[1])
                       
                     },
                    
                     method = "BFGS")
#print(errors_itteration_df)

#plot showing the dependence if both errors on the iteration number

ggplot(errors_iteration_df[errors_iteration_df$iteration >500,],aes(x=iteration))+   #discard the first 500 iterations to make it visible
  geom_line(aes(y=train_errors,color="Training Errors"))+
  geom_line(aes(y=test_errors,color="Test Errors"))+
  labs(title = "Training and test error during aech iteration",x="Iteration number",y="Errors")+
  scale_color_manual(values = c("blue","red"))+
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.2)) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

print(errors_iteration_df$train_errors)

#find optimal iteration
index <- which.min(errors_iteration_df[,3])
cat("The optimal iteration number is:",index,"\n")

#extract training and test error at optimal iteration
optimail_train_errors <- errors_iteration_df[index,2]
optimai_test_errors <- errors_iteration_df[index,3]


cat("Training error at optimal iteration",optimail_train_errors,"\n")
cat("Test error at optimal iteration",optimai_test_errors,"\n")
