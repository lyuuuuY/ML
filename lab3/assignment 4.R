library(neuralnet)
set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
n_input <- 1        
n_hidden <- 10      
n_output <- 1      

weights_input_to_hidden <- n_input * n_hidden
weights_hidden_to_output <- n_hidden * n_output
n_weights <- weights_input_to_hidden + weights_hidden_to_output
bias_hidden <- n_hidden  
bias_output <- n_output
n_weights <- n_weights + bias_hidden + bias_output
winit <- runif(n_weights, min = -1, max = 1)

nn <- neuralnet(Sin ~ Var, data = tr, hidden = 10, act.fct = "logistic", linear.output = TRUE,
                startweights = list(
                  matrix(winit[1:weights_input_to_hidden], nrow = n_input, ncol = n_hidden),
                  matrix(winit[(weights_input_to_hidden + 1):(weights_input_to_hidden + weights_hidden_to_output)],
                         nrow = n_hidden, ncol = n_output)
                ))
    # Plot of the training data (black), test data (blue), and predictions (red)
    plot(tr, cex=2)
    points(te, col = "blue", cex=1)
    points(te[,1],predict(nn,te), col="red", cex=1)
    
    # Comment your results
    
# task 2
    
activation_functions <- list(
      linear = function(x) x,
      relu = function(x) ifelse(x > 0, x, 0),
      softplus = function(x) log(1 + exp(x))
    )
## linear
nn_lin <- neuralnet(Sin ~ Var, data = tr, hidden = 10, act.fct = activation_functions[["linear"]],
               linear.output = TRUE,startweights = list(
                 matrix(winit[1:weights_input_to_hidden], nrow = n_input, ncol = n_hidden),
                 matrix(winit[(weights_input_to_hidden + 1):(weights_input_to_hidden + weights_hidden_to_output)],
                        nrow = n_hidden, ncol = n_output)
               ))
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_lin,te), col="red", cex=1)

## relu

nn_relu <- neuralnet(Sin ~ Var, data = tr, hidden = 10, act.fct = activation_functions[["relu"]],
                    linear.output = TRUE,startweights = list(
                      matrix(winit[1:weights_input_to_hidden], nrow = n_input, ncol = n_hidden),
                      matrix(winit[(weights_input_to_hidden + 1):(weights_input_to_hidden + weights_hidden_to_output)],
                             nrow = n_hidden, ncol = n_output)
                    ))
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_relu,te), col="red", cex=1)

## softplus

nn_softplus <- neuralnet(Sin ~ Var, data = tr, hidden = 10, act.fct = activation_functions[["softplus"]],
                     linear.output = TRUE,startweights = list(
                       matrix(winit[1:weights_input_to_hidden], nrow = n_input, ncol = n_hidden),
                       matrix(winit[(weights_input_to_hidden + 1):(weights_input_to_hidden + weights_hidden_to_output)],
                              nrow = n_hidden, ncol = n_output)
                     ))
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_softplus,te), col="red", cex=1)

## comment your results

# task 3

new_Var <- runif(500, 0, 50)
new_data <- data.frame(Var = new_Var, Sin = sin(new_Var))

new_predictions <- predict(nn, new_data)

plot(new_data$Var, new_data$Sin, col = "blue", main = "Neural Network Predictions on [0, 50]",
     xlab = "x", ylab = "sin(x)", cex = 1, pch = 16,ylim=c(-4,1.5))
points(new_data$Var, predict(nn, new_data), col = "red", cex = 1, pch = 16)
legend("topright", legend = c("True Values", "Predictions"),
       col = c("blue", "red"), pch = 16)


# task 4

weights <- nn$weights
print(weights)

# comment

# task 5

Var_inv <- runif(500, 0, 10)  
Sin_inver <- sin(Var_inv)           
data_inver <- data.frame(Sin_inver, Var_inv)  

nn_inver <- neuralnet(Var_inv~Sin_inver, data = data_inver, hidden = 10, threshold=0.1,linear.output = TRUE,
                startweights = list(
                  matrix(winit[1:weights_input_to_hidden], nrow = n_input, ncol = n_hidden),
                  matrix(winit[(weights_input_to_hidden + 1):(weights_input_to_hidden + weights_hidden_to_output)],
                         nrow = n_hidden, ncol = n_output)
                ))

predictions_inver <- predict(nn_inver, data_inver)

plot(data_inver$Sin_inver, data_inver$Var_inv, col = "blue", main = "Neural Network Predictions: sin(x) -> x",
     xlab = "sin(x)", ylab = "x", cex = 1, pch = 16,ylim = c(-1,10))
points(data_inver$Sin_inver, predictions_inver, col = "red", cex = 1, pch = 16)

