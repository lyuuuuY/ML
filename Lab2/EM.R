set.seed(1234567890)
max_it <- 200 # max number of EM iterations
min_change <- 0.001 # min change in log lik between two consecutive iterations
n=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=n, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(i in 1:n) {
  m <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[i,d] <- rbinom(1,1,true_mu[m,d])
  }
}

M=3 # number of clusters
w <- matrix(nrow=n, ncol=M) # weights
pi <- vector(length = M) # mixing coefficients
mu <- matrix(nrow=M, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the parameters
pi <- runif(M,0.49,0.51)
pi <- pi / sum(pi)
for(m in 1:M) {
  mu[m,] <- runif(D,0.49,0.51)
}
pi
mu
for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the weights
  
  #Bern <- vector(length = n)
  #epsilon <- 1e-10
  #mu <- pmax(mu, epsilon)
  #mu <- pmin(mu, 1 - epsilon)
  
  p_x <- numeric(length = n)
  for (i in 1:n) {
    pi_bern <- numeric(M)  
    Bern <- rep(1, M) 
    max_Bern <- 0
    for (m in 1:M){
      Bern[m] <- 1
      for (j in 1:D) {
        if(x[i,j]==1){
          Bern[m] <- Bern[m]*mu[m,j]
        }else{Bern[m] <- Bern[m]*(1-mu[m,j])}
      }
      pi_bern[m] <- pi[m]*Bern[m]
      p_x[i] <- p_x[i] + pi_bern[m]
    }
    for (m in 1:M) {
      w[i,m] <- pi_bern[m]/p_x[i]
    }
    
  }
  

  
  #Log likelihood computation.
  
  log_p_x <- log(p_x)
  llik[it] <- sum(log_p_x)
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  
  if(it > 1 && abs(llik[it] - llik[it - 1]) <= min_change){
    print(it)
    break
  }
    
  
  
  #M-step: ML parameter estimation from the data and weights
  for (m in 1:M) {
    N_m <- sum(w[,m])
    pi[m] <- N_m / n
    for (d in 1:D) {
      mu[m,d] <- sum(w[,m] * x[,d]) / N_m
    }
    
  }
}
  

pi
mu
plot(llik[1:it], type="o")

