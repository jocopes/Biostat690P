### Simulation study evaluating the efficiency of stratified, blocked
### design versus simple randomization
### Designs compared: (1)Stratified blocked Vs (2) Simple randomization
### Analysis method: (1) Simple t test (2) 2 factor ANOVA adjusting for stratification factor

### Function to generate data
GenerateData <- function(N, theta, mu, alpha, beta, sigma){
  ### Assume 2 strata: S=0 and S=1
  ### Assume 2 treatments: X=1 and X=0
  ### theta = Pr(S=1)
  ### Let Yi = response for the ith individual
  ### Assume Yi ~ N(mu + alpha*Si + beta*Xi, sigma^2)

  myS <- rep(0, N)
  myS[1:round(N*theta)] <- 1

  ## Treatment indicator based on simple randomization. expect 50% each
  randX <- runif(N)
  X1 <- rep(0, N)
  X1[randX < 0.5] <- 1

  ## Treatment indicator based on stratified, blocked randomization. guarenteeing we are getting balance of 1s and 0s
  #blocking guarentees. every 4th person, it shouldbe exactly 50/50
  X2 <- rep(0, N)
  X2S1 <- X2[myS == 1]
  X2S0 <- X2[myS == 0]

  n1 <- length(X2S1)
  n2 <- length(X2S0)

  X2S1[1:round(n1/2)] <- 1 #giving first half to the 1 treatment group assignment
  X2S0[1:round(n2/2)] <- 1

  X2[myS == 1] <- X2S1
  X2[myS == 0] <- X2S0

  ## Generate outcome based on simple randomization
  Y1 <- rnorm(n=N, mean=mu + alpha*myS + beta*X1, sd = sigma)

  ## Generate outcome based on stratified, blocked randomization
  Y2 <- rnorm(n=N, mean=mu + alpha*myS + beta*X2, sd = sigma)

  mydat1 <- cbind(Y1, myS, X1)
  mydat2 <- cbind(Y2, myS, X2)
  mydat <- list(data1 = mydat1, data2= mydat2)
  return(mydat)
}

### Defining simulation parameters
set.seed(1234)
NumDataset <- 1000
N.vec <- c(20, 30, 100, 500)
theta <- 0.40
mu <- 0.1
alpha <- 1.5
beta <- -0.5
sigma <- 1.0
mypow <- c()
betahat <- c()

### Starting simulation

for (j in 1:4){
 N <- N.vec[j]
  mypvals <- c()
  mybetas <- c()

  for (i in 1: NumDataset){
    #print(i)

    mydat <- GenerateData(N, theta, mu, alpha, beta, sigma)
    mydat1 <- mydat$data1
    mydat2 <- mydat$data2

    ### Simple randomization, t test
    pval11 <- t.test(mydat1[,1]~mydat1[,3])$p.value
    est11 <- mean(mydat1[,1][mydat1[,3] == 1]) -  mean(mydat1[,1][mydat1[,3] == 0])

    ### Simple randomization, adjusted analysis
    mymod <- lm(mydat1[,1] ~ mydat1[,2] + mydat1[,3])
    est12 <- mymod$coef[3]
    pval12 <- summary(mymod)$coefficients[3,4]

    ### Stratified, blocked randomization, t test
    pval21 <- t.test(mydat2[,1]~mydat2[,3])$p.value
    est21 <- mean(mydat2[,1][mydat2[,3] == 1]) -  mean(mydat2[,1][mydat2[,3] == 0])

    ### Stratified, blocked randomization, adjusted analysis
    mymod <- lm(mydat2[,1] ~ mydat2[,2] + mydat2[,3])
    est22 <- mymod$coef[3]
    pval22 <- summary(mymod)$coefficients[3,4]

    mypvals <- rbind(mypvals, c(pval11, pval12, pval21, pval22))
    mybetas <- rbind(mybetas, c(est11, est12, est21, est22))
  }

  mypow <- rbind(mypow, c(sum(mypvals[,1] < 0.05), sum(mypvals[,2] < 0.05), sum(mypvals[,3] < 0.05), sum(mypvals[,4] < 0.05))/NumDataset)
  betahat <- rbind(betahat, c(mean(mybetas[,1]), mean(mybetas[,2]),mean(mybetas[,3]),mean(mybetas[,4])))
}

### SR=simple randomization; STR=stratified, blocked randomization
##  T=t test; LM=linear model

colnames(mypow) <- c("SR-T", "SR-LM", "STR-T", "STR-LM")
colnames(betahat) <- c("SR-T", "SR-LM", "STR-T", "STR-LM")
print(mypow)
print(betahat)
