### Example of how to set up a simulation study
### Setting up a simulation study to compare the T test to the wilcoxon test


GenerateData <- function(N, theta, mu, beta, sigma, alpha){
  ### N is the total sample size of the study
  ### Assume 2 treatments: X=1 and X=0, where subjects are randomly assigned with probability theta to treatment 1.
  ### Let Yi = response for the ith individual
  ### Assume Yi ~ N(mu + beta*Xi + alpha*Si, sigma^2)

  ## X1 is the treatment indicator based on simple randomization
  randX <- runif(N)
  X1 <- rep(0, N)
  X1[randX < theta] <- 1

  ## Generate outcome based on model stated above
  Y1 <- rnorm(n=N, mean=mu + beta*X1 + alpha*S1, sd = sigma)

  ## Create dataset, where first col is outcome, second column is treatment indicator
  mydat1 <- cbind(Y1, X1)
  return(mydat1)
}



NumDataset <- 100
N <- 25
theta <- 0.40
mu <- 0.1
beta <- 0.5
sigma <- 2.5

mypvals <- c()
mybetas <- c()

for (i in 1: NumDataset){
    print(i)

    mydat1 <- GenerateData(N, theta, mu,  beta, sigma)

    ### T test
    pval1 <- t.test(mydat1[,1]~mydat1[,2])$p.value
    est1 <- mean(mydat1[,1][mydat1[,2] == 1]) -  mean(mydat1[,1][mydat1[,2] == 0])

    ### Wilcoxon test
    pval2 <- wilcox.test(mydat1[,1]~mydat1[,2])$p.value
    est2 <- median(mydat1[,1][mydat1[,2] == 1]) -  median(mydat1[,1][mydat1[,2] == 0])

    ### Save the pvalues from each dataset from both t test and wilcoxon test
    mypvals <- rbind(mypvals, c(pval1, pval2))
    ### Save the treatment effect estimates from each dataset
    mybetas <- rbind(mybetas, c(est1, est2))
  }


### Power of each test statistic is estimated as  the proportion of datasets resulting in a p value less than 0.05
mypow <- c(sum(mypvals[,1] < 0.05)/NumDataset, sum(mypvals[,2] < 0.05)/NumDataset)
### Average treatment effect according to each estimator
mytreateffect <- c(mean(mybetas[,1]), mean(mybetas[,2]))

## Power
mypow

### Treatment effect
mytreateffect
