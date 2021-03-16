
# ####################################
# BIOS690P, Spring 2021
# R code for HW1, Question 4
# ####################################


###Newton's method
#this is an R function to calcuate score function and observed information under a logistic regression model
score.fun<-function(a0, a1, dat){
###input variables 
#a0: the estimate for beta0
#a1: the estimate for beta1
#dat: the data set
 n<-nrow(dat)
 x.mat<-as.matrix(cbind(rep(1,n), dat$x)) #n x 2 design matrix
 beta.0<-as.matrix(c(a0, a1), 2,1) #2x1 regression coefficent vector
 p<-1/(1+exp(-x.mat%*%beta.0))
 #output score function, 2x1 vector
 lkhd.score=t(x.mat)%*%(dat$y - p) #likelihood score function
 obs.info=t(x.mat)%*%(x.mat*cbind(p*(1-p), p*(1-p))) #observed information
#output the likelihood score and observed information as a list 
 list(lkhd.score=lkhd.score, obs.info=obs.info)
}





