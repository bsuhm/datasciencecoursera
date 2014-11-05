# STATISTICAL SIMULATIONS

# plot histogram of empirical distribution
# library(ggplot2)
# g<-ggplot(data.frame(y-axis=data),aes(x=data))
# g<-g+geom_histogram(color=, fill=, binwidth=)
# g


# simulate binomial distribution b(n,p) n_sim times
# Returns distribution of outcomes (total count how many
#   time i, 1:n was the outcome)
#
# Example use: y<-simulate_binom(0.2,40,1000)
#              plot(x,y[1:20],type="h")

simulate_binom <- function(p,n,n_sim) {
  
  # perform n_sim applications of b(p,n) and gather outcome of each
  outcomes <- rbinom(n_sim, prob = p, size = n)
  
  # create empirical distribution
  count <- integer(n)
  for (i in 1:n_sim) count[outcomes[i]] <- count[outcomes[i]] +1
  
  count
}

# simulate exponential distribution exp(lambda) n_sim times
# computing averages over n simulations each
#
# Example use: y<-phats_exp(1000,0.2,40)
#              plot(x,y[1:20],type="h")
phats_exp <- function(n_sim,lambda,n) {
  
  # as prep for using sapply, build up vector of number of executions
  ns<-c(n)
  for (i in 2:n_sim) ns<-c(n,ns)

  # perform n_sim applications of rexp(n,lambda) and gather outcome of each
  outcomes <- sapply(ns, function(x) rexp(x, lambda))
  
  # computer average of each set of n
  avg <- numeric(n_sim)
  for (i in 1:n_sim) {
    #print(outcomes[,i])
    avg[i] <- mean(outcomes[,i])
  }
  avg
}

# returns confidence interval on mean of observations <vals> for <level>
calc_ci <- function(vals,level) {
  u <- mean(vals)
  sd <- sd(vals)
  q<-qnorm(level)
  n<-length(vals)
  
  # upper and lower CI boundary
  delta <- sd*q/sqrt(n-1)

  return(c(u-delta,u+delta))
}

# simulate CIs on exponention distribution, executing it n times each,
# returning  data frame with rows "lower" and "upper" containing the limits
# for n_sim simulations
gen_cis <- function(n,n_sim,lambda) {
  in_ci <- logical(n)
  ci <- data.frame(row.names=c("Lower","Upper"))
  for (i in 1:n) {
    set.seed(i)
    z <- rexp(n,lambda)
    ci[i] <- calc_ci(z,0.975)
  }
}

# params that need defined in the current environment (!)
#  n=averaging over, nosim=#simulations, 
# lambdas=vector of distribution params
# this one for binomial coverage <- sapply(pvals, function(p){
# need defined: lambdas, n,nosim
coverage <- sapply(pvals, function(p){
  # this one for exponential  
  phats <- phats_exp(nosim,p,n)
  sd <- 1/p
  # this one for binompial 
  #phats <- rbinom(nosim, prob=p, size=n)/n
  #sd <- sqrt(phats*(1-phats))
  ll <- phats - qnorm(0.975)*sd/sqrt(n)
  ul <- phats + qnorm(0.975)*sd/sqrt(n)
  # binom mean(ll<p & ul>p)
  mean(ll<1/p & ul>1/p)
})

# explore power of a testical test
#library(manipulate)
#mu0 = 30
myplot <- function(sigma, mua, n, alpha) {
  g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
  g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mu0, 
                                                                sd = sigma/sqrt(n)), size = 2, col = "red")
  g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mua, 
                                                                sd = sigma/sqrt(n)), size = 2, col = "blue")
  xitc = mu0 + qnorm(1 - alpha) * sigma/sqrt(n)
  g = g + geom_vline(xintercept = xitc, size = 3)
  g
}
#manipulate(myplot(sigma, mua, n, alpha), sigma = slider(1, 10, step = 1, initial = 4), 
#           mua = slider(30, 35, step = 1, initial = 32), n = slider(1, 50, step = 1, 
#                                                                    initial = 16), alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05))

# model agnostic standard errors: attempt to construct stderr
# without assumptions on model, but does assume mean is correct
# and large n
# see http://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval
# library(sandwich)
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
  cf <- coef(object); pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2; a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}

# scatterplot smoothing
# fitting complex functions with quadratic linear models
#splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
#xMat <- cbind(1, x, x^2, splineTerms)
#yhat <- predict(lm(y ~ xMat - 1))
#plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
#lines(x, yhat, col = "red", lwd = 2)

# bootstrapping example: resample height variable from father.son data set
# to bootstrap its median
#data(father.son)
# x<-father.son$sheight
# help(father.son)
# n<-length(x)
# B<-10000
# resamples<-matrix(sample(x,n*B,replace=TRUE),B,n)
# resampledMeans<-apply(resamples,1,median)
# hist(resampledMeans,breaks=40)