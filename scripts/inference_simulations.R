# STATISTICAL SIMULATIONS

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
# Example use: y<-phats_exp(0.2,40,1000)
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
coverage <- sapply(pvals, function(p){
#coverage <- sapply(lambdas, function(p){
  # my exponential simulator phats <- phats_exp(nosim,p,n)
  phats <- rbinom(n, size=nosim,p)/n
#  print (phats)
  ll <- phats - qnorm(0.975)/(p*sqrt(n))
  ul <- phats + qnorm(0.975)/(p*sqrt(n))
  mean(ll>1/p & ul<1/p)
})
