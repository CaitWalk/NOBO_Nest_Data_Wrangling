library(ggplot2)
library(tidyr)
library(dplyr)
library(ape)
library(tensorA)
library(corpcor)
library(cubature)
library(MCMCglmm)
library(lattice)
library(coda)
library(rjags)
library(R2WinBUGS)
library(R2jags)


  # N = represents the number of females alive at the start of the breeding season
  # fn = represents the probability that a female nests
  # ns = represents the probability that a nest is successful
  # cs = is the average clutch size
  # ha = the number of eggs that hatch out/total successful clutch count
  # gs = is the genotyping success rate for eggshells 

#################################################################################
num.iterations <- 1000    #number of times to run the code 

breeding_sample_size = 200
nest_success_prob = 0.3
hatch_prob = 0.85
genotype_success = 0.95
capture_prob = 0.10

total_recap = numeric()

# loop to run recapture loop 
for (iterations in 1:num.iterations) {
  fn = numeric()
  ns = numeric()
  cs = numeric()
  chicks = numeric()
  ic= numeric()  #initical capture
  recap = numeric()
  af = numeric()
  
  #loop to calculate recapture numbers
  for (i in 1:breeding_sample_size) {
    fn [i] = rpois(1,1.1)     #nests/female
    ns [i] = rbinom(1, fn[i], nest_success_prob)  #nest success
    cs [i] = rpois(1, 12)    #clutch size
    chicks [i] = rbinom(1, ns[i] * cs, hatch_prob)  #number of chicks hatched
    ic [i] = rbinom(1, chicks[i], genotype_success)   #number of eggs genotyped
    af [i] = rbinom(1, ic[i], 0.4)    #number of chicks alive in the fall
    recap [i] = rbinom(1, af[i], capture_prob)    #number recaptured in the fall
  }
  
  total_recap[iterations] = sum(recap)
}


print(ic)
sum(ic)
#################################################################################
##      Capture history matrix
#https://www.vogelwarte.ch/modx/assets/files/publications/BPA/BPA%20with%20JAGS.txt
# Define parameter values
n.occasions <- 3                   # Number of capture occasions
marked <- sum(ic)   # number of ic from above code (cant find a way to link btw code--hard code the number in)
phi <- rep(0.45, n.occasions-1)
p <- rep(0.2, n.occasions-1)

# Define matrices with survival and recapture probabilities
PHI <- matrix(phi, ncol = n.occasions-1, nrow = sum(marked))
P <- matrix(p, ncol = n.occasions-1, nrow = sum(marked))

simul.cjs <- function(PHI, P, marked){
  n.occasions <- dim(PHI)[2] + 1
  CH <- matrix(0, ncol = n.occasions, nrow = sum(marked))
  # Define a vector with the occasion of marking
  mark.occ <- rep(1:length(marked), marked[1:length(marked)])
  # Fill the CH matrix
  for (i in 1:sum(marked)){
    CH[i, mark.occ[i]] <- 1       # Write an 1 at the release occasion
    if (mark.occ[i]==n.occasions) next
    for (t in (mark.occ[i]+1):n.occasions){
      # Bernoulli trial: does individual survive occasion?
      sur <- rbinom(1, 1, PHI[i,t-1])
      if (sur==0) break		# If dead, move to next individual 
      # Bernoulli trial: is individual recaptured? 
      rp <- rbinom(1, 1, P[i,t-1])
      if (rp==1) CH[i,t] <- 1
    } #t
  } #i
  return(CH)
}

# Execute function
CH <- simul.cjs(PHI, P, marked)

print(CH)

colSums(CH)
###################################################################
# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)

# Specify model in BUGS language
sink("cjs-c-c.jags")
cat("
model {

# Priors and constraints
for (i in 1:nind){
   for (t in f[i]:(n.occasions-1)){
      phi[i,t] <- mean.phi
      p[i,t] <- mean.p
      } #t
   } #i

mean.phi ~ dunif(0, 1)         # Prior for mean survival
mean.p ~ dunif(0, 1)           # Prior for mean recapture

# Likelihood 
for (i in 1:nind){
   # Define latent state at first capture
   z[i,f[i]] <- 1
   for (t in (f[i]+1):n.occasions){
      # State process
      z[i,t] ~ dbern(mu1[i,t])
      mu1[i,t] <- phi[i,t-1] * z[i,t-1]
      # Observation process
      y[i,t] ~ dbern(mu2[i,t])
      mu2[i,t] <- p[i,t-1] * z[i,t]
      } #t
   } #i
}
",fill = TRUE)
sink()

# Bundle data
jags.data <- list(y = CH, f = f, nind = dim(CH)[1], n.occasions = dim(CH)[2])

# Initial values
# In JAGS we have to give good initial values for the latent state z. At all occasions when an individual was observed, its state is z = 1 for sure. In addition, if an individual was not observed at an occasion, but was alive for sure, because it was observed before and thereafter (i.e. has a capture history of e.g. {101} or {10001}), then we know that the individual was alive at all of these occasions, and thus z = 1. Therefore, we should provide initial values of z = 1 at these positions as well. The following function provides such initial values from the observed capture histories:
known.state.cjs <- function(ch){
  state <- ch
  for (i in 1:dim(ch)[1]){
    n1 <- min(which(ch[i,]==1))
    n2 <- max(which(ch[i,]==1))
    state[i,n1:n2] <- 1
    state[i,n1] <- NA
  }
  state[state==0] <- NA
  return(state)
}

# (Note that the function known.state.cjs is used in section 7.3.1 as well for another purpose) 

inits <- function(){list(mean.phi = runif(1, 0, 1), mean.p = runif(1, 0, 1), z = known.state.cjs(CH))}

# Parameters monitored
parameters <- c("mean.phi", "mean.p")

# MCMC settings
ni <- 10000
nt <- 6
nb <- 5000
nc <- 3

# Call JAGS from R (BRT 1 min)
cjs.c.c <- jags(jags.data, inits, parameters, "cjs-c-c.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())

# Summarize posteriors
print(cjs.c.c, digits = 3)

