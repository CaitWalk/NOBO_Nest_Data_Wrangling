library(ggplot2)
library(tidyr)
library(dplyr)
library(ape)
library(tensorA)
library(corpcor)
library(cubature)
library(MCMCglmm)

  # N = represents the number of females alive at the start of the breeding season
  # fn = represents the probability that a female nests
  # ns = represents the probability that a nest is successful
  # cs = is the average clutch size
  # ha = the number of eggs that hatch out/total successful clutch count
  # gs = is the genotyping success rate for eggshells 

#################################################################################
num.iterations <- 1000    #number of times to run the code 

breeding_sample_size = 75
nest_success_prob = 0.5
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


mean(total_recap)
sum(ic)
#################################################################################
# Define parameter values
n.occasions <- 3                   # Number of capture occasions
marked <- 315   # number of ic from above code (cant find a way to link btw code--hard code the number in)
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


###################################################################
#Bayesian population analysis using WinBUGS: a hierachical perspective
#function from internet
#https://www.vogelwarte.ch/modx/assets/files/publications/BPA/BPA%20with%20JAGS.txt

# Define parameter values
n.occasions <- 6                   # Number of capture occasions
marked <- rep(50, n.occasions-1)   # Annual number of newly marked individuals
phi <- rep(0.65, n.occasions-1)
p <- rep(0.4, n.occasions-1)

# Define matrices with survival and recapture probabilities
PHI <- matrix(phi, ncol = n.occasions-1, nrow = sum(marked))
P <- matrix(p, ncol = n.occasions-1, nrow = sum(marked))

# Define function to simulate a capture-history (CH) matrix
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