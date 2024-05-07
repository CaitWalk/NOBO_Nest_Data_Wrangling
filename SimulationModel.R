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
  ic = numeric()  #initical capture
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


print(marked)
mean(total_recap)
sum(ic)
#################################################################################
n.occasions = 3
phi = 0.45
p = 0.20 #recapture probability 
marked = sum(ic)  #initally captured i.e marked eggshells

#define matrices with survival and recapture prob
PHI <- matrix(phi, ncol = n.occasions - 1, nrow = marked)
P <- matrix(p, ncol = n.occasions - 1, nrow = marked)


# Define function to simulate a capture-history (CH) matrix
simul.cjs <- function(PHI, P, marked, n.occasions){
  CH <- matrix(0, ncol = n.occasions, nrow = marked)
 
   # Define a vector with the occasion of marking
  mark.occ <- rep(1:marked, each = marked)
  
   # Fill the CH matrix
  for (i in 1:marked){
    CH[i, mark.occ[i]] <- 1       # Write an 1 at the release occasion
    if (mark.occ[i]==n.occasions) next
    for (t in (mark.occ[i]+1):n.occasions){
      # Bernoulli trial: does individual survive occasion?
      sur <- rbinom(1, 1, PHI[i, t - 1])
      if (sur==0) break		# If dead, move to next individual 
      # Bernoulli trial: is individual recaptured? 
      rp <- rbinom(1, 1, P[i, t - 1])
      if (rp==1) CH[i,t] <- 1
    } #t
  } #i
  return(CH)
}

simul.cjs(phi, p, marked, n.occasions)

n.occasions <- dim(PHI)[2] + 1

###################################################################
#origingal function from internet
# Define function to simulate a capture-history (CH) matrix
#simul.cjs <- function(PHI, P, marked){
  #n.occasions <- dim(PHI)[2] + 1
 # CH <- matrix(0, ncol = n.occasions, nrow = sum(marked))
  # Define a vector with the occasion of marking
 # mark.occ <- rep(1:length(marked), marked[1:length(marked)])
  # Fill the CH matrix
  #for (i in 1:sum(marked)){
    #CH[i, mark.occ[i]] <- 1       # Write an 1 at the release occasion
   # if (mark.occ[i]==n.occasions) next
   # for (t in (mark.occ[i]+1):n.occasions){
      # Bernoulli trial: does individual survive occasion?
    #  sur <- rbinom(1, 1, PHI[i,t-1])
   #   if (sur==0) break		# If dead, move to next individual 
      # Bernoulli trial: is individual recaptured? 
    #  rp <- rbinom(1, 1, P[i,t-1])
   #   if (rp==1) CH[i,t] <- 1
  #  } #t
#  } #i
#  return(CH)
#}
#simul.cjs(phi, p, marked)
#Error in matrix(0, ncol = n.occasions, nrow = sum(marked)) : 
  #invalid 'ncol' value (too large or NA)