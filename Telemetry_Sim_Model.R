library(R2WinBUGS) #needed for cjs. code
library(R2jags) #needed to run the jags code
library(ggplot2)
library(tidyr)
library(dplyr)
library(coda) #required for rjags package
library(rjags)

# Specify model in BUGS language
sink("cjs-c.c.jags")
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

get.first <- function(x) min(which(x!=0))
# Initial values
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

# Parameters monitored
parameters <- c("mean.phi", "mean.p")

# MCMC settings
ni <- 10000
nt <- 6
nb <- 5000
nc <- 3

#################################################################################################################
#               known fate simulation model for estimating chick survival using telemetry
#################################################################################################################
# Define parameter values
n.occasions.track <- 90                         # Number of tracking occasions
marked.track <- 85                              # number of chicks radioed
phi.track <- rep(0.98, n.occasions.track-1)     # daily survival rate
p.track <- rep(0.2, n.occasions.track-1)        # recapture

# Define matrices with survival and recapture probabilities
PHI.track <- matrix(phi.track, ncol = n.occasions.track-1, nrow = marked.track)
P.track <- 1

simul.known.fate <- function(PHI.track, P.track, marked.track){
  n.occasions.track <- dim(PHI.track)[2] + 1
  CH.track <- matrix(0, ncol = n.occasions.track, nrow = marked.track)
  # Define a vector with the occasion of marking
  mark.occ <- rep(1:length(marked), marked.track[1:length(marked.track)])
  # Fill the CH matrix
  for (i in 1:sum(marked.track)){
    CH.track[i, mark.occ[i]] <- 1       # Write an 1 at the release occasion
    if (mark.occ[i]==n.occasions.track) next
    for (t in (mark.occ[i]+1):n.occasions.track){
      # Bernoulli trial: does individual survive occasion?
      sur <- rbinom(1, 1, PHI.track[i,t-1])
      if (sur==0) break		# If dead, move to next individual 
      # Bernoulli trial: is individual recaptured? 
      rp <- rbinom(1, 1, P.track)
      if (rp==1) CH.track[i,t] <- 1
    } #t
  } #i
  return(CH.track)
}
# Execute function
CH.track <- simul.known.fate (PHI, P.track, marked.track)

print(CH.track)

colSums(CH.track)
################################################################################
#number of simulations
n.sim.track <- 1000

#storage for posterior means
results.track <- data.frame(sim = 1:n.sim.track,
                            mean.phi = NA,
                            mean.p = NA)


#Simulation loop to rerun multiple times
for (s in 1:n.sim.track) {
  #simulate capture history
  CH.track <- simul.known.fate (PHI, P.track, marked.track)
  
  #first marking occasion
  f.track <- apply(CH.track, 1, get.first)
  
  # Bundle data
  jags.data.track <- list(
    y = CH.track, 
    f = f.track, 
    nind = dim(CH.track)[1], 
    n.occasions = dim(CH.track)[2]
  )
  
  #initial values function  
  inits.track <- function(){list(
    mean.phi = runif(1, 0, 1), 
    mean.p = runif(1, 0, 1), 
    z = known.state.cjs(CH.track)
  )
  }
  
  # Run JAGS from R 
  cjs.track <- jags(jags.data.track, inits.track, parameters, 
                    "cjs-c-c.jags", n.chains = nc, n.thin = nt, 
                    n.iter = ni, n.burnin = nb, 
                    working.directory = getwd())
  
  
  # Store posterior means
  results.track$mean.phi[s] <- cjs.track$BUGSoutput$mean$mean.phi
  results.track$mean.p[s]   <- cjs.track$BUGSoutput$mean$mean.p
  
  cat("Completed simulation", s, "\n")
}

# Summarize posteriors
print(cjs.track, digits = 3)
summary(results.track)
boxplot(results.track$mean.phi, main = "Estimated survival (phi)")
boxplot(results.track$mean.p, main = "Estimated recapture (p)")


