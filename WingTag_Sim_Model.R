#################################################################################################################
#                           Simulation model for estimating chick survival using WINGTAG
#################################################################################################################

# Define parameter values
n.occasions.wing <- 3                         # Number of capture occasions
marked.wing <- 100                            # number chicks wing tagged 
phi.wing <- rep(0.45, n.occasions.wing-1)     # survival 
p.wing <- rep(0.2, n.occasions.wing-1)        # recapture

# Define matrices with survival and recapture probabilities
PHI.wing <- matrix(phi.wing, ncol = n.occasions.wing-1, nrow = marked.wing)
P.wing <- matrix(p.wing, ncol = n.occasions.wing-1, nrow = marked.wing)

simul.cjs.wing <- function(PHI.wing, P.wing, marked.wing){
  n.occasions.wing <- dim(PHI.wing)[2] + 1
  CH.wing <- matrix(0, ncol = n.occasions.wing, nrow = marked.wing)
  # Define a vector with the occasion of marking
  mark.occ <- rep(1:length(marked.wing), marked.wing[1:length(marked.wing)])
  # Fill the CH matrix
  for (i in 1:sum(marked.wing)){
    CH.wing[i, mark.occ[i]] <- 1       # Write an 1 at the release occasion
    if (mark.occ[i]==n.occasions.wing) next
    for (t in (mark.occ[i]+1):n.occasions.wing){
      # Bernoulli trial: does individual survive occasion?
      sur <- rbinom(1, 1, PHI.wing[i,t-1])
      if (sur==0) break		# If dead, move to next individual 
      # Bernoulli trial: is individual recaptured? 
      rp <- rbinom(1, 1, P.wing[i,t-1])
      if (rp==1) CH.wing[i,t] <- 1
    } #t
  } #i
  return(CH.wing)
}
# Execute function
#CH.wing <- simul.cjs.wing (PHI.wing, P.wing, marked.wing)

#number of simulations
n.sim.wing <- 1000

#storage for posterior means
results.wing <- data.frame(sim = 1:n.sim.wing,
                           mean.phi = NA,
                           mean.p = NA)

#Simulation loop to rerun multiple times
for (s in 1:n.sim.wing) {
  
  # simulate capture history
  CH.wing <- simul.cjs.wing (PHI.wing, P.wing, marked.wing)
  
  # Create vector with occasion of marking (i.e. first capture)
  f.wing <- apply(CH.wing, 1, get.first)
  
  # Bundle data 
  jags.data.wing <- list(
    y = CH.wing, 
    f = f.wing, 
    nind = dim(CH.wing)[1], 
    n.occasions = dim(CH.wing)[2]
  )
  
  #initial values
  inits.wing <- function(){
    list(
      mean.phi = runif(1, 0, 1), 
      mean.p = runif(1, 0, 1), 
      z = known.state.cjs(CH.wing)
    )
  }
  
  # run model
  cjs.wing <- jags(jags.data.wing, inits.wing, parameters, 
                   "cjs-c-c.jags", n.chains = nc, 
                   n.thin = nt, n.iter = ni, n.burnin = nb, 
                   working.directory = getwd())
  # Store posterior means
  results.wing$mean.phi[s] <- cjs.wing$BUGSoutput$mean$mean.phi
  results.wing$mean.p[s]   <- cjs.wing$BUGSoutput$mean$mean.p
  
  cat("Completed simulation", s, "\n")
}

# Summarize posteriors
print(cjs.wing, digits = 3)
summary(results.wing)
boxplot(results.wing$mean.phi, main = "Estimated survival (phi)")
boxplot(results.wing$mean.p, main = "Estimated recapture (p)")