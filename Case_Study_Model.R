## Model for the case study part where I supply real data from TT and LP to my
#  model from the simulations


# Load packages
library(nimble)
library(parallel)

# PARAMETERS ----------------------------------
winter_dsr <- 0.9966302
summer_dsr <- 0.9951 # Rough values of adult breeding dsr from Albany

ni <- 2500
nt <- 1
nb <- 500
nc <- 3

# Filter out only needed columns (assuming column 10 is site, and 11-20 are capture history)
real_data_subset <- Real_ch[, c(10, 11:20)]
colnames(real_data_subset)[1] <- "site"
sites <- unique(real_data_subset$site)

# Results storage
site_results <- list()

# Function to find first time seen (alive or harvested)
get_first <- function(x) which(x %in% c(1, 3))[1]

# Loop over sites
for (site_name in sites) {
  site_data <- real_data_subset %>% filter(site == !!site)
  ch_site <- as.matrix(site_data[, -1])  # Remove 'site' column
  
  f <- apply(ch_site, 1, get_first)
  
  ch <- ch_site  
  N_tot <- nrow(ch)
  n.occasions <- ncol(ch)
  
  # First and last occasion where each individual is seen
  first <- apply(ch != 2, 1, function(x) which(x)[1])   # first non-2 (seen alive or dead)
  last.track <- apply(ch != 2, 1, function(x) tail(which(x), 1))  # last non-2
  
  # -------------------------
  # z.data: known true states (only what's observed)
  # -------------------------
  z.data <- matrix(NA, nrow = N_tot, ncol = n.occasions)
  
  for (i in 1:N_tot) {
    if (first[i] < last[i]) {
      for (t in (first[i]+1):last[i]) {
      obs <- ch[i, t]
      if (obs %in% c(1, 3)) {
        z.data[i, t] <- obs
      } else {
        z.data[i, t] <- 1  # assume alive between known observations
      }
      if (!is.na(z.data[i, t]) && z.data[i, t] == 3) {
        if (t < n.occasions) {
          z.data[i, (t + 1):n.occasions] <- NA
        }
        break
      }
    }
  }
  
  # -------------------------
  # z.init: initial values for latent state model
  # -------------------------
  z.init <- matrix(NA, nrow = N_tot, ncol = n.occasions)
  
  for (i in 1:N_tot) {
    for (t in first[i]:last.track[i]) {
      obs <- ch[i, t]
      if (obs == 1) {
        z.init[i, t] <- 1
      } else if (obs == 3) {
        z.init[i, t] <- 3
      } else {
        # Not observed → assume alive unless previously dead
        if (t == first[i]) {
          z.init[i, t] <- 1
        } else if (z.init[i, t - 1] == 3) {
          z.init[i, t] <- 3  # stay dead
        } else {
          z.init[i, t] <- 1  # assume alive
        }
      }
    }
  }
  
  mod.data <- list(ch = ch, z = z.data)
  mod.constants <- list(n.occasions = n.occasions,
                        n.years = 1,
                        Nind = N_tot,
                        first = first,
                        year = rep(1, N_tot), # All individuals belong to year 1
                        last = last.track)
  
  params <- c("chick_month_survival", "winter_dsr", "summer_dsr", "harvest_prob", "fall_cap_prob", "winter_cap_prob")
  
  # Model function
  NOBO_real_MCMC <- function(seed, mod.data, mod.constants, params, z.init, ni, nt, nb) {
    require(nimble)
    
    NOBO_sim <- nimbleCode({
      chick_month_survival ~ dunif(0, 1)
      winter_dsr ~ dunif(0, 1)
      summer_dsr ~ dunif(0, 1)
      harvest_prob ~ dunif(0, 1)
      fall_cap_prob ~ dunif(0, 1)
      winter_cap_prob ~ dunif(0, 1)
      
      # Survival, harvest, and capture probabilities
      s[1,1:(n.occasions-1)] <- c(chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival,
                                  winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31)
      
      r[1:(n.occasions-1)] <- c(1, 1, 1, 1, 1,
                                (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4))
      
      capture_probs[1:n.occasions] <- c(0, 0, 0, 0, 0, 0,fall_cap_prob, 0, winter_cap_prob, 0)
      
      for(h in 1:n.years){
        for(o in 1:(n.occasions-1)){
          ps[1,1,h,o] <- s[h,o]*r[o]
          ps[1,2,h,o] <- 1-s[h,o]
          ps[1,3,h,o] <- s[h,o]*(1-r[o])
          ps[2,1:3,h,o] <- c(0,1,0)
          ps[3,1:3,h,o] <- c(0,0,1)
        }
      }
      
      for(q in 1:n.occasions){
        po[1,1:3,q] <- c(capture_probs[q], 1-capture_probs[q], 0)
        po[2,1:3,q] <- c(0,1,0)
        po[3,1:3,q] <- c(0,0,1)
      }
      
      for(i in 1:Nind){
        for(t in (first[i]+1):last[i]){
          z[i,t] ~ dcat(ps[z[i, t-1], 1:3, year[i], t-1])
          ch[i,t] ~ dcat(po[z[i, t], 1:3, t])
        }
      }
    })
    
    init.function <- function() list(
      z = z.init,
      chick_month_survival = runif(1, 0.5, 1),
      winter_dsr = runif(1, 0.9, 1),
      summer_dsr = runif(1, 0.9, 1),
      harvest_prob = runif(1, 0, 0.2),
      fall_cap_prob = runif(1, 0.05, 0.95),
      winter_cap_prob = runif(1, 0.05, 0.95)
    )
    
    NOBO.chick.sim <- nimbleModel(code = NOBO_sim,
                                  data = mod.data,
                                  constants = mod.constants,
                                  inits = init.function())
    
    NOBO.chick.sim.mcmc.out <- nimbleMCMC(model = NOBO.chick.sim,
                                          niter = ni, nchains = 1, nburnin = nb,
                                          monitor = params, thin = nt,
                                          samplesAsCodaMCMC = TRUE, setSeed = seed)
    
    return(NOBO.chick.sim.mcmc.out)
  }
  
  # Run MCMC in parallel
  this_cluster <- makeCluster(nc)
  site_mcmc <- parLapply(cl = this_cluster, X = 1:nc,
                         fun = NOBO_real_MCMC,
                         mod.data = mod.data,
                         mod.constants = mod.constants,
                         params = params,
                         z.init = z.init,
                         ni = ni,
                         nb = nb,
                         nt = nt)
  stopCluster(this_cluster)
  
  site_results[[site_name]] <- list(
    site = site_name,
    posterior_samples = site_mcmc
  )
  }
}

# Optional: Save results
#saveRDS(site_results, file = "NOBO_real_data_results.rds")
