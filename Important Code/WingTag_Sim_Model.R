library(jagsUI)
library(extraDistr)
library(ggplot2)
library(tidyr)
library(dplyr)

################################################################################
#          Simulation model for estimating chick survival using WINGTAG
################################################################################
# Code for simulating and analyzing northern bobwhite chick survival data. Simulating
#   a 3-year study. Every breeding season a certain number of chicks are genotyped
#   in each month, can be recaptured or harvested later based on survival and detection
#   parameters. Also banding a certain number of other birds each fall and spring to
#   estimate survival and seasonal capture probability.

# Assuming that vital rates don't vary over time or demographics right now. Could change later.

# Summer goes from April - September, winter from October - March
require(parallel)
library(extraDistr)


# PARAMETERS ---------------------------------- 
num.iterations <- 100 # Number of iterations to run the simulation
n.years <- 3
n.occasions <- 12*n.years - 1 # Number of occasions (months) in the simulation. Starting in May. Last possible fate date is harvest from February - March, so cutting out last April (can't be observed)
n.states <- 3 # Number of states in the multistate model

# Fixed Biological / Sampling Parameters ----------------------------------------
hatch_prob         <- 0.85
geno_egg           <- 0.98
geno_feather       <- 0.99

winter_dsr <- 0.9966302
summer_dsr <- 0.9951 # Rough values of adult breeding dsr from Albany

# Cohort release probabilities, will vary by year. Juvenile can be released from May - October
probabilities_1 <- c(0.05, 0.35, 0.30, 0.19, 0.1, 0.01, rep(0, times=29))
probabilities_2 <- c(rep(0, times=12), 0.05, 0.35, 0.30, 0.19, 0.1, 0.01, rep(0, times=17))
probabilities_3 <- c(rep(0, times=24), 0.05, 0.35, 0.30, 0.19, 0.1, 0.01, rep(0, times=5))
probabilities <- rbind(probabilities_1, probabilities_2, probabilities_3)

# Releasing extra birds via trapping each November and January Helps with estimating adult survival and capture probabilites
Nrelease_fall <- 100
Nrelease_winter <- 100
release_periods <- matrix(c(7, 9, 19, 21, 31, 33), nrow=3, ncol=2, byrow=T)

# MCMC settings
ni <- 25000
nt <- 1
nb <- 5000
nc <- 3


# Simulating Data --------------------------------------------------------------

# Storage for scenario results
wing_scenario_results <- vector("list", nrow(SimScenarios_ChickSuv))

# Looping through scenarios
for(S in 1:nrow(SimScenarios_ChickSuv)) {
  scenario <- SimScenarios_ChickSuv[S,]
  
  #Extract scenario-specific parameters
  n.breed <- scenario$Adult_sample_size  # Number of breeding individuals radio-marked each year
  n_females <- round(0.75 * n.breed)
  n_males <- n.breed - n_females
  
  nest_succ_prob <- as.numeric(scenario$Nest_success)
  chick_survival_to_fall <- as.numeric(scenario$Chick_survival)
  chick_month_survival <- chick_survival_to_fall^(1/5)
  harvest_prob <- as.numeric(scenario$Harvest_rate)
  
  recap_probs <- as.numeric(strsplit(as.character(scenario$Recap_probs), ",")[[1]])
  fall_cap_prob <- recap_probs[1]
  winter_cap_prob <- recap_probs[2]
  
  # Nest propensity values
  nest_prop_values <- switch(as.character(scenario$Nest_propensity),
                             "low" = list(F = c(0.846, 0.090, 0.064, 0.000, 0.000),M = c(0.898, 0.102, 0, 0, 0)),
                             "low-med" = list(F = c(0.625, 0.216, 0.132, 0.024, 0.003), M = c(0.753, 0.233, 0.014, 0, 0)),
                             "medium" = list(F = c(0.433, 0.328, 0.187, 0.047, 0.005), M = c(0.586, 0.383, 0.031, 0, 0)),
                             "medium-high" = list(F = c(0.254, 0.430, 0.243, 0.066, 0.007), M = c(0.390, 0.559, 0.051, 0, 0)),
                             "high" = list(F = c(0.095, 0.516, 0.295, 0.084, 0.010), M = c(0.154, 0.771, 0.075, 0, 0)),
                             
                             "mean" = list(F = c(0.4794, 0.3525, 0.1499, 0.0175, 0.0007), M = c(0.7218, 0.2632, 0.0150, 0, 0))
  )
  
  init_avg_f <- setNames(nest_prop_values$F, paste0("rate_", 0:4))
  init_avg_m <- setNames(nest_prop_values$M, paste0("rate", 0:4))
  
  # Monthly survival. Will differ for birds released in each year
  # Setting up survival as from t-1 to t
  
  # Released in first year 
  s_1 <- c(rep(chick_month_survival, 5),
           winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28, winter_dsr^31,
           summer_dsr^30, summer_dsr^31, summer_dsr^30, summer_dsr^31, summer_dsr^31, summer_dsr^30,
           winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28, winter_dsr^31,
           summer_dsr^30, summer_dsr^31, summer_dsr^30, summer_dsr^31, summer_dsr^31, summer_dsr^30,
           winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28)
  # Released in second year. Survival before the second May is uninformative
  s_2 <- c(rep(NA, times=12),
           rep(chick_month_survival, 5),
           winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28, winter_dsr^31,
           summer_dsr^30, summer_dsr^31, summer_dsr^30, summer_dsr^31, summer_dsr^31, summer_dsr^30,
           winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28)
  # Released in third year. Survival before the third May is uninformative
  s_3 <- c(rep(NA, times=24),
           rep(chick_month_survival, 5),
           winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28)
  
  s <- rbind(s_1, s_2, s_3)
  
  # Capture probabilities, assuming will be the same across years
  capture_probs <- rep(c(0, 0, 0, 0, 0, 0,fall_cap_prob, 0, winter_cap_prob, 0, 0, 0), times=3)[1:n.occasions] # Monthly capture probabilities, Only can capture in November and January
  
  
  # Harvest probabilities, harvest can occur in Nov - Feb. Going to assume that capture probabilities
  #   differ between spring and fall but consistent across years (otherwise, not identifiable)
  # r value is probability of not being harvested from t-1 to t
  harvest_prob <- 0.0668 # Probability of harvest over 4-month period
  r <- c(rep(1, 6),
         rep((1 - harvest_prob)^(1/4), 4),
         rep(1, 8),
         rep((1 - harvest_prob)^(1/4), 4),
         rep(1, 8),
         rep((1 - harvest_prob)^(1/4), 4))
  
  
  # Initialize stroage for simulation
  CH_multistate_list <- vector("list", num.iterations)
  summary_stats <- array(NA, dim=c(num.iterations, 4, n.years))
  dimnames(summary_stats) <- list(1:num.iterations,c("marked", "fall_recaptures", "winter_recaptures", "harvests"),paste0("Year",1:n.years))
  posterior_summary_list <- vector("list", num.iterations)
  
  #iteration loop
  for(v in 1:num.iterations){
    tot_brood_cap_chicks<- rep(NA, times=n.years) # Number of chicks captured in each year
    n_marked_total <- rep(NA, times=n.years) # Number of total birds marked in each year
    release_occasions <- release_year <- c() # release_occasions gives first occasion for each chick, release_year gives year of first capture for each chick
  
    # Simulating the number of chicks captured in each year
    for(y in 1:n.years){
      
      marked_chicks <- integer(0)
      chicks_captured <- integer(0)
      
      #Females
      for(k in seq_len(n_females)){
        
        ind_nest <- rcat(1, init_avg_f) - 1 # How many nests each female has
        nest_succ <- rbinom(1, ind_nest, nest_succ_prob) # Number of successful nests
        clutch <- rpois(1, 12) # Average clutch size, could do truncated normal to bound this
        chicks <- rpois(1, nest_succ * clutch * hatch_prob) # Total number of chicks produced
        
        #chick capture rates
        chicks_captured <- rpois(1, chicks*exp(-1.12435))# Number of chicks captured, based on a model from the data
        if (chicks_captured > 0) {
          marked_chicks <- c(marked_chicks, rep(k, chicks_captured))  # marked_chicks is a vector giving the adult ID for every chick genotyped
        }
      }
      
      #Males
      for(k in seq_len(n_males)){
        
        ind_nest <- rcat(1, init_avg_m) - 1 # How many nests each male has
        nest_succ <- rbinom(1, ind_nest, nest_succ_prob) # Number of successful nests
        clutch <- rpois(1, 12) # Average clutch size, could do truncated normal to bound this
        chicks <- rpois(1, nest_succ * clutch * hatch_prob) # Total number of chicks produced
        
        #chick capture rates
        chicks_captured <- rpois(1, chicks*exp(-1.12435))# Number of chicks captured, based on a model from the data
        if (chicks_captured > 0) {
          marked_chicks <- c(marked_chicks, rep(k, chicks_captured))  # marked_chicks is a vector giving the adult ID for every chick genotyped
        }
      }
      
      tot_brood_cap_chicks[y] <- length(marked_chicks)
      
      # Assign cohorts to main group
      cohort_release <- t(rmultinom(1, size = tot_brood_cap_chicks[y], prob = probabilities[y,])) # Assigning period each individual released in
      released_chick <- rep(1:n.occasions, cohort_release[1, ])
      
      # Adding in extra captured adults. If last year, don't need to bother releasing birds in the last period
      if(y < n.years){
        released_chick <- c(released_chick,
                            rep(release_periods[y,1], Nrelease_fall),
                            rep(release_periods[y,2], Nrelease_winter))
      } else{
        released_chick <- c(released_chick,
                            rep(release_periods[y,1], Nrelease_fall))
      }
      
      n_marked_total[y] <- length(released_chick)
      
      release_occasions <- c(release_occasions,
                             released_chick)
      release_year <- c(release_year,
                        rep(y, n_marked_total[y]))
    }
    N_tot <- sum(n_marked_total)
  
  
  summary_stats[v,1,] <- tot_brood_cap_chicks
  
  
  # Have 3 state variables:
  #   1: Alive
  #   2: Dead from natural causes
  #   3: Dead from harvest
  # Have 3 observation variables:
  #   1: Observed alive
  #   2: Not observed
  #   3: Observed dead (harvested)
  
  
    # State transition matrix goes from t to t+1. Have to index by year marked
    ps <- array(NA, dim=c(3,3,n.occasions-1,n.years))
    for(y in 1:n.years){
      for(t in 1:(n.occasions-1)){
        ps[1,1,t,y] <- s[y,t]*r[t] # Probability of staying alive is probability of surviving natural causes AND surviving harvest
        ps[1,2,t,y] <- 1-s[y,t] # Probability of transitioning from alive to dead natural causes is probability of not surviving from natural causes
        ps[1,3,t,y] <- s[y,t]*(1-r[t]) # Probability of transitioning from alive to harvested is probability of surviving natural causes AND succumbing to harvest
        ps[2,1:3,t,y] <- c(0,1,0) # Naturally dead birds stay this way
        ps[3,1:3,t,y] <- c(0,0,1) # Harvested birds stay this way
      } 
    }
    
    
    # Observation matrix, setting first to 0 since won't use. Easier to interpret this way
    po <- array(NA, dim=c(3,3,n.occasions)) 
    for(t in 1:n.occasions){
      po[1,1:3,t] <- c(capture_probs[t], 1-capture_probs[t], 0) # Alive birds can be detected or not observed based on capture probability
      po[2,1:3,t] <- c(0,1,0) # Dead birds can't be detected
      po[3,1:3,t] <- c(0,0,1) # Harvested birds can only be detected harvested
    }
    
    
    # Generating capture histories
    z <- ch <- z.init <- z.data <- matrix(NA, nrow=N_tot, ncol=n.occasions) # z is true state, ch is capture history, z.data is values of z for period known alive based on ch, z.init is initial values for z = NA
    last.track <- rep(n.occasions, N_tot) # last tracked date. Setting to the number of occasions, but will be overwritten if harvested (known dead)
    
    for(i in 1:N_tot){
      
      z[i, release_occasions[i]] <- ch[i, release_occasions[i]] <- 1
      
      for(t in (release_occasions[i]+1):n.occasions){
        
        # State process
        z[i, t] <- rcat(1, ps[z[i, t-1], 1:3, t-1, release_year[i]])
        
        # Observation process
        ch[i, t] <- rcat(1, po[z[i, t], 1:3, t])
        
        # Breaking if harvested
        if(ch[i, t] == 3){
          last.track[i] <- t
          break
        }
      }
      
      # Setting z-state data. If harvested, known if was alive until previous period
      whichz <- which(ch[i,]==1 | ch[i,]==3)
      z.data[i, min(whichz):max(whichz)] <- 1
      if(ch[i, max(whichz)] == 3){
        z.data[i, max(whichz)] <- 3
      }
      
      # Setting z-state initial values
      values.fill <- which(is.na(z.data[i,]))
      values.fill <- values.fill[values.fill > release_occasions[i] & values.fill <= last.track[i]]
      for(b in values.fill){
        if(b == values.fill[1]){
          z.init[i,b] <- 1
        } else{
          z.init[i,b] <- ifelse(z.init[i,b-1] == 2, 2,
                                ifelse(rbern(1, 0.7)==1, 1, 2))
        }
      }
    }
    #made changes in this loop to account for 0s in the capture history
    for(b in 1:n.years){
      ch.temp <- ch[release_year == b,]
      ch.temp <- ch.temp[1:tot_brood_cap_chicks[b],]
      
      # Fall recaptures
      tab.cap.f <- table(ch.temp[, capture_probs == fall_cap_prob])
      summary_stats[v, 2, b] <- if ("1" %in% names(tab.cap.f)) {
        unname(tab.cap.f["1"])
      } else {
        0
      }
      
      # Winter recaptures
      tab.cap.w <- table(ch.temp[, capture_probs == winter_cap_prob])
      summary_stats[v, 3, b] <- if ("1" %in% names(tab.cap.w)) {
        unname(tab.cap.w["1"])
      } else {
        0
      }
      
      # Harvests
      tab.harv <- table(ch.temp)
      summary_stats[v, 4, b] <- if ("3" %in% names(tab.harv)) {
        unname(tab.harv["3"])
      } else {
        0
      }
    }
  
    # Statistical Model -----------------------------------------------------------
    
    # Running in parallel
    
    mod.data <- list(ch = ch,
                     z = z.data)
    
    mod.constants <- list(n.occasions = n.occasions,
                          n.years = n.years,
                          Nind = N_tot,
                          first = release_occasions,
                          year = release_year,
                          last = last.track)
    
    params <- c("chick_month_survival", "winter_dsr", "summer_dsr", "harvest_prob", "fall_cap_prob", "winter_cap_prob")
    
    NOBO_sim_MCMC <- function(seed, mod.data, mod.constants, params, z.init, ni, nt, nb){
      
      require(nimble)
      
      ## Model code
      
      NOBO_sim <- nimbleCode({
        
        # Priors
        chick_month_survival ~ dunif(0, 1)
        winter_dsr ~ dunif(0, 1)
        summer_dsr ~ dunif(0, 1)
        harvest_prob ~ dunif(0, 1)
        fall_cap_prob ~ dunif(0, 1)
        winter_cap_prob ~ dunif(0, 1)
        
        # Survival, harvest, and capture probabilities
        s[1,1:(n.occasions-1)] <- c(chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival,
                                    winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28, winter_dsr^31,
                                    summer_dsr^30, summer_dsr^31, summer_dsr^30, summer_dsr^31, summer_dsr^31, summer_dsr^30,
                                    winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28, winter_dsr^31,
                                    summer_dsr^30, summer_dsr^31, summer_dsr^30, summer_dsr^31, summer_dsr^31, summer_dsr^30,
                                    winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28)
        s[2,1:(n.occasions-1)] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival,
                                    winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28, winter_dsr^31,
                                    summer_dsr^30, summer_dsr^31, summer_dsr^30, summer_dsr^31, summer_dsr^31, summer_dsr^30,
                                    winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28)
        s[3,1:(n.occasions-1)] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival, chick_month_survival,
                                    winter_dsr^31, winter_dsr^30, winter_dsr^31, winter_dsr^31, winter_dsr^28)
        r[1:(n.occasions-1)] <- c(1, 1, 1, 1, 1, 1,
                                  (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4),
                                  1, 1, 1, 1, 1, 1, 1, 1,
                                  (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4),
                                  1, 1, 1, 1, 1, 1, 1, 1,
                                  (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4), (1 - harvest_prob)^(1/4))
        capture_probs[1:n.occasions] <- c(0, 0, 0, 0, 0, 0,fall_cap_prob, 0, winter_cap_prob, 0, 0, 0, 0, 0, 0, 0, 0, 0,fall_cap_prob, 0, winter_cap_prob, 0, 0, 0, 0, 0, 0, 0, 0, 0,fall_cap_prob, 0, winter_cap_prob, 0, 0)
        
        # State Transition Matrix
        for(h in 1:n.years){
          for(o in 1:(n.occasions-1)){
            ps[1,1,h,o] <- s[h,o]*r[o]
            ps[1,2,h,o] <- 1-s[h,o]
            ps[1,3,h,o] <- s[h,o]*(1-r[o])
            ps[2,1:3,h,o] <- c(0,1,0)
            ps[3,1:3,h,o] <- c(0,0,1)
          } 
        }
        
        # Observation Matrix
        for(q in 1:n.occasions){
          po[1,1:3,q] <- c(capture_probs[q], 1-capture_probs[q], 0)
          po[2,1:3,q] <- c(0,1,0)
          po[3,1:3,q] <- c(0,0,1)
        }
        
        # Likelihood
        for(i in 1:Nind){
          # State at first capture will be 1. Input as data
          for(t in (first[i]+1):last[i]){
            # State process
            z[i,t] ~ dcat(ps[z[i, t-1], 1:3, year[i], t-1])
            # Observation process
            ch[i,t] ~ dcat(po[z[i, t], 1:3, t])
          }
        }
      })
      
      init.function <- function() list(z = z.init,
                                       chick_month_survival = runif(1,0.5,1),
                                       winter_dsr = runif(1, 0.9, 1),
                                       summer_dsr = runif(1, 0.9, 1),
                                       harvest_prob = runif(1, 0, 0.2),
                                       fall_cap_prob = runif(1, 0.05, 0.95),
                                       winter_cap_prob = runif(1, 0.05, 0.95))
      
      NOBO.chick.sim <- nimbleModel(code = NOBO_sim,
                                    data = mod.data,
                                    constants = mod.constants,
                                    inits = init.function())
      
      NOBO.chick.sim.mcmc.out  <- nimbleMCMC(model = NOBO.chick.sim, 
                                             niter = ni, nchains = 1, nburnin = nb,
                                             monitor=params, thin=nt, samplesAsCodaMCMC=TRUE, setSeed = seed)
      
      return(NOBO.chick.sim.mcmc.out)
    }
    
    
    this_cluster <- makeCluster(3) # Creating cluster
    
    NOBO_chick_sim <- parLapply(cl = this_cluster, X = 1:nc, 
                                fun = NOBO_sim_MCMC, 
                                mod.data = mod.data,
                                mod.constants = mod.constants,
                                params = params,
                                z.init = z.init,
                                ni = ni,
                                nb = nb,
                                nt = nt)
    
    stopCluster(this_cluster)
    
    posterior_summary_list[[v]] <- NOBO_chick_sim
  }# END iteration loop
  
  # SAVE results of this scenario: 
  wing_scenario_results[[S]] <- list(
    scenario = scenario,
    summary_stats = summary_stats,
    posterior_samples = posterior_summary_list
  )
  
}# END  scenario loop

saveRDS(wing_scenario_results, file = "NOBO_wing_scenario_results.rds")
