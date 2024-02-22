library(ggplot2)
library(tidyr)
library(dplyr)
library(ape)
library(tensorA)
library(corpcor)
library(cubature)
library(MCMCglmm)

########creating a function in R
#   Function_name <- function(parameter){
#           function body   
#   }
#####where:
# function_name is the name of the function object stored in the R environment and used for calling that function
# parameters/formal arguments = variables that we set/change
# function body = set of commands inside curly brackets run in predefined order every time we call the function
# i.e. what we need the function to do 


#function to calculate the number of chicks initially marked from eggshell genotyping
marked <- function(N, fn, ns, cs, ha, gs){
  N * fn * ns * cs * hr * gs
  breeding_sample_size = rpois(50, 97)
  nest_success_prob = 0.5
  hatch_prob = 0.85
  
  
  
  fn = rpois(breeding_sample_size, 1.1)
  ns = rbinom(fn, 1, nest_success_prob) 
  cs = rpois(fn, 12) 
  ha = rbinom(ns, cs, hatch_prob)
  gs = rbinom(ns, ha, 0.95)
  
  print(ha)
  
  #######
  # N = represents the number of females alive at the start of the breeding season
  # fn = represents the probability that a female nests
  # ns = represents the probability that a nest is successful
  # cs = is the average clutch size
  # ha = the number of eggs that hatch out/total successful clutch count
  # gs = is the genotyping success rate for eggshells 
}

# function to calculate the number of marked individuals recaptured in the fall 
recap <- function(M, phi, p){
  set.seed(123)
  y = rbinom(M, 1, p * phi)
  print(y)
  #M * phi * p
  
  ############
  # M = number of individual chicks marked at time of hatch i.e. successful eggshell genotype
  # phi = probability a hatched chick survives to the fall
  # p = probability a chicks is captured in the fall 
}

recap(100,0.5,0.20)



#number of females range from 82 - 115 at April 15 
# n = number of samples
lambda = 97 #average 
std = 11.76 #standard deviation

rtnorm(n = 50, mean = mu, sd = std, lower = 82, upper = 115) #normal distribution with upper and lower limits
rpois(n=50, lambda = lambda) #50 random numbers from poisson distribution with mean at 97

# nests/hen range from 0.6100 to 0.9100
#mu <- 0.7825
#std <- 0.0995

#nest success range from 0.4300 - 0.7500
# mu = 0.6013
# sd = 0.0955

#clutch size range from 1-26
# mu = 12.5237
# sd = 3.1871





