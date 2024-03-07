library(ggplot2)
library(tidyr)
library(dplyr)
library(ape)
library(tensorA)
library(corpcor)
library(cubature)
library(MCMCglmm)

#number of females range from 82 - 115 at April 15 
# n = number of samples
#mean = 97 
#standard deviations= 11.76 

# nests/hen range from 0.6100 to 0.9100
#mu <- 0.7825
#std <- 0.0995

#nest success range from 0.4300 - 0.7500
# mu = 0.6013
# sd = 0.0955

#clutch size range from 1-26
# mu = 12.5237
# sd = 3.1871

########creating a function in R
#   Function_name <- function(parameter){
#           function body   
#   }
#####where:
# function_name is the name of the function object stored in the R environment and used for calling that function
# parameters/formal arguments = variables that we set/change
# function body = set of commands inside curly brackets run in predefined order every time we call the function
# i.e. what we need the function to do 

#  N * fn * ns * cs * ha * gs

  # N = represents the number of females alive at the start of the breeding season
  # fn = represents the probability that a female nests
  # ns = represents the probability that a nest is successful
  # cs = is the average clutch size
  # ha = the number of eggs that hatch out/total successful clutch count
  # gs = is the genotyping success rate for eggshells 

#################################################################################
num.iterations <- 10000    #number of times to run the code 

breeding_sample_size = 100
nest_success_prob = 0.5
hatch_prob = 0.85
genotype_success = 0.95
capture_prob = 0.15

total_recap = numeric()

# loop to run recapture loop 
for (iterations in 1:num.iterations) {
  fn = numeric()
  ns = numeric()
  cs = numeric()
  chicks = numeric()
  marked = numeric()
  recap = numeric()
  af = numeric()
  
  #loop to calculate recapture numbers
  for (i in 1:breeding_sample_size) {
    fn [i] = rpois(1,1.1)     #nests/female
    ns [i] = rbinom(1, fn[i], nest_success_prob)  #nest success
    cs [i] = rpois(1, 12)    #clutch size
    chicks [i] = rbinom(1, ns[i] * cs, hatch_prob)  #number of chicks hatched
    marked [i] = rbinom(1, chicks[i], genotype_success)   #number of eggs genotyped
    af [i] = rbinom(1, marked[i], 0.4)    #number of chicks alive in the fall
    recap [i] = rbinom(1, af[i], capture_prob)    #number recaptured in the fall
  }
  
  total_recap[iterations] = sum(recap)
}

print(total_recap)
mean(total_recap)

