rm(list = ls()) # Clear the workspace
N <- 20151*2  #The total sample size across treat & control.
n <- N/2   #Sample size for each group (1/2 the total)

p_control <- .20 # underlying probability of success in control
p_treat <- rnorm(n,0.2,0.01) # underlying probability of success in treat
rate <- c()

# Now let's use bootstrapping to record the results of many simulations.  
for (i in 1:5000){  # loop through 1,999 more times to get 5k total simulations
  tempcontrol <- runif(n) #create n random uniform draws for control for this simulation
  success_control <- as.integer(tempcontrol < p_control) # creates binary indicator for success with p_control chance
  temptreat <- runif(n) #create n random uniform draws for treat
  success_treat <- as.integer(temptreat < p_treat[i]) # creates binary indicator for success with p_treat chance
  
  results_t <- t.test(success_treat, success_control, alternative = "greater") #Record the t.test of equality in the two for this round of simulations
  if (results_t$p.value < 0.50) {
    rate <- c(rate, p_treat[i])
 }else if (results_t$p.value >= 0.50)
    rate <- c(rate, 0.2)
}

hist(rate, bin = 5)
