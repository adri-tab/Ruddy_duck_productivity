Sys.setlocale("LC_ALL", "en_US.UTF-8")

require(tidyverse)
require(nimble)
require(MCMCvis)
require(mcmcplots)

set.seed(0)

sd_sim = .3

tibble(
  ts = c(rep(1, 11),
         rep(2, 5))) %>% 
  rowid_to_column("x") %>% 
  mutate(
    y = c(
    (13:23 / 3) %>% map_dbl(~ rnorm(1, .x, sd_sim)),
    (10:14 / 3) %>% map_dbl(~ rnorm(1, .x, sd_sim))
  ), 
  N = exp(y)) -> q

JuvCode <- nimbleCode(
  {
   # hierarchical prior on lambda max
    r_max_avg ~ dunif(log(0.01), log(5))
    r_max_sd ~ dunif(0, 1e2)
    
    prior_r_max ~ dnorm(r_max_avg, sd = r_max_sd)
    log(prior_lambda_max) <- prior_r_max
    
    for (r in 1:2) {
      # 
      r_avg[r] ~ dnorm(r_max_avg, sd = r_max_sd)
      # r_avg[r] ~ dunif(log(0.01), log(5))
      N_int[r] ~ dnorm(0, sd = 1e2)
      N_sd[r] ~ dunif(0, 1e3)
      
      log(lambda[r]) <- r_avg[r]
    }
    
    # LL
    for (t in 1:R) {
      
     N[t] ~ dnorm(N_int[ts_id[t]] + r_avg[ts_id[t]] * year[t], 
                                sd = N_sd[ts_id[t]])
    }
    
  })

# model data
JuvConst <- list(R = nrow(q),
                 ts_id = q$ts)

JuvData <- list(N = q$y,
                year = q$x)

JuvInit <- list(r_max_avg = log(1), 
                r_max_sd = 1, 
                prior_r_max = log(1.5),
                N_int = rep(0, 2),
                r_avg = rep(log(1), 2),
                N_sd = rep(1, 2))

# node targets
JuvMon <- c("r_max_avg", 
            "r_max_sd", 
            "prior_r_max",
            "r_avg",
            "prior_lambda_max",
            "lambda")

# mcmc parameters
nsample_1 <- 5e4 ; thin_1 <- 10 ; nburnin_1 <- 2e3 ; nchain_1 <- 1
nsample_1 * thin_1 + nburnin_1

myseed <- 1

nimbleMCMC(
  code = JuvCode, 
  constants = JuvConst, 
  data = JuvData, 
  inits = JuvInit, 
  monitors = JuvMon,
  niter = nsample_1 * thin_1 + nburnin_1, 
  nburnin = nburnin_1, 
  thin = thin_1, 
  nchains = nchain_1, 
  setSeed = myseed) -> JuvOut

MCMCsummary(object = JuvOut, 
            Rhat = FALSE, 
            n.eff = FALSE) -> JuvOut2
