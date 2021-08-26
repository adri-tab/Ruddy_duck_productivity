library(nimble)
library(tidyverse)
require(mcmcplots)
set.seed(1)

modCode <- nimbleCode(
  {
    a ~ dnorm(0, sd = 1e4)
    er ~ dunif(0, 1e4)
    
    for (i in 1:10) {
      
      log(y[i]) ~ dnorm(a, sd = er)
      
    }
  }
)

modConsts <- list()

modData <- list(y = rnorm(10, 1e2, 10) %>% exp())

modInits <- list(a = 0, er = 1)

Rmodel <- nimbleModel(code = modCode, 
                      name = "Rmodel",
                      constants = modConsts,
                      data = modData, 
                      inits = modInits)


# node targets
to_mon <- c("a", "er")

## 3. build MCMC from compiled or not compiled model
# Parameters monitored (i.e., for which estimates are saved)
mcmc <- buildMCMC(Rmodel, monitors = to_mon) 

## 4. compile MCMC, requires the Rmodel to be compiled
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(mcmc, project = Rmodel)

# mcmc parameters
niter <- 1e5 ; nburnin <- 1e4 ; nsample <- 1e4 ; nchain <- 1

Csamples <- runMCMC(Cmcmc, niter = niter, nburnin = nburnin, 
                    thin = max(1, round((niter - nburnin) / nsample)), nchains = nchain)
Csamples <- (if (is_list(Csamples)) {Csamples} else {list(Csamples)}) %>% map(as_tibble)

# plot for convergence check
Csamples %>% walk(~ .x %>% mcmcplot())

## 7. convert samples to an mcmc object for the coda package
Csamples_coda <- Csamples %>% "[["(1) %>% as.mcmc()

## summarize output
Csamples_coda %>% summary()

