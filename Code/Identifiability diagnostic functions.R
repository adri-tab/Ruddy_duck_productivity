hessian <- function(funfcn, x, y, delta){
  # function to calculate the hessian of funfcn at x
  t <- length(x)
  h <- matrix(0, t, t)
  Dx <- delta * diag(t)
  for (i in 1:t) {
    100
    for (j in 1:i) {
      h[i,j] <- (do.call("funfcn", list(x + Dx[i,] + Dx[j, ], y)) # LL with
                 - do.call("funfcn", list(x + Dx[i,] - Dx[j, ], y))
                 - do.call("funfcn", list(x - Dx[i,] + Dx[j, ], y))
                 + do.call("funfcn", list(x - Dx[i,] - Dx[j, ], y))) / (4 * delta^2)
      h[j, i] <- h[i, j]
    }
  }
  return(h)
}

hessianmethod <- function(funfcn, pars, y, delta, print = TRUE){
  # Applies the Hessian method
  # funfcn - function which return negative loglikelihood
  # pars - parameter values at which hessian is evaluated
  # y - data to be passed into likelihood
  # delta - error used in calculating Hessian and cutt-off
  # suggested value delta = 0.00001
  # cut-off used delta*p, where p is no. pars
  cutoff <- delta*length(pars)
  # Finds hessian matrix
  h <- do.call("hessian", list(funfcn, pars, y, delta))
  # Calculates eigenvalues
  E <- eigen(h)
  # find standardised eigenvalues
  standeigenvalues <- abs(E$values) / max(abs(E$values))
  # find number of estimable parameters
  # number of parameters with eigenvalues below the cutoff
  noestpars <- 0
  for (i in 1:length(pars)) {
    if (standeigenvalues[i] > cutoff) {
      noestpars <- noestpars + 1
    }
  }
  if (print) {
    # Prints whether model is parameter redundant or not
    101
    # Then prints smallest eigenvalue and
    # number of estimable parameters
    if (min(standeigenvalues) < cutoff) {
      cat("model is non-identifiable or parameter redundant")
    }
    else {
      cat("model is identifiable or not parameter redundant")
    }
    cat("\n")
    cat('smallest standardized eigenvalue', min(standeigenvalues))
    cat("\n")
    cat('number of estimable parameters', noestpars)
  }
  result <- list(standeigenvalues = standeigenvalues, noestpars = noestpars)
  return(result)
}


# Here beta[1] = theta[1], beta[2] = theta[2] and alpha = theta[3]
mfx <- function(theta) {
  dlm(m0 = 0, C0 = 0,
      GG = theta[1]*theta[2], W = 0.1^2,
      FF = theta[3],V = 0.1^2)
}
likfx <- function(theta,y) {
  RW.obj <- mfx(theta)
  dlmLL(y = y, mod = RW.obj)
}

