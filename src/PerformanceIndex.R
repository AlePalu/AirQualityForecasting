## compute WAIC index
## fit: MCMC sampled returned by some fit() method
WAIC <- function(fit){
  llik   = rstan::extract(fit, "log_lik")[[1]]
  p_WAIC = sum(apply(llik, 2, var))
  lppd   = sum(apply(llik, 2, function(x) log(mean(exp(x)))))
  WAIC   = - 2 * lppd + 2 * p_WAIC
  return(WAIC)
}


library(mvtnorm)
## implements BIC as 2log(f(x|theta)) - rlog(n), where
##    * theta is the posterior mean obtain from MCMC sample
##    * n is the number of data points
##    * f(x|theta) is the model likelihood
##    * r is the number of parameters
## it works for AR or ARX models, with gaussian distributed errors
BIC <- function(sample, data){
    ## compute posterior mean of parmaters
    params <- data.frame(rstan::extract(sample, perm = TRUE))
    theta <- params[, grepl("mu", colnames(params))]

    posteriorMean <- apply(theta, 2, mean)
    sigma2 <- mean(params[, "sigma2"])
    
    r <- dim(theta)[2] ## number of parameters
    n <- length(data)  ## number of points

    ## compute likelihood of data, assumed normally distributed (works for AR and ARX)
    llik = dmvnorm(data, posteriorMean, diag(rep(sigma2, length(data))), log = TRUE)

    return(2*llik - r*log(n))
}

## compute model bayesian predictive residuals
## forecast is the output of ARforecast
## data is a vector of test data
computePredictiveResiduals <- function(forecast, data) {
    ## at each step, compute mean and standard deviation from the forecast
    pred.mean = apply(forecast, 2, mean)
    pred.sd = apply(forecast, 2, sd)

    ## compute residual
    residuals = (pred.mean - data) / pred.sd

    invisible(residuals)
}

## compute the MSE as the sum of residual squared
## forecast: forecast matrix produced by some forecast() function
## data: a vector of test data
MSE <- function(forecast, data){
    residuals = computePredictiveResiduals(forecast, data)
    invisible(sum(residuals^2))
}
