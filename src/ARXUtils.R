source("DataProcessUtils.r")

## the following function returns a sample from an ARX(p) model
## ARmodel is a compiled stan model, obtained from stan_model(...)
## data is a vector of real values (the time series we want to predict)
## p is an integer indicating the order of autoregression
## reg_part is a hashmap associating to each key a list maden by two elements:
##          + first element is a vector of real values (the time series
##            we use as external regressor)
##          + second element is the order of autoregression relative to this
##            covariate
ARXfit <- function(ARXmodel, data, p, reg_part){
    ## create dataset for ARX(p)
    d <- data.frame(intercept = rep(1, length(data)))
    for (t in 1:p) {
        columnName <- sprintf("Y[t-%d]", t)
        d[, columnName] <- shift(data, t)
    }
    pMax = p
    
    ## add the reg_part part
    for (var in ls(reg_part)) {
        if(reg_part[[var]][[2]] > pMax){
            pMax = reg_part[[var]][[2]]
        }
        for (t in 1:reg_part[[var]][[2]]) {
            columnName <- sprintf("%s[t-%d]", var, t)
            d[, columnName] <- shift(reg_part[[var]][[1]], t)
        }
    }
    ## last p rows have NaN due to shifting, not consider them
    numRows = nrow(d)
    d <- d[0:(numRows - pMax), ]

    ## number of parameters
    k <- p + 1
    for (var in ls(reg_part)){
        k <- k + reg_part[[var]][[2]]
    }

    stanData <- list(
        N = nrow(d),
        K = k,
        Y = data[1:nrow(d)],
        X = d,
        ## prior parameters
        scale_s2 = 10
    )

    ## init regression coefficient parameter to their MLE
    X <- as.matrix(d)[, 2:ncol(d)]
    linReg <- lm(data[1:nrow(d)] ~ X)

    stanData$mean_beta <- summary(linReg)$coefficients[, 1]
    stanData$var_beta <- (summary(linReg)$coefficients[, 2])^2

    ## data are now ready to be supplied to Stan
    ARXsample <- sampling(ARXmodel,
                          data = stanData, chains = 1,
                          iter = 10000, warmup = 2000,
                          show_messages = FALSE
                          )

    invisible(ARXsample) ## return output of stan
}

## extract parameters from MCMC sample returned by stan as an hashmap
## sample: the MCMC sample from which extract
## regressors: same hashmap passed to ARXfit as reg_part
## p: the order of autoregression
ARXextract <- function(sample, regressors, p) {
    ## extract parameters from MCMC sample 
    params = data.frame(rstan::extract(sample, perm = T))
    params['lp__'] = NULL

    result           <- new.env(hash = TRUE)
    result$intercept <- params["beta.1"]
    result$sigma2    <- params["sigma2"]
    
    beta <- params[, grepl("beta", colnames(params))]
    beta["beta.1"] <- NULL ## remove intercept

    result$ARpart <- beta[,1:p]

    print(head(beta))
    
    ## temporary variable used to keep track of the already pulled parameters
    c = p
    for (var in 1:length(regressors)) { ## add parameters for regressive part
        name <- sprintf("%s", ls(regressors)[var])
        result[[name]] <- beta[, (c + 1):(c + regressors[[ls(regressors)[var]]][[2]]), drop = FALSE]
        c = c + regressors[[ls(regressors)[var]]][[2]]
    }
    
    invisible(result)
}

## compute the forecast distribution of an ARX(p)
## data: an hashmap containing both autoregressive and regressive values (see example)
## fit: the hashmap as returned by fitARX
## horizon: number of time steps we need to simulate, starting from the last point in data
ARXforecast <- function(data, reg_part, sample, horizon){
    order = dim(sample$ARpart)[2]
    params = sample$ARpart
    
    for (i in 0:(order-1)) {
        colName = sprintf("Y[t-%s]", i)
        params[colName] = data[length(data) - i]
    }
    
    ## build a structure: "parameter | lagged values" for each component in fit
    for (v in ls(sample)) {
        if (!v %in% c("intercept", "sigma2", "ARpart")) {
            ## add parameter block
            params = cbind(params, sample[[v]])

            for (i in 0:(reg_part[[v]][[2]]-1)) {
                colName = sprintf("%s[t-%s]", v, i)
                params[colName] = reg_part[[v]][[1]][length(reg_part[[v]][[1]]) - i]
            }
        }
    }
    params['intercept'] = sample$intercept
    params['sigma2']    = sample$sigma2
    
    forecast = matrix(NA, nrow = nrow(params), ncol = horizon)

    ## used for the for loop below
    tot <- 2*order
    for(var in ls(reg_part)){
        tot <- tot + (2 * reg_part[[var]][[2]])
    }
    
    for(i in 1:horizon){
        forecast[, i] = apply(params, 1,
                              function(x){
                                  ## simulate the ARX process
                                  mean = x[tot + 1]

                                  for (i in 1:order){ ## autoregressive part
                                      mean = mean + (x[i] * x[i + order])
                                  } 
                                  k = 2 * order
                                  for(var in ls(reg_part)){
                                      varOrder = reg_part[[var]][[2]]
                                      for (i in 1:varOrder){
                                          mean = mean + (x[k + i] * x[k + i + varOrder])
                                      }
                                      k = k + (2 * varOrder)
                                  }
                                  
                                  return(rnorm(1, mean, sqrt(x[tot + 2])))
                              })
        ## shift columns
        if(order > 1){
            for(j in order:2){
                col1 = sprintf("Y[t-%s]", j)
                col2 = sprintf("Y[t-%s]", j-1)
                params[col1] = params[col2]
            }
        }
        params["Y[t-1]"] = forecast[, i]
    }

    ## each column of forecast is a sample from the predictive distribution
    ## of y[t+h] given the past
    invisible(forecast) # return the forecast as an M x horizon matrix
}

## Ytrain: vector of real values
## Xtrain: regressors used during training, an hashmap
## block: boolean indicating if data must be moved in blocks of length horizon
ARXLiveForecast <- function(Ytrain, Xtrain, Ytest, Xtest, sample, horizon){
    l_test = length(Ytest)
    forecast = matrix(NA, nrow = nrow(sample$ARpart), ncol = l_test)

    order = dim(sample$ARpart)[2]

    data = new.env(hash = TRUE)
    maxOrder = order
    for(var in ls(Xtest)){
        varOrder = Xtest[[var]][[2]]
        if(varOrder > maxOrder){
            maxOrder = varOrder
        }
    }

    for(var in ls(Xtest)){
        Xlength  = length(Xtrain[[var]][[1]])
        varOrder = Xtrain[[var]][[2]]
        data[[var]] = list(c(Xtrain[[var]][[1]][(Xlength - maxOrder - horizon):Xlength], Xtest[[var]][[1]]),
                           varOrder)
    }

    response = c(Ytrain[(length(Ytrain) - maxOrder - horizon):length(Ytrain)], Ytest)

    for(t in seq(maxOrder + horizon + 1, length(response))){
        ## supply to ARXforecast data as they would arrive in real world
        ## hour by hour
        liveData = new.env(hash = TRUE)
        for (var in ls(data)) {
            varOrder = data[[var]][[2]]
            liveData[[var]] = list(data[[var]][[1]][(t-varOrder-horizon+1):(t-horizon)], data[[var]][[2]])
        }
        liveResponse = response[(t-order-horizon+1):(t-horizon)]
        partialForecast = ARXforecast(liveResponse, liveData, sample, horizon)
        
        ## save the forecast
        if ((t - maxOrder - horizon - 1) <= l_test) {
            forecast[, t - maxOrder - horizon - 1] = partialForecast[, horizon]
        }
    }
    
    invisible(forecast)
}

ARXforecastRange <- function(Ytrain, Xtrain, Ytest, Xtest, sample, range){
    if (length(range) != 2) {
          return()
    }

    movie = new.env(hash = TRUE)
    for (i in seq(range[1], range[2])) {
        movie[[sprintf("%d", i)]] = ARXLiveForecast(Ytrain, Xtrain, Ytest, Xtest, sample, i)
    }

    invisible(movie)
}
