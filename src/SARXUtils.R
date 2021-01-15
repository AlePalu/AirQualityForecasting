## the following function returns a sample from an ARXSR model (ARX model with mixed effect)
## ARXSRmodel is a compiled stan model, obtained from stan_model(...)
## data is a vector of real values (the time series we want to predict)
## p is an integer indicating the order of autoregression
## reg_part is a hashmap associating to each key a list maden by two elements:
##          + first element is a vector of real values (the time series
##            we use as external regressor)
##          + second element is the order of autoregression relative to this
##            covariate
## W is the regime effect design matrix
SARXfit <- function(SARXmodel, data, p, reg_part, W, inits){
    ## create dataset for SARX
    d <- data.frame(intercept = rep(1, length(data)))
    if (p > 0){
        for (t in 1:p) {
            columnName <- sprintf("t-%d", t)
            d[, columnName] <- shift(data, t)
        }
    }
    pMax = p
    
    ## add the reg_part part
    for (var in ls(reg_part)) {
        if(reg_part[[var]][[2]] > pMax){
            pMax = reg_part[[var]][[2]]
        }
        for (t in 1:reg_part[[var]][[2]]) {
            columnName <- sprintf("%s-%d", var, t)
            d[, columnName] <- shift(reg_part[[var]][[1]], t)
        }
    }
    if (p == 0) d$intercept = NULL
    ## last pMax rows have NaN due to shifting, not consider them
    numRows = nrow(d)
    d <- d[0:(numRows - pMax), ]
    
    ## number of parameters
    k <- if(p == 0) 0 else (p + 1)
    for (var in ls(reg_part)){
        k <- k + reg_part[[var]][[2]]
    }

    ## number of groups
    W <- W[0:(numRows - pMax), , drop = FALSE]
    nGroups <- dim(W)[2]
    
    stanData <- list(
        N = nrow(d),
        K = k,
        nGr = nGroups,
        Y = data[1:nrow(d)],
        X = d,
        G = W,
        ## prior parameters
        scale_s2 = 10
    )

    ## init regression coefficient parameter to their MLE
    X <- as.matrix(d)[, 2:ncol(d)]
    linReg <- lm(data[1:nrow(d)] ~ X)
    
    stanData$mean_theta <- summary(linReg)$coefficients[, 1]
    stanData$var_theta  <- (summary(linReg)$coefficients[, 2])^2

    inits <- function() {
        list(theta         = as.array(rep(0.1, k)), 
             gamma         = as.array(rep(0.1, nGroups)),
             sigma2_regime = as.array(rep(10, nGroups)))
    }
    
    ## data are now ready to be supplied to Stan
    SARXsample <- sampling(SARXmodel, init = inits,
                          data = stanData, chains = 1,
                          iter = 8000, warmup = 1000,
                          show_messages = FALSE
                          )
    
    invisible(SARXsample) ## return output of stan
}

SARXextract <- function(sample, regressors, p){
    ## extract parameters from MCMC sample 
    params = data.frame(rstan::extract(sample, perm = T))
    params['lp__'] = NULL

    result           <- new.env(hash = TRUE)
    result$intercept <- params["theta.1"]
    result$sigma2    <- params["sigma2"]
    
    theta <- params[, grepl("theta", colnames(params))]
    theta["theta.1"] <- NULL ## remove intercept

    result$ARpart <- theta[,1:p]

    ## temporary variable used to keep track of the already pulled parameters
    c = p
    for (var in 1:length(regressors)) { ## add parameters for regressive part
        name <- sprintf("%s", ls(regressors)[var])
        result[[name]] <- theta[, (c + 1):(c + regressors[[ls(regressors)[var]]][[2]]), drop = FALSE]
        c = c + regressors[[ls(regressors)[var]]][[2]]
    }
    
    result$gamma <- params[, grepl("gamma", colnames(params))]
    result$sigma2_regime <- params[, grepl("sigma2_regime", colnames(params))]

    invisible(result)
}

## G is the regime effect matrix relative to the test data
SARXforecast <- function(data, reg_part, G, sample, horizon){
    if("sigma2_regime" %in% ls(sample)){
        rm(list = c("sigma2_regime"), envir = sample) ## useless for forecast
    }
    order = dim(sample$ARpart)[2]
    params = sample$ARpart
    
    for (i in 1:order) {
        colName = sprintf("Y[t-%s]", i)
        params[colName] = data[length(data) - i]
    }
    
    ## build a structure: "parameter | lagged values" for each component in fit
    for (v in ls(sample)) {
        if (!v %in% c("intercept", "sigma2", "ARpart", "gamma")) {
            ## add parameter block
            params = cbind(params, sample[[v]])

            for (i in 1:reg_part[[v]][[2]]) {
                colName = sprintf("%s[t-%s]", v, i)
                params[colName] = reg_part[[v]][[1]][length(reg_part[[v]][[1]]) - i]
            }
        }
    }

    ## add regime effect
    ## standardize factor names
    colnames(G) <- lapply(seq(1:dim(G)[2]), function(x){
        return(sprintf("F%d", x))
    })
    params = cbind(params, sample$gamma)
    params = cbind(params, t(replicate(dim(params)[1], G[1,])))
    
    params['intercept'] = sample$intercept
    params['sigma2']    = sample$sigma2
    
    forecast = matrix(NA, nrow = nrow(params), ncol = horizon)

    tot <- 2*order
    for(var in ls(reg_part)){
        tot <- tot + (2 * reg_part[[var]][[2]])
    }
    tot <- tot + 2*dim(G)[2]

    nFactor = dim(G)[2]
    
    for(i in 1:horizon){
        forecast[, i] = apply(params, 1,
                              function(x){
                                  ## simulate the SARX process
                                  mean = x[tot + 1]

                                  ## autoregressive part
                                  for (i in 1:order){
                                      mean = mean + (x[i] * x[i + order])
                                  } 
                                  k = 2 * order

                                  ## regressive part
                                  for(var in ls(reg_part)){
                                      varOrder = reg_part[[var]][[2]]
                                      for (i in 1:varOrder){
                                          mean = mean + (x[k + i] * x[k + i + varOrder])
                                      }
                                      k = k + (2 * varOrder)
                                  }
                                  
                                  ## seasonal part
                                  for(i in 1:nFactor){
                                      mean = mean + (x[k + i] * x[k + i + nFactor])
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

        ## move seasonal factor one step ahead
        for(j in seq(1, nFactor)){
            colName = sprintf("F%d", j)
            params[colName] = G[1 + i, j]
        }
    }

    ## each column of forecast is a sample from the predictive distribution
    ## of y[t+h] given the past
    invisible(forecast) # return the forecast as an M x horizon matrix    
}

SARXLiveForecast <- function(Ytrain, Xtrain, Ytest, Xtest, Gtrain, Gtest, sample, horizon){
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
    G = rbind(Gtrain[(dim(Gtrain)[1] - horizon):dim(Gtrain)[1],], Gtest)
    
    for(t in seq(maxOrder + horizon + 1, length(response))){ ## can't know G more than this
        ## supply to SARXforecast data as they would arrive in real world
        ## hour by hour
        liveData = new.env(hash = TRUE)
        for (var in ls(data)) {
            varOrder = data[[var]][[2]]
            liveData[[var]] = list(data[[var]][[1]][(t-varOrder-horizon):(t-horizon)], data[[var]][[2]])
        }
        liveResponse = response[(t-order-horizon):(t-horizon)]
        partialForecast = SARXforecast(liveResponse, liveData, G[(t-maxOrder-horizon):(t-maxOrder),], sample, horizon)
        
        ## save the forecast
        if ((t - maxOrder - horizon - 1) <= l_test) {
            forecast[, t - maxOrder - horizon - 1] = partialForecast[, horizon]
        }
    }
    
    invisible(forecast)
}

SARXforecastRange <- function(Ytrain, Xtrain, Ytest, Xtest, Gtrain, Gtest, sample, range){
    if (length(range) != 2) {
        return()
    }
    
    movie = new.env(hash = TRUE)
    for (i in seq(range[1], range[2])) {
        print(i)
        movie[[sprintf("%d", i)]] = SARXLiveForecast(Ytrain, Xtrain, Ytest, Xtest, Gtrain,Gtest, sample, i)
    }
    
    invisible(movie)
}
