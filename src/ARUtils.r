source("DataProcessUtils.r")

## the following function returns a sample from an AR(p) model
## ARmodel is a compiled stan model, obtained from stan_model(...)
## data is a vector of real values (the time series)
## p is an integer indicating the order of autoregression
ARfit <- function(ARmodel, data, p){
    ## create dataset for AR(p)
    d = data.frame(intercept = rep(1, length(data)))
    for (t in 1:p){
        columnName = sprintf("t-%d", t)
        d[,columnName] = shift(data, t)
    }
    ## last p rows have NaN due to shifting, not consider them
    d = d[0:(nrow(d)-p),]

    stanData = list(N        = nrow(d),
                    K        = p+1,
                    Y        = data[1:nrow(d)],
                    X        = d,
                     ## prior parameters
                    scale_s2 = 10)
    
    ## init regression coefficient parameter to their MLE
    X = as.matrix(d)[,2:ncol(d)]
    linReg = lm(data[1:nrow(d)] ~ X)

    stanData$mean_beta = summary(linReg)$coefficients[,1]
    stanData$var_beta  = (summary(linReg)$coefficients[,2])^2

    ## data are now ready to be supplied to Stan
    ARsample = sampling(ARmodel,
                        data = stanData, chains = 2,
                        iter = 3000, warmup = 1000,
                        show_messages = FALSE)

    invisible(ARsample) ## return output of stan
}

## extract parameters from MCMC sample returned by stan as an hashmap
## sample: the MCMC sample from which extract
ARextract <- function(sample) {
    # extract parameters from MCMC sample 
    params = data.frame(rstan::extract(sample, perm = T))
    params['lp__'] = NULL

    result           <- new.env(hash = TRUE)
    result$intercept <- params["beta.1"]
    result$sigma2    <- params["sigma2"]
    
    ARpart <- params[, grepl("beta", colnames(params))]
    ARpart["beta.1"] <- NULL ## remove intercept from ARpart

    result$ARpart    <- ARpart
    invisible(result)
}

## compute forecast distribution of AR(p) models
## data is a vector of real values containing at least p values
## sample: hashmap as produced by ARextract
## horizon: number of time steps we need to simulate, starting from the last point in data
ARforecast <- function(data, sample, horizon){
    order = dim(sample$ARpart)[2]
    params = data.frame(sample$ARpart)
    
    # add as many lags as the order of the AR
    for(i in 1:order){
        colName = sprintf("t-%s", i)
        params[colName] = data[length(data)-(i-1)]
    }
    
    params['intercept'] = sample$intercept
    params['sigma2']    = sample$sigma2   
    # params is a temporary dataframe organized as: AR coefficients | lags | intercept | sigma2
    
    forecast = matrix(NA, nrow = nrow(params), ncol = horizon)
    
    for(i in 1:horizon){
        forecast[, i] = apply(params, 1,
                                 function(x){
                                     # simulate the AR process
                                     mean = x[2*order+1]
                                     for(i in 1:order)
                                         mean = mean + x[i]*x[order+i]
                                     return(rnorm(1, mean, sqrt(x[2*order+2])))
                                 }
                             )
        ## shift columns
        if(order > 1){
            for(j in order:2){
                col1 = sprintf("t-%s", j)
                col2 = sprintf("t-%s", j-1)
                params[col1] = params[col2]
            }
        }
        params["t-1"] = forecast[, i]
    }

    ## each column of forecast is a sample from the predictive distribution
    ## of y[t+h] given the past
    invisible(forecast) # return the forecast as an M x horizon matrix
}

## compute the forecast as data arrives time step by time step
## data:vector of real values (the time series)
## sample: hashmap as produced by ARextract
## horizon: if we are at time t, the function will compute the predictive distribution
##          of Y_{t+h} for all h between 1 and horizon
## block: boolean indicating if data must be moved in blocks of length horizon
## the function returns a forecast matrix valid to be plotted by plotForecast
ARLiveForecast <- function(trainData, testData, sample, horizon, block = FALSE) {
    order = dim(sample$ARpart)[2]
    params = data.frame(sample$ARpart)

    data = c(trainData[(length(trainData) - order - horizon):length(trainData)], testData)    
    forecast = matrix(NA, nrow = nrow(params), ncol = length(testData))
    
    if (block == TRUE) {
        for (t in seq(order, length(data), by = horizon)) {
            ## supply to ARforecast data as they would arrive in real world
            ## in blocks, namely we simulate the arrival of new data every horizon steps

            liveData = data[(t - order + 1):t]
            partialForecast = ARforecast(liveData, sample, horizon)

            ## save the forecast
            for (i in 1:horizon) {
                if (t + i <= length(data)) { ## prediction out of available data!
                    forecast[, t + i] = partialForecast[, i]
                }
            }
        }
    } else {
        for (t in seq(order + horizon + 1, length(data))) {
            ## supply to ARforecast data as they would arrive in real world
            ## hour by hour
            liveData = data[(t - order - horizon + 1):(t - horizon)]
            partialForecast = ARforecast(liveData, sample, horizon)

            ## save the forecast
            if ((t - order - horizon - 1) <= length(testData)) {
                forecast[, t - order - horizon - 1] = partialForecast[, horizon]
            }
        }
    }

    invisible(forecast)
}


## compute a sequence of forecast matrix with horizon ranging from range[1] to range[2]
## trainData: vector of real values used for training(the time series)
## testData: vector of real values against which compare the prediction
## sample: hashmap as produced by ARextract
## range: vector of two elements indicating minumim and maximum time horizon range to span
## the function returns an hashmap of forecast matrices valid to be plotted and transformed in GIF format
ARforecastRange <- function(trainData, testData, sample, range) {
    if (length(range) != 2) {
          return()
    }

    movie = new.env(hash = TRUE)
    for (i in seq(range[1], range[2])) {
        movie[[sprintf("%d", i)]] = ARLiveForecast(trainData, testData, sample, i)
    }

    invisible(movie)
}
