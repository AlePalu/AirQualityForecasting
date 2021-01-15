## shift a time series of k steps, putting k trailing NaN
shift = function(data, k){
    result = sapply(1:length(data), function(x) data[x+k])
    invisible(result)
}

## prepare a dataset ready to be supplied for ARX(p) fitting
## y: vector of response variable
## X: hashmap of covariates
## p: order of autoregression
dataPrepare = function(y, X, p) {
    d <- data.frame(intercept = rep(1, length(y)))
    for (t in 1:p) {
        columnName <- sprintf("t-%d", t)
        d[, columnName] <- shift(y, t)
    }
    ## add the X part
    for (var in ls(X)) {
        for (t in 1:p) {
            columnName <- sprintf("%s-%d", var, t)
            d[, columnName] <- shift(X[[var]], t)
        }
    }
    ## last p rows have NaN due to shifting, not consider them
    d <- d[0:(nrow(d) - p), ]

    invisible(d)
}
