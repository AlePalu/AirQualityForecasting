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
ARXSRfit <- function(ARXSRmodel, data, p, reg_part, W, inits){
    ## create dataset for ARXSR(p)
    d <- data.frame(intercept = rep(1, length(data)))
    for (t in 1:p) {
        columnName <- sprintf("t-%d", t)
        d[, columnName] <- shift(data, t)
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
    ## last pMax rows have NaN due to shifting, not consider them
    numRows = nrow(d)
    d <- d[0:(numRows - pMax), ]

    ## number of parameters
    k <- p + 1
    for (var in ls(reg_part)){
        k <- k + reg_part[[var]][[2]]
    }

    ## number of groups
    nGroups <- dim(W)[2]
    W <- W[0:(numRows - pMax), ]

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
        list(theta = rep(0.1, k), 
             gamma = rep(0.1, nGroups),
             sigma2_regime = rep(10, nGroups))
    }
    
    ## data are now ready to be supplied to Stan
    ARXSRsample <- sampling(ARXSRmodel, init = inits,
                          data = stanData, chains = 1,
                          iter = 8000, warmup = 1000,
                          show_messages = FALSE
                          )
    
    invisible(ARXSRsample) ## return output of stan
}
