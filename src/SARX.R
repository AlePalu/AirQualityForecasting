## ARX with switching regime (LMM)

## load libraries...
library(readr);

library(rstan);   ## R interface to stan
library(parallel) ## parallel computing
library(rjags)    ## R interface to JAGS
library(coda)

## plot libraries
library(ggplot2);
library(gridExtra)
library(purrr);
library(dplyr);

options(warns = -1)

## import custom functions
source("DataProcessUtils.r")
source("SARXUtils.R")
source("PlotUtils.r")

## import data
data = read_csv("../data/tsData.csv", col_types=cols());
head(data)

## introdue a new dummy variable day/night
startNightH <- strptime("22:00:00", format = "%H:%M:%S")
endNightH   <- strptime("06:00:00", format = "%H:%M:%S")

dateTimeVector = data %>% pull(created_at)

nightDummy <- lapply(dateTimeVector, function(x){
    time = strptime(format(x, "%H:%M:%S"), format = "%H:%M:%S")

    ## check if night
    if(time >= startNightH || time <= endNightH){
        return(1)
    } else {
        return(0)
    }
})

## add the dummy to the dataset
data$night = as.numeric(unlist(nightDummy))

## focus on the time series we are interested in
interestingPot <- 1091
startDate      <- "2020-09-01"
endDate        <- "2020-10-27" ## move endDate one week before
fd             <- 7 ## one week simulation

## train dataset
Ytrain1W <- data[data$created_at > parse_datetime(startDate) &
                 data$created_at < parse_datetime(endDate) &
                 data$pot_id == interestingPot, ]
Ytrain1W <- Ytrain1W[!is.na(Ytrain1W$pm2p5SPS), ] ## drop NaN

## test dataset
Ytest1W <- data[data$created_at >= parse_datetime(endDate) &
                data$created_at < parse_datetime(as.character(as.Date(endDate) + fd)) &
                data$pot_id == interestingPot, ]
Ytest1W <- Ytest1W[!is.na(Ytest1W$pm2p5SPS),] ## drop NaN

## LMM autoregressive stan model
stanModel = "
data{
        int<lower = 0> N;       // number of obs
        int<lower = 0> K;       // number of covariates (including the intercept)
        int<lower = 0> nGr;     // number of groups

        vector[N] Y;            // response
        matrix[N, K] X;	        // covariates (fixed effects)
        matrix[N, nGr] G;       // groups allocation (regime effect)

	// prior parameters
        vector[K] mean_theta;	
        vector[K] var_theta;
        real<lower = 0> scale_s2;
}

parameters{
        real<lower = 0> sigma2;
        vector[nGr] sigma2_regime;

        vector[K] theta;    // regression coefficients
        vector[nGr] gamma;  // regime specific effect
}

transformed parameters{
        vector[N] mu;
        for(i in 1:N){
            mu[i] = row(X, i) * theta + row(G, i) * gamma;
        }
}

model{
        // Prior:
        sigma2 ~ inv_gamma(2., scale_s2);
        for(k in 1:K) {
            theta[k] ~ normal(mean_theta[k], pow(var_theta[k], 0.5));
        }

        for(j in 1:nGr){
            sigma2_regime[j] ~ inv_gamma(2,10);
            gamma[j] ~ normal(0.0, pow(sigma2_regime[j], 0.5));
        }

        // Likelihood:
        Y ~ normal(mu, pow(sigma2, 0.5));
}

generated quantities{
        vector[N] log_lik;
        for (j in 1:N) {
            log_lik[j] = normal_lpdf(Y[j] | mu[j], pow(sigma2, 0.5));
        }
}
"

## compile model
ARXSRmodel <- stan_model(model_code = stanModel)

## we want 4 different regimes depending on the variables weekend and night
night = as.factor(Ytrain1W$night)
weekend = as.factor(Ytrain1W$weekend)

## create design matrix for regime effect
G <- model.matrix(~ night + weekend,
                  contrasts.arg = list(night = contrasts(night, contrasts = F),
                                       weekend = contrasts(weekend, contrasts = F))
                  )[,-1]

## columns of G are, in order: night0 night1 weekend0 weekend1
## rename for clarity
colnames(G) <- c("day", "night", "weekday", "weekend")
head(G)

## transform into a unique factor
uniqueF <- apply(G,1,function(x){
    ## associate to each row a factor identifier
    ID = 0
    for(i in 1:length(x)){
        ID = ID + x[i]*2^i
    }
    return(ID)
})

uniqueF <- as.factor(uniqueF)
## levels are
## 10 -> 1010: day   - weekday
## 12 -> 0110: night - weekday
## 18 -> 1001: day   - weekend
## 20 -> 0101: night - weekend

W <- model.matrix(~ uniqueF,
                  contrasts.arg = list(uniqueF = contrasts(uniqueF, contrasts = F))
                  )[,-1]

## columns of G are, in order: uniqueF10 uniqueF12 uniqueF18 uniqueF20
## rename for clarity
colnames(W) <- c("DD", "ND", "DE", "NE")
head(W)
## levels are
## 1000 -> day   - weekday (DD)
## 0100 -> night - weekday (ND)
## 0010 -> day   - weekend (DE)
## 0001 -> night - weekend (NE)

## fit the model
p <- 7                    ## order of autoregression

## external regressors
regressors      <- new.env(hash = TRUE) ## create hashmap in R
regressors$temp <- list(Ytrain1W$temperature_sht, p)
regressors$hum  <- list(Ytrain1W$humidity_sht, p)
regressors$wind <- list(Ytrain1W$wind, p)
regressors$rain <- list(Ytrain1W$rain, p)

## get sample from model
pm2p5 <- Ytrain1W$pm2p5SPS ## response to predict

ARXSRsample <- ARXSRfit(ARXSRmodel, ## compiled stan model
                        pm2p5,      ## response
                        p,          ## autoregression order
                        regressors, ## regressors
                        W)          ## regime effect design matrix

## check regime effect distribution a posteriori
plotPosteriorDensity(ARXSRsample, c("gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]"))

## try only weekend

weekend = as.factor(Ytrain1W$weekend)
## create design matrix for regime effect
Gw <- model.matrix(~ weekend,
                  contrasts.arg = list(weekend = contrasts(weekend, contrasts = F))
                  )[,-1]

## columns of G are, in order: night0 night1 weekend0 weekend1
## rename for clarity
colnames(Gw) <- c("weekday", "weekend")
head(Gw)

Gfull <- t(apply(Gw, 1, function(x){rep(x, each = (p+1))})) ## +1 for intercept

## build the autoregressive design matrix
ARdesignMatrix <- data.frame(intercept = rep(1, length(pm2p5)))
for (t in 1:p) {
    columnName <- sprintf("Y[t-%d]", t)
    ARdesignMatrix[, columnName] <- shift(pm2p5, t)
}

## replicate design matrix nGr times
ARdesignMatrix <- t(apply(ARdesignMatrix, 1, function(x){rep(x, length(colnames(Gw)))}))

## now musk ARdesignMatrix by the dummy matrix
ARdesignMatrix <- ARdesignMatrix * Gfull

factorNames <- c()
for(i in 1:dim(ARdesignMatrix)[2]){
    factorNames <- c(factorNames, sprintf("%s - %s", colnames(ARdesignMatrix)[i], colnames(Gfull)[i]))
}

colnames(ARdesignMatrix) <- factorNames

## inspect ARdesignMatrix
head(ARdesignMatrix)

ARXSRsample <- ARXSRfit(ARXSRmodel,      ## compiled stan model
                        pm2p5,           ## response
                        p,               ## autoregression order
                        regressors,      ## regressors
                        ARdesignMatrix)  ## regime effect design matrix

plotPosteriorDensity(ARXSRsample, c("gamma[10]", "gamma[11]"))

gammaDF <- data.frame(rstan::extract(ARXSRsample, perm = TRUE))
gammaDF <- gammaDF[, grepl("gamma", colnames(gammaDF))]

plotPosteriorBoxplot(gammaDF, colnames(ARdesignMatrix))

## try only night/day

night = as.factor(Ytrain1W$night)
## create design matrix for regime effect
Gw <- model.matrix(~ night,
                  contrasts.arg = list(night = contrasts(night, contrasts = F))
                  )[,-1]

## columns of G are, in order: night0 night1 weekend0 weekend1
## rename for clarity
colnames(Gw) <- c("day", "night")
head(Gw)

Gfull <- t(apply(Gw, 1, function(x){rep(x, each = (p+1))})) ## +1 for intercept

## build the autoregressive design matrix
ARdesignMatrix <- data.frame(intercept = rep(1, length(pm2p5)))
for (t in 1:p) {
    columnName <- sprintf("Y[t-%d]", t)
    ARdesignMatrix[, columnName] <- shift(pm2p5, t)
}

## replicate design matrix nGr times
ARdesignMatrix <- t(apply(ARdesignMatrix, 1, function(x){rep(x, length(colnames(Gw)))}))

## now musk ARdesignMatrix by the dummy matrix
ARdesignMatrix <- ARdesignMatrix * Gfull

factorNames <- c()
for(i in 1:dim(ARdesignMatrix)[2]){
    factorNames <- c(factorNames, sprintf("%s - %s", colnames(ARdesignMatrix)[i], colnames(Gfull)[i]))
}

colnames(ARdesignMatrix) <- factorNames

## inspect ARdesignMatrix
head(ARdesignMatrix)

ARXSRsample <- ARXSRfit(ARXSRmodel,      ## compiled stan model
                        pm2p5,           ## response
                        p,               ## autoregression order
                        regressors,      ## regressors
                        ARdesignMatrix)  ## regime effect design matrix

gammaDF <- data.frame(rstan::extract(ARXSRsample, perm = TRUE))
gammaDF <- gammaDF[, grepl("gamma", colnames(gammaDF))]

plotPosteriorBoxplot(gammaDF, colnames(ARdesignMatrix))



## fare codice per la forecast
