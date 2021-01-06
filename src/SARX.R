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
startNightH <- strptime("19:00:00", format = "%H:%M:%S")
endNightH   <- strptime("05:00:00", format = "%H:%M:%S")

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

## extract hour from datetime index
hour <- lapply(dateTimeVector, function(x){
    return(format(x, "%H:%M"))
})

day <- lapply(dateTimeVector, function(x){
    return(weekdays(as.Date(x)))
})

## add the dummy to the dataset
data$night <- as.numeric(unlist(nightDummy))
data$hour  <- as.factor(unlist(hour))
data$day   <- as.factor(unlist(day))

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

## autoregressive stan model with seasonal effect
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
SARXmodel <- stan_model(model_code = stanModel)

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

Wfull <- t(apply(W, 1, function(x){rep(x, each = (p+1))})) ## +1 for intercept

ARdesignMatrix <- data.frame(intercept = rep(1, length(pm2p5)))
for (t in 1:p) {
    columnName <- sprintf("Y[t-%d]", t)
    ARdesignMatrix[, columnName] <- shift(pm2p5, t)
}

## replicate ARdesignMatrix for each effect
ARdesignMatrix <- t(apply(ARdesignMatrix, 1, function(x){rep(x, length(colnames(W)))}))

## now musk ARdesignMatrix by the dummy matrix
ARdesignMatrix <- ARdesignMatrix * Wfull
head(ARdesignMatrix)

## sample from this model
ARXSRsample <- SARXfit(ARXSRmodel, ## compiled stan model
                        pm2p5,      ## response
                        0,          ## autoregression order
                        regressors, ## regressors
                        ARdesignMatrix)          ## regime effect design matrix

gammaDFfull <- data.frame(rstan::extract(ARXSRsample, perm = TRUE))
gammaDFfull <- gammaDFfull[, grepl("gamma", colnames(gammaDFfull))]

factorNames <- c()
for(i in 1:dim(ARdesignMatrix)[2]){
    factorNames <- c(factorNames, sprintf("%s - %s", colnames(ARdesignMatrix)[i], colnames(Wfull)[i]))
}

colnames(ARdesignMatrix) <- factorNames

plotPosteriorBoxplot(gammaDFfull[,25:32], colnames(ARdesignMatrix)[25:32])

## check regime effect distribution a posteriori
plotPosteriorDensity(ARXSRsample, c("gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]"))

## try only weekend

## create design matrix for seasonal effect
Gw <- matrix(Ytrain1W$weekend)

head(Gw)

Gfull <- t(apply(Gw, 1, function(x){rep(x, each = (p+1))})) ## +1 for intercept

## build the autoregressive design matrix
ARdesignMatrix <- data.frame(intercept = rep(1, length(pm2p5)))
for (t in 1:p) {
    columnName <- sprintf("Y[t-%d]", t)
    ARdesignMatrix[, columnName] <- shift(pm2p5, t)
}

## now musk ARdesignMatrix by the dummy matrix
ARdesignMatrix <- ARdesignMatrix * Gfull

## inspect ARdesignMatrix
head(ARdesignMatrix)

ARXSRsample <- ARXSRfit(ARXSRmodel,      ## compiled stan model
                        pm2p5,           ## response
                        p,               ## autoregression order
                        regressors,      ## regressors
                        ARdesignMatrix)  ## regime effect design matrix

plotPosteriorDensity(ARXSRsample, c("gamma[1]", "gamma[2]", "gamma[3]"))

gammaDF <- data.frame(rstan::extract(ARXSRsample, perm = TRUE))
gammaDF <- gammaDF[, grepl("gamma", colnames(gammaDF))]

x11()
plotPosteriorBoxplot(gammaDF, colnames(ARdesignMatrix))

## try only night/day

## create design matrix for regime effect
Gn <- matrix(Ytrain1W$night)
head(Gn)

Gfull <- t(apply(Gn, 1, function(x){rep(x, each = (p+1))})) ## +1 for intercept

## build the autoregressive design matrix
ARdesignMatrix <- data.frame(intercept = rep(1, length(pm2p5)))
for (t in 1:p) {
    columnName <- sprintf("Y[t-%d]", t)
    ARdesignMatrix[, columnName] <- shift(pm2p5, t)
}

## musk ARdesignMatrix by the dummy matrix
ARdesignMatrix <- ARdesignMatrix * Gfull

## inspect ARdesignMatrix
head(ARdesignMatrix)

ARXSRsampleNight <- ARXSRfit(ARXSRmodel,      ## compiled stan model
                             pm2p5,           ## response
                             p,               ## autoregression order
                             regressors,      ## regressors
                             ARdesignMatrix)  ## regime effect design matrix

gammaDFnight <- data.frame(rstan::extract(ARXSRsampleNight, perm = TRUE))
gammaDFnight <- gammaDFnight[, grepl("gamma", colnames(gammaDFnight))]

plotPosteriorBoxplot(gammaDFnight, colnames(ARdesignMatrix))

## weekend/weekday + day/night

Gwn <- matrix(Ytrain1W$weekend)
Gwn <- cbind(Gwn, Ytrain1W$night)

colnames(Gwn) <- c("weekend", "night")

Gwnfull <- t(apply(Gwn, 1, function(x){rep(x, each = (p+1))})) ## +1 for intercept

ARdesignMatrix <- data.frame(intercept = rep(1, length(pm2p5)))
for (t in 1:p) {
    columnName <- sprintf("Y[t-%d]", t)
    ARdesignMatrix[, columnName] <- shift(pm2p5, t)
}

## replicate ARdesignMatrix for each effect
ARdesignMatrix <- t(apply(ARdesignMatrix, 1, function(x){rep(x, length(colnames(Gwn)))}))

## now musk ARdesignMatrix by the dummy matrix
ARdesignMatrix <- ARdesignMatrix * Gwnfull
head(ARdesignMatrix)

SARXsampleNW <- SARXfit(ARXSRmodel,      ## compiled stan model
                        pm2p5,           ## response
                        0,               ## autoregression order
                        regressors,      ## regressors
                        ARdesignMatrix)  ## regime effect design matrix

## save the sampling
save(SARXsampleNW, file = "SARXsampleNW.dat")

factorNames <- c()
for(i in 1:dim(ARdesignMatrix)[2]){
    factorNames <- c(factorNames, sprintf("%s - %s", colnames(ARdesignMatrix)[i], colnames(Gwnfull)[i]))
}

colnames(ARdesignMatrix) <- factorNames

gammaDFnw <- data.frame(rstan::extract(SARXsampleNW, perm = TRUE))
gammaDFnw <- gammaDFnw[, grepl("gamma", colnames(gammaDFnw))]

plotPosteriorBoxplot(gammaDFnw, colnames(ARdesignMatrix))


## fare codice per la forecast

## only intercept for weekend
SARXsampleOW <- SARXfit(ARXSRmodel,      ## compiled stan model
                        pm2p5,           ## response
                        p,               ## autoregression order
                        regressors,      ## regressors
                        data.frame(Ytrain1W$weekend))  ## regime effect design matrix

gammaDFow <- data.frame(rstan::extract(SARXsampleOW, perm = TRUE))
gammaDFow <- gammaDFow[, grepl("gamma", colnames(gammaDFow))]

plotPosteriorDensity(SARXsampleOW, c("gamma"))


## model with 24 hourly factor
hour <- Ytrain1W$hour
Gh <- model.matrix(~ hour,
                   contrasts.arg = list(hour = contrasts(hour, contrasts = F))
                  )[,-1]

## sample from this model
SARXsampleH <- SARXfit(ARXSRmodel,      ## compiled stan model
                       pm2p5,           ## response
                       p,               ## autoregression order
                       regressors,      ## regressors
                       Gh)  ## regime effect design matrix

save(SARXsampleH, file = "SARXsampleH.dat")

gammaDFh <- data.frame(rstan::extract(SARXsampleH, perm = TRUE))
gammaDFh <- gammaDFh[, grepl("gamma", colnames(gammaDFh))]

plotPosteriorBoxplot(gammaDFh, levels(hour))

## pot in Milano centro (loreto)
interestingPot <- 1018
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

p <- 7                    ## order of autoregression

## external regressors
regressors      <- new.env(hash = TRUE) ## create hashmap in R
regressors$temp <- list(Ytrain1W$temperature_sht, p)
regressors$hum  <- list(Ytrain1W$humidity_sht, p)
regressors$wind <- list(Ytrain1W$wind, p)
regressors$rain <- list(Ytrain1W$rain, p)

## get sample from model
pm2p5 <- Ytrain1W$pm2p5SPS ## response to predict

## try 24 hour factor model with this series

## model with 24 hourly factor
hour <- Ytrain1W$hour
Gh <- model.matrix(~ hour,
                   contrasts.arg = list(hour = contrasts(hour, contrasts = F))
                  )[,-1]

## sample from this model
SARXsampleH2 <- SARXfit(SARXmodel,      ## compiled stan model
                        pm2p5,           ## response
                        p,               ## autoregression order
                        regressors,      ## regressors
                        Gh)  ## regime effect design matrix

save(SARXsampleH2, file = "SARXsampleHLoreto.dat")

gammaDFh <- data.frame(rstan::extract(SARXsampleH2, perm = TRUE))
gammaDFh <- gammaDFh[, grepl("gamma", colnames(gammaDFh))]

plotPosteriorBoxplot(gammaDFh, levels(hour))

## forecast

hourTest <- Ytest1W$hour
Ghtest <- model.matrix(~ hourTest,
                       contrasts.arg = list(hourTest = contrasts(hourTest, contrasts = F))
                       )[,-1]

parameters <- SARXextract(SARXsampleH2, regressors, p)


forecast <- SARXforecast(data = pm2p5,
                         reg_part = regressors,
                         G = Ghtest,
                         sample = parameters,
                         horizon = 24)

test_regressors      <- new.env(hash = TRUE) ## create hashmap in R
test_regressors$temp <- list(Ytest1W$temperature_sht, p)
test_regressors$hum  <- list(Ytest1W$humidity_sht, p)
test_regressors$wind <- list(Ytest1W$wind, p)
test_regressors$rain <- list(Ytest1W$rain, p)

liveForecast <- SARXLiveForecast(
    Ytrain    = Ytrain1W$pm2p5SPS,
    Xtrain    = regressors,
    Ytest     = Ytest1W$pm2p5SPS,
    Xtest     = test_regressors,
    G         = Ghtest,
    sample    = parameters,
    horizon   = 2
)

plotForecast(Ytest1W$pm2p5SPS, Ytrain1W$pm2p5SPS, liveForecast, p, "SARX(7) model - pm2p5 Forecast. Time horizon: 2h")

movie <- SARXforecastRange(
    Ytrain    = Ytrain1W$pm2p5SPS,
    Xtrain    = regressors,
    Ytest     = Ytest1W$pm2p5SPS,
    Xtest     = test_regressors,
    G         = Ghtest,
    sample    = parameters,
    range     = c(1,12)  ## from 1 hour to 12 hours forecast
)

## model with 7 day factor
day <- Ytrain1W$day

Gd <- model.matrix(~ day,
                   contrasts.arg = list(day = contrasts(day, contrasts = F))
                   )[,-1]

SARXsampleD <- SARXfit(SARXmodel,      ## compiled stan model
                       pm2p5,           ## response
                       p,               ## autoregression order
                       regressors,      ## regressors
                       Gd)  ## regime effect design matrix

save(SARXsampleD, file = "SARXsampleDLoreto.dat")

gammaDFd <- data.frame(rstan::extract(SARXsampleD, perm = TRUE))
gammaDFd <- gammaDFd[, grepl("gamma", colnames(gammaDFd))]

dat <- stack(as.data.frame(gammaDFd))
dat$ind <- rep(levels(day), each = dim(SARXsampleD)[1])

x11()
ggplot(dat) +
    geom_boxplot(aes(x = ind, y = values, fill = ind)) +
    geom_hline(aes(yintercept = 0), col = 2, lty = 2) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
    theme(legend.position = "null") +
    xlab("") + 
    ylab("")


plotPosteriorBoxplot(gammaDFd, levels(day))

