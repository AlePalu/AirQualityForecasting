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
source("ARUtils.R")
source("PlotUtils.R")
source("DataProcessUtils.R")

data = read_csv("../data/tsData.csv", col_types=cols());
head(data)

## the following function returns a sample from an ARX(p) model with SSVS priors
## data is a vector of real values (the time series we want to predict)
## reg_part is a hashmap associating to each key a vector of real values (the time series
##          we use as external regressor)
## p is an integer indicating the order of autoregression
## JAGSmodel is the path to the file containing the JAGS description of the model
SSVS_ARX = function(data, reg_part, p, JAGSmodel) {
    ## train model in JAGS
    c_p <- 100
    intersect <- 0.01
    tau_p <- intersect / sqrt(2 * log(c_p) * c_p^2 / (c_p^2 - 1))

    ## create dataset for ARX(p)  
    d <- dataPrepare(data, reg_part, p)
    
    ## number of parameters
    k <- ((length(reg_part) + 1) * p) + 1
    
    data_JAGS <- list(
        N = nrow(d),         ## number of points
        K = k,               ## number of covariates (including intercept)
        Y = data[1:nrow(d)], ## series we want to predict
        X = d,               ## matrix of covariates
        tau = tau_p,         ## variance of spike component
        c = c_p              ## scaling factor for slab variance
    )
  
    ## init parameters
    init = function() {
        list(beta = rep(0, k), g = rep(0, k))
    }
  
    ## prepare JAGS
    model <- jags.model(JAGSmodel,
                        data = data_JAGS,
                        n.adapt = 1000,
                        inits = init,
                        n.chains = 1)
    
    update(model, n.iter = 500)
    
    ## parameters we want JAGS to return 
    param <- c("beta", "g", "mdl")
    
    ## sample from model...
    output <- coda.samples(model = model,
                           variable.names = param,
                           n.iter = 50000,
                           thin = 10)
    
    invisible(as.matrix(output)) ## return output
}

## prepare data
interestingPot <- 1091
startDate      <- "2020-09-01"
endDate        <- "2020-10-27"
fd             <- 7 ## one week simulation

data_potid <- data[data$created_at > parse_datetime(startDate) &
                   data$created_at < parse_datetime(endDate) &
                   data$pot_id == interestingPot, ]
data_potid <- data_potid[!is.na(data_potid$pm2p5SPS), ] ## remove NaN

pm2p5 <- data_potid$pm2p5SPS ## response to predict
p <- 10                      ## order of autoregression

## regressive part
input_data_all      <- new.env(hash = TRUE) ## create hashmap in R
input_data_all$temp <- data_potid$temperature_sht
input_data_all$hum  <- data_potid$humidity_sht
input_data_all$wind <- data_potid$wind
input_data_all$rain <- data_potid$rain

## make the MCMC simulation
ARXSSVSmodel = SSVS_ARX(
  pm2p5,
  input_data_all,
  10,
  "modelFiles/ARX_SSVS.bug"
)

save(ARXSSVSmodel, file = "AR10SSVS.dat")

## load data without running JAGS...
load("AR10SSVS.dat")

## MPM

## the matrix of gamma variables indicating inclusion or not of the covariate in the model
g <- ARXSSVSmodel[, grepl("g", colnames(ARXSSVSmodel))]
g_mean <- apply(g, 2, mean)

tempData <- dataPrepare(pm2p5, input_data_all, 10)

## posterior inclusion probabilities
x11()
p3 <- data.frame(value = g_mean, var = colnames(tempData)) %>%
    ggplot(aes(y = value, x = var, fill = var)) + 
    geom_bar(stat="identity") + 
    geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
    coord_flip() + 
    theme_minimal() + 
    theme(legend.position="none") + 
    ylab("posterior inclusion probabilities") + 
    xlab("")
p3

## most relevant variables a posteriori
mp <- as.vector(which(g_mean > 0.5))
g_mean[mp]

## HPD

mdl = ARXSSVSmodel[, "mdl"]
unique_model = unique(mdl)

d_hpd = data.frame(model = unique_model , freq = 0)
d_hpd$freq = apply (d_hpd , 1 , function ( row ) {
  model = row [1]
  count = 0
  for ( i in 1: length ( mdl ) ) {
    if ( model == mdl [ i ]) { count = count + 1}
  }
  count
})
HDPmodel = d_hpd[which.max(d_hpd$freq), ]

HDPmodel

## features of most visited model
colnames(dd)[as.logical(g[which.max(d_hpd$freq), ])]
