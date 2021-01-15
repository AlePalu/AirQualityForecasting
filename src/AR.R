## AR(p)

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

## import data
data = read_csv("../data/tsData.csv", col_types=cols());
head(data)

## focus on the time series we are interested in
interestingPot = 1091
startDate      = "2020-09-01"
endDate        = "2020-11-03"

## number of days to forecast (this will be used to build the test set)
fd = 1

## train dataset
Ytrain = data$pm2p5SPS[data$created_at > parse_datetime(startDate) &
                       data$created_at < parse_datetime(endDate)   &
                       data$pot_id == interestingPot];
Ytrain = Ytrain[!is.na(Ytrain)]; # drop NaN

## test set
Ytest = data$pm2p5SPS[data$created_at >= parse_datetime(endDate) & 
                      data$created_at < parse_datetime(as.character(as.Date(endDate) + fd)) &
                      data$pot_id == interestingPot];
Ytest = Ytest[!is.na(Ytest)]; # drop NaN

## compile model
ARmodel <- stan_model(file = "modelFiles/AR.stan")

## we try a simple AR(2) model
AR2sample <- ARfit(ARmodel, Ytrain, 2)

## AR2sample now is an object from which we can extract posterior MCMC samples of parameters

## plot posterior densities of AR(2) parameters...
plotPost(AR2sample, c("beta[1]", "beta[2]", "beta[3]")) 

## plot of the forecast produced by an AR(p) model... example with an AR(10)

AR10sample <- ARfit(ARmodel, Ytrain, 10) ## get the MCMC sample

# extract parameters from MCMC sample
parameters <- ARextract(AR10sample)
## parameters is an hashmap such that
## parameters$intercept contains the MCMC sample for the intercept parameter
## parameters$sigma2    contains the MCMC sample for the noise variance parameter
## parameters$beta      contains a matrix where each column contains the MCMC sample for
##                      each regression coefficient

## compute forecast matrix
forecast <- ARforecast(
    data = Ytrain,
    sample = parameters,
    horizon = 24 ## 1 day forecast
) 

plotForecast(Ytest, Ytrain, forecast, "AR10 model - pm2p5 1 Day Forecast")

## what if we simulate one week??
startDate <- "2020-09-01"
endDate   <- "2020-10-27" ## move endDate one week before
fd        <- 7 ## one week simulation

## train dataset
Ytrain1W <- data$pm2p5SPS[data$created_at > parse_datetime(startDate) &
                          data$created_at < parse_datetime(endDate) &
                          data$pot_id == interestingPot]
Ytrain1W <- Ytrain1W[!is.na(Ytrain1W)] ## drop NaN

## test dataset
Ytest1W <- data$pm2p5SPS[data$created_at >= parse_datetime(endDate) &
                         data$created_at < parse_datetime(as.character(as.Date(endDate) + fd)) &
                         data$pot_id == interestingPot]
Ytest1W <- Ytest1W[!is.na(Ytest1W)] ## drop NaN

## let's produce a new MCMC sample using new data...
AR10sample1W <- ARfit(ARmodel, Ytrain1W, 10)

# extract parameters from MCMC sample
parameters <- ARextract(AR10sample1W)

## compute forecast distribution for 1 week
forecast1W <- ARforecast(
    data    = Ytrain1W,
    sample  = parameters,
    horizon = 24 * 7 ## 7 day forecast
) 

plotForecast(Ytest1W, Ytrain1W, forecast1W, "AR10 model - pm2p5 7 Day Forecast")

## up to now we have considered long term forecasts considering information
## available at the moment the forecast is requested. Namely given information up to
## time t, we considered the distribution of Y_{t+i}, for i between 1 and h.
## If h is large, the forecast quality is really poor

## what if we simulate the arrival of new data?? Updating our forecast on the base
## of observed data??

liveForecast <- ARLiveForecast(
    trainData = Ytrain1W,
    testData  = Ytest1W,
    sample    = parameters,
    horizon   = 8
)

plotForecast(Ytest1W, Ytrain1W, liveForecast, "AR10 model - pm2p5 Forecast with hourly update")

## increase the step to see how this model gets worse and worse... meaning that it is
## not able to produce long terms predictions

## compute forecasting from 1 up to 12 hour time horizon (takes time...)
movie <- ARforecastRange(Ytrain1W, Ytest1W, parameters, c(1,12))

## save single frames...
for (frame in ls(movie)) {
    title <- sprintf("AR(7) model - pm2p5 Forecast. Time horizon: %s hours", frame)
    fileTitle <- sprintf("frame%s", frame)
    plotForecast(Ytest1W, Ytrain1W, movie[[frame]], 2, title, png = TRUE, fileTitle = fileTitle)    
}

## create animation
library(magick) ## (requires ImageMagick installed on system)
library(gtools)

mixedsort(list.files(path=sprintf("%s/frames/", getwd()), pattern = '*.png', full.names = TRUE)) %>% 
        image_read() %>%              ## reads each path file
        image_join() %>%              ## joins image
        image_animate(fps=1) %>%      ## animates
        image_write("AR7forecast.gif") ## write to current working dir

## what if we change p??
AR2sample1W <- ARfit(ARmodel, Ytrain1W, 2) ## p = 2
liveForecast <- ARLiveForecast(
    data      = Ytest1W,
    sample    = AR2sample1W,
    horizon   = 1
)
plotForecast(Ytest1W, Ytrain1W, liveForecast, "AR2 model - pm2p5 Forecast with hourly update")

AR20sample1W <- ARfit(ARmodel, Ytrain1W, 20) ## p = 20
liveForecast <- ARLiveForecast(
    data      = Ytest1W,
    sample    = AR20sample1W,
    horizon   = 1
)
plotForecast(Ytest1W, Ytrain1W, liveForecast, "AR20 model - pm2p5 Forecast with hourly update")
