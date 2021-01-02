## ARX(p)

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
source("ARXUtils.R")
source("PlotUtils.r")

## import data
data = read_csv("../data/tsData.csv", col_types=cols());
head(data)

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

## autoregressive stan model of any order p
stanModel = "
data{
	int<lower = 0> N;       // number of obs
	int<lower = 0> K;       // number of covariates (including the intercept)
	vector[N] Y;     	// response
	matrix[N, K] X;		// covariates

	// prior parameters
	vector[K] mean_beta;	
	vector[K] var_beta;
	real<lower = 0> scale_s2;
}

parameters{
	real<lower = 0> sigma2;
	vector[K] beta;
}

transformed parameters{
	vector[N] mu;
  	mu = X * beta;
}

model{
	// Prior:
	sigma2 ~ inv_gamma(2., scale_s2);
	for(k in 1:K) {
		beta[k] ~ normal(mean_beta[k], pow(var_beta[k], 0.5));	
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
ARXmodel <- stan_model(model_code = stanModel)

## to allow for different orders of covariates the following convention is used
## each element passed as regressor ti a fit function is a list maden by two elements:
## c(data, order of autoregression for this data)
## for example (Ytrain1W$wind, 3) says that output will depend on wind by only its firts
## 3 lagged values: wind[t-1], wind[t-2], wind[t-3]

p <- 10                    ## order of autoregression

## external regressors
regressors      <- new.env(hash = TRUE) ## create hashmap in R
regressors$temp <- list(Ytrain1W$temperature_sht, p)
regressors$hum  <- list(Ytrain1W$humidity_sht, p)
regressors$wind <- list(Ytrain1W$wind, p)
regressors$rain <- list(Ytrain1W$rain, p)

## get sample from model
pm2p5 <- Ytrain1W$pm2p5SPS ## response to predict

ARX10sample <- ARXfit(ARXmodel,   ## compiled stan model
                      pm2p5,      ## response
                      p,          ## autoregression order
                      regressors) ## regressors

## save the sampling... caution: it is a big file!!
save(ARX10sample, file = "ARX10data.dat")

## do not run stan, load data directly instead!!
load("ARX10data.dat")

# extract parameters from MCMC sample
parameters <- ARXextract(ARX10sample, regressors)

## compute forecast matrix
forecast = ARXforecast(data     = pm2p5,
                       reg_part = regressors,
                       sample   = parameters,
                       horizon  = 24*7) ## 7 days forecast

plotForecast(Ytest1W$pm2p5SPS, pm2p5, forecast, "ARX(10) model - pm2p5 1 Day Forecast using all covariates")

## up to now we have considered long term forecasts considering information
## available at the moment the forecast is requested. Namely given information up to
## time t, we considered the distribution of Y_{t+i}, for i between 1 and h.
## If h is large, the forecast quality is really poor

## what if we simulate the arrival of new data?? Updating our forecast on the base
## of observed data??

test_regressors      <- new.env(hash = TRUE) ## create hashmap in R
test_regressors$temp <- list(Ytest1W$temperature_sht, p)
test_regressors$hum  <- list(Ytest1W$humidity_sht, p)
test_regressors$wind <- list(Ytest1W$wind, p)
test_regressors$rain <- list(Ytest1W$rain, p)

liveForecast <- ARXLiveForecast(
    Ytrain    = Ytrain1W$pm2p5SPS,
    Xtrain    = regressors,
    Ytest     = Ytest1W$pm2p5SPS,
    Xtest     = test_regressors,
    sample    = parameters,
    horizon   = 1
)

plotForecast(Ytest1W$pm2p5SPS, Ytrain1W$pm2p5SPS, liveForecast, "AR10 model - pm2p5 Forecast with hourly update")

## compute forecasting from 1 up to 12 hour time horizon (takes A LOT of time...)
movie <- ARXforecastRange(
    Ytrain    = Ytrain1W$pm2p5SPS,
    Xtrain    = regressors,
    Ytest     = Ytest1W$pm2p5SPS,
    Xtest     = test_regressors,
    sample    = parameters,
    range     = c(1,12)  ## from 1 hour to 12 hours forecast
)

## save single frames...
for (frame in ls(movie)) {
    title <- sprintf("ARX10 model - pm2p5 Forecast. Time horizon: %s hours", frame)
    fileTitle <- sprintf("frame%s", frame)
    plotForecast(Ytest1W$pm2p5SPS, Ytrain1W$pm2p5SPS, movie[[frame]], title, png = TRUE, fileTitle = fileTitle) 
}

## create animation
library(magick) ## (requires ImageMagick installed on system)
library(gtools)

mixedsort(list.files(path=sprintf("%s/frames/", getwd()), pattern = '*.png', full.names = TRUE)) %>% 
    image_read() %>%               ## reads each path file
    image_join() %>%               ## joins image
    image_animate(fps=1) %>%       ## animates
    image_write("ARXforecast.gif") ## write to current working dir

save(movie, file = "ARX10forecastSimulation.dat")
