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
source("PerformanceIndex.R")
source("DataProcessUtils.r")
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

autoregressiveOrder <- 2    ## order of autoregression
timeHorizon <- 4            ## time horizon used to evaluate the model performance

k <- 4                      ## number of covariates
modelMask = expand.grid(rep(list(0:1),k))

colnames(modelMask) <- c("temp", "hum", "rain", "wind")
modelMask

## covariates (name of environment must be the same as column names of modelMask matrix)
regressors      <- new.env(hash = TRUE) ## create hashmap in R
regressors$temp <- list(Ytrain1W$temperature_sht, 2)
regressors$hum  <- list(Ytrain1W$humidity_sht, 1)
regressors$rain <- list(Ytrain1W$rain, 3)
regressors$wind <- list(Ytrain1W$wind, 3)

test_regressors      <- new.env(hash = TRUE) ## create hashmap in R
test_regressors$temp <- list(Ytest1W$temperature_sht, 2)
test_regressors$hum  <- list(Ytest1W$humidity_sht, 1)
test_regressors$rain <- list(Ytest1W$rain, 3)
test_regressors$wind <- list(Ytest1W$wind, 3)

pm2p5 <- Ytrain1W$pm2p5SPS ## response to predict

## performance indexes
MSEvector  = c()
WAICvector = c()
BICvector  = c()
for (m in seq(2, dim(modelMask)[1])){
    ## create covariate hashmap for this model
    Mregressors      <- new.env(hash = TRUE)
    Mtest            <- new.env(hash = TRUE)
    pMax <- autoregressiveOrder ## maximum autoregressive order in the model
    for(var in seq(1,dim(modelMask)[2])){
        if(modelMask[m, var] == 1){
            Mregressors[[colnames(modelMask)[var]]] <- regressors[[colnames(modelMask)[var]]]
            Mtest[[colnames(modelMask)[var]]]       <- test_regressors[[colnames(modelMask)[var]]]

            if(Mregressors[[colnames(modelMask)[var]]][[2]] > pMax){
                pMax = Mregressors[[colnames(modelMask)[var]]][[2]]
            }
        }
    }
    
    ModelSample <- ARXfit(ARXmodel,             ## compiled stan model
                          pm2p5,                ## response
                          autoregressiveOrder,  ## autoregression order
                          Mregressors)          ## regressors

    ## store BIC and WAIC index, they require only the sample
    BICvector  = c(BICvector, BIC(ModelSample, pm2p5[1:(length(pm2p5)-pMax)]))
    WAICvector = c(WAICvector, WAIC(ModelSample))
    
    ## compute forecast matrix

    ## extract parameters from sample
    parameters <- ARXextract(ModelSample, Mregressors, autoregressiveOrder)
    
    liveForecast <- ARXLiveForecast(
        Ytrain    = Ytrain1W$pm2p5SPS,
        Xtrain    = Mregressors,
        Ytest     = Ytest1W$pm2p5SPS,
        Xtest     = Mtest,
        sample    = parameters,
        horizon   = timeHorizon
    )

    ## store predictive MSE
    MSEvector  = c(MSEvector, MSE(liveForecast, Ytest1W$pm2p5SPS))
    
    ## save forecast plot
    title <- sprintf("ARX(%d", autoregressiveOrder)
    for(var in ls(Mregressors)){
        title <- sprintf("%s,%d", title, Mregressors[[var]][[2]])
    }
    title <- sprintf("%s) model - pm2p5 Forecast. Time horizon: %d hours. Regressors:", title, timeHorizon)
    for(var in ls(Mregressors)){
        title <- sprintf("%s %s ", title, var)
    }
    fileTitle <- paste(c("1tempARX", modelMask[m,]), collapse = "")
    plotForecast(Ytest1W$pm2p5SPS, Ytrain1W$pm2p5SPS, liveForecast, autoregressiveOrder,
                 title, png = TRUE, fileTitle = fileTitle) 
}

## plot results
MSEvectorDF = data.frame(MSE = MSEvector)

x11()
ggplot(MSEvectorDF) + geom_line(aes(x = (1:length(MSE)), y = MSE)) +
    geom_line(aes(x = (1:length(MSE)), y = MSE)) +
    geom_point(aes(x = (1:length(MSE)), y = MSE), size = 2) + 
    labs(y = "MSE", x = "time horizon") + ggtitle("MSE as function of time horizon")

library(magick) ## (requires ImageMagick installed on system)
library(gtools)

mixedsort(list.files(path=sprintf("%s/frames/", getwd()), pattern = '*.png', full.names = TRUE)) %>% 
    image_read() %>%               ## reads each path file
    image_join() %>%               ## joins image
    image_animate(fps=1) %>%       ## animates
    image_write("ARXforecastTemperature.gif") ## write to current working dir
