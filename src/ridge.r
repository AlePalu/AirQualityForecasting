library(readr);

## stan libraries
library(rstan);
library(parallel) # parallel computing

## JAGS libraries
library(rjags)
library(coda)

## plot utilities
library(ggplot2);
library(gridExtra)
library(purrr);
library(dplyr);

options(warns = -1)

source("DataProcessUtils.r")
source("PlotUtils.r")

data <- read_csv("../data/tsData.csv", col_types = cols())
head(data)

stanModelRidge <- "
data{
	int<lower = 0> N;       // number of obs
	int<lower = 0> K;       // number of covariates (including the intercept)
	real<lower = 0> Y[N];   // response vector
	matrix[N, K] X;		// covariates
	real<lower = 0> scale_s2;
}

parameters{
	vector[K] beta;
	real<lower = 0> lambda;
	real<lower = 0> sigma2;
}

transformed parameters{
	vector[N] mu;
  	mu = X * beta;
}

model{
        // Likelihood:
        for (i in 1:N) {
             Y[i] ~ normal(mu[i], sigma2);
        }

	// Prior:
	// beta
        for (j in 1:K) {
             beta[j] ~ normal(0.0, 1.0 / lambda);
        }

        lambda ~ exponential(0.1);
 
	sigma2 ~ inv_gamma(2., scale_s2);
}
"

ARmodelRidge <- stan_model(model_code = stanModelRidge)

interestingPot <- 1091
startDate      <- "2020-09-01"
endDate        <- "2020-10-27" 
fd             <- 7            ## one week simulation

data_potid <- data[data$created_at > parse_datetime(startDate) &
                   data$created_at < parse_datetime(endDate) &
                   data$pot_id == interestingPot, ]
data_potid <- data_potid[!is.na(data_potid$pm2p5SPS), ] ## remove NaN
head(data_potid)

input_data_all      <- new.env(hash = TRUE) ## create hashmap in R
input_data_all$temp <- data_potid$temperature_sht
input_data_all$hum  <- data_potid$humidity_sht
input_data_all$wind <- data_potid$wind
input_data_all$rain <- data_potid$rain

pm2p5 <- data_potid$pm2p5SPS ## response to predict

p <- 7 ## order of autoregression

d <- dataPrepare(pm2p5, input_data_all, p)
head(d)

stanData <- list(
    N = nrow(d),
    K = p + (length(ls(input_data_all)) * (p)) + 1,
    Y = pm2p5[1:nrow(d)],
    X = d,
    scale_s2 = 2
    )

## simulation may take a long time!!
ARX7sampleRidge <- sampling(ARmodelRidge,
                            data = stanData, chains = 1,
                            iter = 20000, warmup = 5000,
                            thin = 5,
                            control = list(max_treedepth = 20, adapt_delta = 0.95),
                            show_messages = FALSE
                            )

save(ARX7sampleRidge, file = "ARX7RidgeNo0.dat")

## load data without running stan instead!!
load("ARX7Ridge.dat")

## inspect lambda a posteriori
plotPosteriorDensity(ARX7sampleRidge, c("lambda"))

## variable selection

## extract parameter sample
par  <- rstan::extract(ARX7sampleRidge, pars = c("beta"), permuted = TRUE)
beta <- as.matrix(par$beta)

## compute posterior credible intervals for beta vector
alpha <- 0.1 ## larger alphas will select more variables
CI_beta <- apply(beta, 2, quantile, c(alpha, 1 - alpha))

## if the credibility interval does not contain 0, the variable is meaningfull
meaningfull = NULL
for (l in 1:dim(beta)[2]) {
    if (!(CI_beta[1, l] < 0 && CI_beta[2, l] > 0)) {
        meaningfull <- c(meaningfull, colnames(d)[l])
    }
}

meaningfull

## boxplot of results
dat <- stack(as.data.frame(beta))
dat$ind <- rep(colnames(d), each = dim(beta)[1])

library(gtools)

x11()
ggplot(dat) +
    geom_boxplot(aes(x = ind, y = values, fill = ind)) +
    geom_hline(aes(yintercept = 0), col = 2, lty = 2) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "null") +
    xlab("") + 
    ylab("") +
    scale_x_discrete(limits=rev(mixedsort(colnames(d))))
