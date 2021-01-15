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

source("DataProcessUtils.R")
source("PlotUtils.R")

data <- read_csv("../data/tsData.csv", col_types = cols())
head(data)

ARmodelLasso <- stan_model(file = "modelFiles/ARX_Lasso.stan")

interestingPot <- 1091
startDate      <- "2020-09-01"
endDate        <- "2020-10-27"
fd             <- 7 ## one week simulation

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

p <- 10 ## order of autoregression

d <- dataPrepare(pm2p5, input_data_all, p)

stanData <- list(
    N = nrow(d),
    K = p + (length(ls(input_data_all)) * (p+1)) + 1,
    Y = pm2p5[1:nrow(d)],
    X = d,
    scale_s2 = 2
    )

## simulation may take a long time!!
ARsampleLasso <- sampling(ARmodelLasso,
                          data = stanData, chains = 1,
                          iter = 30000, warmup = 5000,
                          thin = 5,
                          control = list(max_treedepth = 20, adapt_delta = 0.95),
                          show_messages = FALSE
                          )

save(ARsampleLasso, file = "AR10Lasso.dat")

## load data without running stan instead!!
load("AR10Lasso.dat")

## inspect lambda a posteriori
plotPosteriorDensity(ARsampleLasso, c("lambda2"))

## variable selection

## extract parameter sample
par  <- rstan::extract(ARsampleLasso, pars = c("beta"), permuted = TRUE)
beta <- as.matrix(par$beta)

## compute posterior credible intervals for beta vector
alpha <- 0.025 ## larger alphas will select more variables
CI_beta <- apply(beta, 2, quantile, c(alpha, 1 - alhpa))

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

x11()
ggplot(dat) +
    geom_boxplot(aes(x = ind, y = values, fill = ind)) +
    geom_hline(aes(yintercept = 0), col = 2, lty = 2) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "null") +
    xlab("") + 
    ylab("")
