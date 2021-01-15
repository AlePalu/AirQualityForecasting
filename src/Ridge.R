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
source("ARXUtils.R")
source("PlotUtils.R")

data <- read_csv("../data/tsData.csv", col_types = cols())
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

ARmodelRidge <- stan_model(file = "modelFiles/ARX_Ridge.stan")

regressors      <- new.env(hash = TRUE) ## create hashmap in R
regressors$temp <- Ytrain1W$temperature_sht
regressors$hum  <- Ytrain1W$humidity_sht
regressors$wind <- Ytrain1W$wind
regressors$rain <- Ytrain1W$rain

pm2p5 <- Ytrain1W$pm2p5SPS ## response to predict

p <- 7 ## order of autoregression

## design matrix
d <- dataPrepare(pm2p5, regressors, p)
head(d)

stanData <- list(
    N = nrow(d),
    K = p + (length(ls(regressors)) * (p)) + 1,
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
load("../cestino/ARX7RidgeNo0.dat")

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
    theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                     face = "bold",
                                     size = 10)) +
    theme(legend.position = "null") +
    xlab("") + 
    ylab("") +
    scale_x_discrete(limits=rev(mixedsort(colnames(d))))

## movie of simulation
regressors      <- new.env(hash = TRUE) ## create hashmap in R
regressors$temp <- list(Ytrain1W$temperature_sht, p)
regressors$hum  <- list(Ytrain1W$humidity_sht, p)
regressors$wind <- list(Ytrain1W$wind, p)
regressors$rain <- list(Ytrain1W$rain, p)

test_regressors      <- new.env(hash = TRUE) ## create hashmap in R
test_regressors$temp <- list(Ytest1W$temperature_sht, p)
test_regressors$hum  <- list(Ytest1W$humidity_sht, p)
test_regressors$wind <- list(Ytest1W$wind, p)
test_regressors$rain <- list(Ytest1W$rain, p)

parameters <- ARXextract(ARX7sampleRidge, regressors, p)

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
    title <- sprintf("ARX(7) model - ridge regularization. pm2p5 Forecast. Time horizon: %s hours", frame)
    fileTitle <- sprintf("frame%s", frame)
    plotForecast(Ytest1W$pm2p5SPS, Ytrain1W$pm2p5SPS, movie[[frame]], p, title, png = TRUE, fileTitle = fileTitle) 
}

## create animation
library(magick) ## (requires ImageMagick installed on system)
library(gtools)

mixedsort(list.files(path=sprintf("%s/frames/", getwd()), pattern = '*.png', full.names = TRUE)) %>% 
    image_read() %>%               ## reads each path file
    image_join() %>%               ## joins image
    image_animate(fps=1) %>%       ## animates
    image_write("ARX7Ridgeforecast.gif") ## write to current working dir
