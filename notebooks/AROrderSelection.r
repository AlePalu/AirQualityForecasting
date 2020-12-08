nullFile <- file("all.Rout", open = "wt")
sink(nullFile)

library(readr);

# stan libraries
library(rstan);
library(parallel) # parallel computing

# plot utilities
library(ggplot2);
library(gridExtra)
library(purrr);
library(dplyr);

# graphical options
options(repr.plot.width=50, repr.plot.height=25)
options(warn=-1)

# set global plot theme
t =  theme_minimal() +
     theme(legend.position="none", 
           axis.text.x = element_text(face="bold", size=35),
           axis.text.y = element_text(face="bold", size=35), 
           text = element_text(size=40))

theme_set(t)

data = read_csv("../data/tsData.csv", col_types=cols());
head(data)

# focus on the time series we are interested in
interestingPot = 1091
startDate      = "2020-09-01"
endDate        = "2020-11-03"

# number of days to forecast (this will be used to build the test set)
fd = 1

# train dataset
Ytrain = data$pm2p5SPS[data$created_at > parse_datetime(startDate) &
                       data$created_at < parse_datetime(endDate)   &
                       data$pot_id == interestingPot];
Ytrain = Ytrain[!is.na(Ytrain)]; # drop NaN
N = length(Ytrain);

## shift a vector of k steps, putting k trailing NaN
shift = function(data, k){
    result = sapply(1:length(data), function(x) data[x+k])
    invisible(result)
}

stanModel = "
data{
	int<lower = 0> N;       // number of obs
	int<lower = 0> K;       // number of covariates (including the intercept)
	vector[N] Y;     	// response
	matrix[N, K] X;		// covariates

	// we pass also the parameters for the priors
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

ARmodel = stan_model(model_code = stanModel) 

## ARmodel is a compiled stan model
## data is a vector of real values
## p is an integer indicating the order of autoregression
fitAR = function(ARmodel, data, p){
    ## create dataset for AR(p)
    d = data.frame(intercept = rep(1, length(data)))
    for (t in 1:p){
        columnName = sprintf("t-%d", t)
        d[,columnName] = shift(data, t)
    }
    ## last p rows have NaN due to shifting, not consider them
    d = d[0:(nrow(d)-p),]

    stanData = list(N        = nrow(d),
                    K        = p+1,
                    Y        = data[1:nrow(d)],
                    X        = d,
                     ## prior parameters
                    scale_s2 = 10)
    
    ## init regression coefficient parameter to their MLE
    X = as.matrix(d)[,2:ncol(d)]
    linReg = lm(data[1:nrow(d)] ~ X)

    stanData$mean_beta = summary(linReg)$coefficients[,1]
    stanData$var_beta  = (summary(linReg)$coefficients[,2])^2

    ## data are now ready to be supplied to Stan
    ARsample = sampling(ARmodel,
                        data = stanData, chains = 2,
                        iter = 3000, warmup = 1000,
                        show_messages = FALSE)

    invisible(ARsample)
}

## giusto per vedere se funziona, confrontiamo col plot sul notebook dove abbiamo
## fittato un AR(2)
c = fitAR(ARmodel, Ytrain, 2)

# plot posterior density of parameters togheter with 95% credible intervals
plotPost = function(sample, vars, cI = FALSE){
    # extract samples to plot
    plotData = rstan::extract(sample, vars) %>% 
               as_tibble() %>% 
               map_df(as_data_frame, .id = 'param')
    
    plots = list()
    for(var in vars){
        data = plotData[plotData$param == var,]
        # plot density
        p = ggplot(data, aes(value)) + 
                    geom_density(fill = "lightblue", alpha = 0.4) + 
                    labs(title = var)
        if(cI){
            qu = quantile(data$value, prob = c(0.05, 0.95))
            p = p + geom_vline(xintercept = qu, lty = 2, color = "blue", size = 2)
        }
        plots <- c(plots, list(p))
        # plot traceplot
        p = ggplot(data, aes(x = 1:length(data$value), y = value)) + 
                    geom_line(color = "grey") + 
                    labs(x = "iterations", y = "trace")
        plots <- c(plots, list(p))
    }
    grid.arrange(grobs = plots, nrow = length(vars), ncol = 2)
}

## le posterior conincidono con quelle sul notebook :)
plotPost(c, c("beta[1]", "beta[2]", "beta[3]"))

WAIC <- function(fit, param){  
  llik   <- rstan::extract(fit, param)[[1]]
  p_WAIC <- sum(apply(llik, 2, var))
  lppd   <- sum(apply(llik, 2, function(x) log(mean(exp(x)))))
  WAIC   <- - 2 * lppd + 2 * p_WAIC
  return(WAIC)  
}

## compute WAIC for a grid of p values
selectAROrder = function(minP, maxP, data, stanModel){
    ## compile model for stan
    ARmodel = stan_model(model_code = stanModel) 

    resultList = vector("list", maxP - minP + 1)
    for(i in 1: (maxP-minP+1)){
        print(sprintf("AR(%d)", i))
        ## get a sample from the AR of order p
        fit = fitAR(ARmodel, data, i)
        ## store WAIC of this model
        resultList[i] = WAIC(fit, "log_lik")
    }

    invisible(resultList)
}

## marti non runnare questo da 1 a 25, ci mette davvero tanto!!
l = selectAROrder(1, 25, Ytrain, stanModel)

# plot WAIC
x11()
plot(x = 1:length(l), l, type="l")
