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
